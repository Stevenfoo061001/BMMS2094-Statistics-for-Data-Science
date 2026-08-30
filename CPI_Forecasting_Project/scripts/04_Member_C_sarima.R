# 04_Member_C_sarima.R - select the best SARIMA specification.
# Run after 01_data_prep.R.

source("scripts/00_setup.R")
if (!file.exists("data/training_data.rds")) source("scripts/01_data_prep.R")

train <- readRDS("data/training_data.rds")
test <- readRDS("data/testing_data.rds")
selection_train <- train %>% slice_head(n = nrow(train) - validation_h)
validation <- train %>% slice_tail(n = validation_h)
train_ts <- ts(train$index, start = c(2010, 1), frequency = 12)
selection_train_ts <- ts(selection_train$index, start = c(2010, 1), frequency = 12)
test_ts <- ts(test$index, start = c(2025, 1), frequency = 12)
out <- "output/plots/Member_C_sarima"

# Standard Member C plot sequence, matching the familiar naming convention
# used by the archived SARIMA work and the other members' output folders.
first_difference <- diff(train_ts)
save_plot(
  autoplot(train_ts) +
    labs(title = "Steven: Overall CPI Training Series", x = "Year", y = "CPI index") +
    theme_minimal(),
  file.path(out, "Member_C_01_autoplot.png")
)

png(file.path(out, "Member_C_02_acf.png"), width = 3300, height = 1500, res = 300)
Acf(first_difference, lag.max = 36, main = "Steven: First-Differenced CPI ACF")
dev.off()

png(file.path(out, "Member_C_03_pacf.png"), width = 3300, height = 1500, res = 300)
Pacf(first_difference, lag.max = 36, main = "Steven: First-Differenced CPI PACF")
dev.off()

arima_metadata <- function(model, model_key, selected_variant) {
  selected_order <- forecast::arimaorder(model)
  get_order <- function(name, default = 0L) {
    if (name %in% names(selected_order)) as.integer(selected_order[[name]]) else default
  }
  p <- get_order("p"); d <- get_order("d"); q <- get_order("q")
  P <- get_order("P"); D <- get_order("D"); Q <- get_order("Q")
  m <- get_order("m", seasonal_period)
  coefficient_names <- names(coef(model))
  includes_drift <- "drift" %in% coefficient_names
  includes_mean <- any(c("intercept", "mean") %in% coefficient_names)
  suffix <- if (includes_drift) " with drift" else if (includes_mean) " with mean/intercept" else ""

  tibble(
    model_key = model_key,
    selected_variant = selected_variant,
    model_specification = paste0("ARIMA(", p, ",", d, ",", q, ")(", P, ",", D, ",", Q, ")[", m, "]", suffix),
    p = p, d = d, q = q, P = P, D = D, Q = Q,
    seasonal_period = m,
    includes_drift = if_else(includes_drift, "Yes", "No"),
    includes_mean = if_else(includes_mean, "Yes", "No")
  )
}

# Fit a model safely. A failed or non-convergent candidate is recorded as NA.
evaluate_candidate <- function(model_key, selected_variant, model_function) {
  tryCatch({
    model <- model_function(selection_train_ts)
    validation_forecast <- forecast::forecast(model, h = validation_h)
    forecast_values <- as.numeric(validation_forecast$mean)
    model_aicc <- tryCatch(as.numeric(model$aicc), error = function(error) NA_real_)
    bind_cols(
      tibble(AICc = model_aicc),
      arima_metadata(model, model_key, selected_variant),
      residual_diagnostics(model, fitdf = length(coef(model))),
      overfitting_diagnostic(validation_forecast, validation) %>%
        rename_with(~ paste0("Validation_", .x)),
      metrics(validation$index, forecast_values, selection_train$index) %>%
        rename_with(~ paste0("Validation_", .x))
    )
  }, error = function(error) {
    tibble(model_key = model_key, selected_variant = selected_variant,
           model_specification = NA_character_, p = NA_integer_, d = NA_integer_, q = NA_integer_,
           P = NA_integer_, D = NA_integer_, Q = NA_integer_, seasonal_period = NA_integer_,
           includes_drift = NA_character_, includes_mean = NA_character_, AICc = NA_real_,
           Ljung_Box_lag = NA_integer_, Ljung_Box_p_value = NA_real_, Ljung_Box_acceptable = "Unavailable",
           Ljung_Box_decision = "Unavailable", Residual_ACF_spike_count = NA_integer_,
           Residual_ACF_spike_lags = "Unavailable", Residual_ACF_has_consecutive_spikes = "Unavailable",
           Residual_ACF_pattern = "Unavailable", Residual_ACF_acceptable = "Unavailable",
           Residuals_acceptable = "No", Diagnostic_note = "Candidate model failed to fit.",
           Validation_Training_RMSE = NA_real_, Validation_Test_RMSE = NA_real_,
           Validation_RMSE_ratio = NA_real_, Validation_Training_MAPE = NA_real_,
           Validation_Test_MAPE = NA_real_, Validation_Overfitting_acceptable = "Unavailable",
           Validation_Overfitting_assessment = "Unavailable",
           Validation_MAE = NA_real_, Validation_RMSE = NA_real_, Validation_MAPE = NA_real_,
           Validation_MASE = NA_real_, Error_Message = conditionMessage(error))
  })
}

# Candidates are selected with internal validation, preserving the final test
# period for one unbiased evaluation after the selected model is refitted.
candidate_labels <- c(
  arima_011_no_drift = "ARIMA(0,1,1) without drift",
  arima_011_drift = "ARIMA(0,1,1) with drift",
  arima_111_no_drift = "ARIMA(1,1,1) without drift",
  arima_111_drift = "ARIMA(1,1,1) with drift",
  sarima_011_001_no_drift = "SARIMA(0,1,1)(0,0,1)[12] without drift",
  sarima_011_001_drift = "SARIMA(0,1,1)(0,0,1)[12] with drift",
  sarima_111_111 = "SARIMA(1,1,1)(1,1,1)[12]",
  sarima_011_011 = "SARIMA(0,1,1)(0,1,1)[12]",
  sarima_110_110 = "SARIMA(1,1,0)(1,1,0)[12]",
  sarima_111_110 = "SARIMA(1,1,1)(1,1,0)[12]",
  sarima_110_011 = "SARIMA(1,1,0)(0,1,1)[12]",
  auto_arima = "Automatic ARIMA selection",
  auto_arima_no_drift = "Automatic ARIMA selection without drift"
)
candidates <- list(
  arima_011_no_drift = function(series) forecast::Arima(series, order = c(0, 1, 1), include.drift = FALSE),
  arima_011_drift = function(series) forecast::Arima(series, order = c(0, 1, 1), include.drift = TRUE),
  arima_111_no_drift = function(series) forecast::Arima(series, order = c(1, 1, 1), include.drift = FALSE),
  arima_111_drift = function(series) forecast::Arima(series, order = c(1, 1, 1), include.drift = TRUE),
  sarima_011_001_no_drift = function(series) forecast::Arima(series, order = c(0, 1, 1), seasonal = list(order = c(0, 0, 1), period = 12), include.drift = FALSE),
  sarima_011_001_drift = function(series) forecast::Arima(series, order = c(0, 1, 1), seasonal = list(order = c(0, 0, 1), period = 12), include.drift = TRUE),
  sarima_111_111 = function(series) forecast::Arima(series, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 12)),
  sarima_011_011 = function(series) forecast::Arima(series, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),
  sarima_110_110 = function(series) forecast::Arima(series, order = c(1, 1, 0), seasonal = list(order = c(1, 1, 0), period = 12)),
  sarima_111_110 = function(series) forecast::Arima(series, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 0), period = 12)),
  sarima_110_011 = function(series) forecast::Arima(series, order = c(1, 1, 0), seasonal = list(order = c(0, 1, 1), period = 12)),
  auto_arima = function(series) forecast::auto.arima(series, seasonal = TRUE, stepwise = FALSE, approximation = FALSE),
  auto_arima_no_drift = function(series) forecast::auto.arima(series, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, allowdrift = FALSE)
)

candidate_results <- bind_rows(Map(evaluate_candidate, names(candidates), unname(candidate_labels), candidates)) %>%
  mutate(
    Validation_eligible = if_else(
      Residuals_acceptable == "Yes" & Validation_Overfitting_acceptable == "Yes", "Yes", "No"
    )
  ) %>%
  arrange(desc(Validation_eligible == "Yes"), Validation_RMSE)

write_csv(candidate_results, "output/member_C_candidate_results.csv")
print(candidate_results)

if (all(is.na(candidate_results$Validation_RMSE))) {
  stop(
    "All SARIMA candidates failed to fit. First error: ",
    candidate_results$Error_Message[1],
    ". See output/member_C_candidate_results.csv for every error."
  )
}

# Prefer a model with acceptable residuals. If none pass, select the smallest
# Ljung-Box issue first, then report that further refinement is required.
acceptable_results <- candidate_results %>% filter(Validation_eligible == "Yes")
if (nrow(acceptable_results) > 0) {
  recommended_key <- acceptable_results$model_key[which.min(acceptable_results$Validation_RMSE)]
  recommendation_note <- "Recommended: lowest-RMSE candidate passing Ljung-Box, residual ACF, and validation overfitting checks."
} else {
  recommended_key <- candidate_results$model_key[which.min(candidate_results$Validation_RMSE)]
  recommendation_note <- "Fallback: no candidate passed all three validation checks; selected the lowest validation-RMSE candidate and marked it as ineligible for final selection."
}

recommended_name <- candidate_labels[[recommended_key]]
recommended_model <- candidates[[recommended_key]](train_ts)
recommended_forecast <- forecast::forecast(recommended_model, h = h)
selected_metadata <- arima_metadata(recommended_model, recommended_key, recommended_name)
cat("\n", recommendation_note, "\nSelected candidate:", selected_metadata$model_specification, "\n", sep = "")

# This becomes Steven's selected SARIMA result for the group comparison.
accuracy_C <- metrics(test$index, as.numeric(recommended_forecast$mean), train$index) %>%
  bind_cols(
    selected_metadata,
    residual_diagnostics(recommended_model, fitdf = length(coef(recommended_model))),
    overfitting_diagnostic(recommended_forecast, test_ts)
  ) %>%
  mutate(member = "Steven", model_family = "ARIMA/SARIMA", model = model_specification) %>%
  select(member, model_family, selected_variant, model_specification, model_key,
         p, d, q, P, D, Q, seasonal_period, includes_drift, includes_mean,
         model, MAE, RMSE, MAPE, MASE, everything())
write_csv(accuracy_C, "output/member_C_accuracy.csv")
print(accuracy_C)
write_csv(
  accuracy_C %>% select(member, model_family, selected_variant, model_specification, model_key,
                         p, d, q, P, D, Q, seasonal_period, includes_drift, includes_mean),
  "output/member_C_selected_model_specification.csv"
)

comparison_plot <- ggplot(candidate_results, aes(reorder(model_specification, Validation_RMSE), Validation_RMSE, fill = Validation_eligible)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c("Yes" = "#1b9e77", "No" = "#d95f02")) +
  labs(title = "Steven: ARIMA/SARIMA Candidate Comparison", subtitle = "Green = passed Ljung-Box, residual ACF, and validation overfitting checks", x = "ARIMA/SARIMA candidate", y = "Validation RMSE", fill = "Validation eligible") +
  theme_minimal()
save_plot(comparison_plot, file.path(out, "Member_C_06_candidate_comparison.png"), width = 12, height = 6)

forecast_plot <- autoplot(recommended_forecast) +
  autolayer(test_ts, series = "Actual test CPI") +
  labs(title = paste("Steven: Recommended", selected_metadata$model_specification, "Forecast"), x = "Year", y = "CPI index") +
  theme_minimal()
save_plot(forecast_plot, file.path(out, "Member_C_04_forecast.png"))
save_plot(forecast_plot, file.path(out, "Member_C_07_recommended_forecast.png"))

png(file.path(out, "Member_C_05_residuals.png"), width = 3300, height = 2400, res = 300)
checkresiduals(recommended_model, lag = 24)
dev.off()

png(file.path(out, "Member_C_08_recommended_residuals.png"), width = 3300, height = 2400, res = 300)
checkresiduals(recommended_model, lag = 24)
dev.off()
