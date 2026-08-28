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

# Fit a model safely. A failed or non-convergent candidate is recorded as NA.
evaluate_candidate <- function(name, model_function) {
  tryCatch({
    model <- model_function(selection_train_ts)
    forecast_values <- as.numeric(forecast(model, h = validation_h)$mean)
    bind_cols(
      tibble(model = name, AICc = model$aicc),
      ljung_box_diagnostic(model, fitdf = length(coef(model))),
      residual_acf_diagnostic(model),
      metrics(validation$index, forecast_values, selection_train$index) %>%
        rename_with(~ paste0("Validation_", .x))
    )
  }, error = function(error) {
    tibble(model = name, AICc = NA_real_, Ljung_Box_lag = NA_real_, Ljung_Box_p_value = NA_real_, Ljung_Box_acceptable = NA_character_, Residual_ACF_acceptable = NA_character_, Residual_ACF_spike_lags = NA_character_, Validation_MAE = NA_real_, Validation_RMSE = NA_real_, Validation_MAPE = NA_real_, Validation_MASE = NA_real_, Error_Message = conditionMessage(error))
  })
}

# Candidates are selected with internal validation, preserving the final test
# period for one unbiased evaluation after the selected model is refitted.
candidates <- list(
  "SARIMA(1,1,1)(1,1,1)[12]" = function(series) Arima(series, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 12)),
  "SARIMA(0,1,1)(0,1,1)[12]" = function(series) Arima(series, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),
  "SARIMA(1,1,0)(1,1,0)[12]" = function(series) Arima(series, order = c(1, 1, 0), seasonal = list(order = c(1, 1, 0), period = 12)),
  "SARIMA(1,1,1)(1,1,0)[12]" = function(series) Arima(series, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 0), period = 12)),
  "SARIMA(1,1,0)(0,1,1)[12]" = function(series) Arima(series, order = c(1, 1, 0), seasonal = list(order = c(0, 1, 1), period = 12)),
  "auto.arima selected model" = function(series) auto.arima(series, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
)

candidate_results <- bind_rows(Map(evaluate_candidate, names(candidates), candidates)) %>%
  mutate(Residuals_acceptable = if_else(
    Ljung_Box_acceptable == "Yes" & Residual_ACF_acceptable == "Yes", "Yes", "No"
  )) %>%
  arrange(desc(Residuals_acceptable == "Yes"), Validation_RMSE)

write_csv(candidate_results, "output/steven_sarima_candidate_results.csv")
print(candidate_results)

if (all(is.na(candidate_results$Validation_RMSE))) {
  stop(
    "All SARIMA candidates failed to fit. First error: ",
    candidate_results$Error_Message[1],
    ". See output/steven_sarima_candidate_results.csv for every error."
  )
}

# Prefer a model with acceptable residuals. If none pass, select the smallest
# Ljung-Box issue first, then report that further refinement is required.
acceptable_results <- candidate_results %>% filter(Residuals_acceptable == "Yes")
if (nrow(acceptable_results) > 0) {
  recommended_name <- acceptable_results$model[which.min(acceptable_results$Validation_RMSE)]
  recommendation_note <- "Recommended: lowest-RMSE candidate passing Ljung-Box and residual ACF checks."
} else {
  recommended_name <- candidate_results$model[which.min(candidate_results$Validation_RMSE)]
  recommendation_note <- "Fallback: no candidate passed both residual checks; selected the lowest validation-RMSE candidate."
}

recommended_model <- candidates[[recommended_name]](train_ts)
recommended_forecast <- forecast(recommended_model, h = h)
cat("\n", recommendation_note, "\nSelected candidate:", recommended_name, "\n", sep = "")

# This becomes Steven's selected SARIMA result for the group comparison.
accuracy_C <- metrics(test$index, as.numeric(recommended_forecast$mean), train$index) %>%
  bind_cols(
    ljung_box_diagnostic(recommended_model, fitdf = length(coef(recommended_model))),
    residual_acf_diagnostic(recommended_model)
  ) %>%
  mutate(
    Residuals_acceptable = if_else(
      Ljung_Box_acceptable == "Yes" & Residual_ACF_acceptable == "Yes", "Yes", "No"
    ),
    member = "Steven",
    model = recommended_name
  ) %>%
  select(member, model, everything())
write_csv(accuracy_C, "output/member_C_accuracy.csv")
print(accuracy_C)

comparison_plot <- ggplot(candidate_results, aes(reorder(model, Validation_RMSE), Validation_RMSE, fill = Residuals_acceptable)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c("Yes" = "#1b9e77", "No" = "#d95f02")) +
  labs(title = "Steven: SARIMA Candidate Comparison", subtitle = "Green = Ljung-Box and residual ACF checks passed", x = "SARIMA candidate", y = "Validation RMSE", fill = "Residuals acceptable") +
  theme_minimal()
save_plot(comparison_plot, file.path(out, "Member_C_06_candidate_comparison.png"), width = 12, height = 6)

save_plot(
  autoplot(recommended_forecast) + autolayer(test_ts, series = "Actual test CPI") +
    labs(title = paste("Steven: Recommended", recommended_name, "Forecast"), x = "Year", y = "CPI index") + theme_minimal(),
  file.path(out, "Member_C_07_recommended_forecast.png")
)

png(file.path(out, "Member_C_08_recommended_residuals.png"), width = 3300, height = 2400, res = 300)
checkresiduals(recommended_model, lag = 24)
dev.off()
