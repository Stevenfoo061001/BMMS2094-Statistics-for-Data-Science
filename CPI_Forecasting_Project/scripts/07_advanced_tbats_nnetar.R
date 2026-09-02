# 07_advanced_tbats_nnetar.R - exploratory advanced forecasts outside the core syllabus.
# Run after 01_data_prep.R. This script does not replace the four core models.

source("scripts/00_setup.R")
if (!file.exists("data/training_data.rds")) source("scripts/01_data_prep.R")

train <- readRDS("data/training_data.rds")
test <- readRDS("data/testing_data.rds")
selection_train <- train %>% slice_head(n = nrow(train) - validation_h)
validation <- train %>% slice_tail(n = validation_h)

train_ts <- ts(train$index, start = c(train$year[1], train$month_number[1]), frequency = 12)
selection_train_ts <- ts(selection_train$index,
                         start = c(selection_train$year[1], selection_train$month_number[1]),
                         frequency = 12)
validation_ts <- ts(validation$index,
                    start = c(validation$year[1], validation$month_number[1]),
                    frequency = 12)
test_ts <- ts(test$index, start = c(test$year[1], test$month_number[1]), frequency = 12)
out <- "output/plots/Member_E_advanced"

# Each candidate returns a fitted model and forecast. NNETAR has a fixed seed
# so its neural-network forecast is reproducible when the script is re-run.
candidates <- list(
  tbats = function(series, horizon) {
    model <- tbats(series, seasonal.periods = 12, use.box.cox = NULL)
    list(model = model, forecast = forecast(model, h = horizon))
  },
  nnetar = function(series, horizon) {
    set.seed(2094)
    model <- nnetar(series, p = 3, P = 1, size = 2, repeats = 20)
    list(model = model, forecast = forecast(model, h = horizon))
  }
)

candidate_labels <- c(
  tbats = "TBATS (annual seasonality = 12)",
  nnetar = "NNETAR(3,1)[12] (size = 2, repeats = 20)"
)

evaluate_candidate <- function(model_key, candidate_function) {
  tryCatch({
    fitted_candidate <- candidate_function(selection_train_ts, validation_h)
    model <- fitted_candidate$model
    validation_forecast <- fitted_candidate$forecast
    bind_cols(
      tibble(model_key = model_key, model = candidate_labels[[model_key]]),
      metrics(validation$index, as.numeric(validation_forecast$mean), selection_train$index) %>%
        rename_with(~ paste0("Validation_", .x)),
      residual_diagnostics(model, fitdf = 0),
      overfitting_diagnostic(validation_forecast, validation_ts) %>%
        rename_with(~ paste0("Validation_", .x))
    )
  }, error = function(error) {
    tibble(
      model_key = model_key, model = candidate_labels[[model_key]],
      Validation_MAE = NA_real_, Validation_RMSE = NA_real_,
      Validation_MAPE = NA_real_, Validation_MASE = NA_real_,
      Ljung_Box_lag = NA_integer_, Ljung_Box_p_value = NA_real_,
      Ljung_Box_acceptable = "Unavailable", Ljung_Box_decision = "Unavailable",
      Residual_ACF_spike_count = NA_integer_, Residual_ACF_spike_lags = "Unavailable",
      Residual_ACF_has_consecutive_spikes = "Unavailable",
      Residual_ACF_pattern = "Unavailable", Residual_ACF_acceptable = "Unavailable",
      Residuals_acceptable = "No", Diagnostic_note = "Candidate model failed to fit.",
      Validation_Training_RMSE = NA_real_, Validation_Test_RMSE = NA_real_,
      Validation_RMSE_ratio = NA_real_, Validation_Training_MAPE = NA_real_,
      Validation_Test_MAPE = NA_real_, Validation_Overfitting_acceptable = "Unavailable",
      Validation_Overfitting_assessment = "Unavailable", Error_Message = conditionMessage(error)
    )
  })
}

candidate_results <- bind_rows(Map(evaluate_candidate, names(candidates), candidates)) %>%
  mutate(Validation_eligible = if_else(
    Residuals_acceptable == "Yes" & Validation_Overfitting_acceptable == "Yes", "Yes", "No"
  )) %>%
  arrange(desc(Validation_eligible == "Yes"), Validation_RMSE)
write_csv(candidate_results, "output/member_E_candidate_results.csv")
print(candidate_results)

if (all(is.na(candidate_results$Validation_RMSE))) {
  stop("Both advanced candidates failed to fit. See output/member_E_candidate_results.csv.")
}

acceptable_results <- candidate_results %>% filter(Validation_eligible == "Yes")
if (nrow(acceptable_results) > 0) {
  recommended_key <- acceptable_results$model_key[which.min(acceptable_results$Validation_RMSE)]
  recommendation_note <- "Recommended: lowest validation-RMSE candidate passing residual and validation overfitting checks."
} else {
  recommended_key <- candidate_results$model_key[which.min(candidate_results$Validation_RMSE)]
  recommendation_note <- "Fallback: no advanced candidate passed all checks; selected the lowest validation-RMSE candidate."
}

recommended_candidate <- candidates[[recommended_key]](train_ts, h)
recommended_model <- recommended_candidate$model
recommended_forecast <- recommended_candidate$forecast
recommended_name <- candidate_labels[[recommended_key]]

cat("\n", recommendation_note, "\nSelected advanced candidate: ", recommended_name, "\n", sep = "")

save_plot(
  autoplot(train_ts) +
    labs(title = "Advanced Models: Overall CPI Training Series", x = "Year", y = "CPI index") +
    theme_minimal(),
  file.path(out, "Member_E_01_autoplot.png")
)

forecast_plot <- autoplot(recommended_forecast) +
  autolayer(test_ts, series = "Actual test CPI") +
  labs(title = paste("Advanced model:", recommended_name, "18-Month Holdout Forecast"),
       x = "Year", y = "CPI index") +
  theme_minimal()
save_plot(forecast_plot, file.path(out, "Member_E_02_forecast.png"))

png(file.path(out, "Member_E_03_residuals.png"), width = 3300, height = 2400, res = 300)
checkresiduals(recommended_model, lag = 24)
dev.off()

accuracy_E <- metrics(test$index, as.numeric(recommended_forecast$mean), train$index) %>%
  bind_cols(
    residual_diagnostics(recommended_model, fitdf = 0),
    overfitting_diagnostic(recommended_forecast, test_ts)
  ) %>%
  mutate(
    member = "Advanced exploratory",
    model_family = "TBATS / neural-network autoregression",
    selected_variant = recommended_name,
    model_specification = recommended_name,
    model = model_specification
  ) %>%
  select(member, model_family, selected_variant, model_specification, model,
         MAE, RMSE, MAPE, MASE, everything())
write_csv(accuracy_E, "output/member_E_accuracy.csv")
print(accuracy_E)
