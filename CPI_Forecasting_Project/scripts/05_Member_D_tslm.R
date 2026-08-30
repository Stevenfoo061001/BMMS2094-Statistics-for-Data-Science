# Tan Wei Ching - select the best trend-regression candidate.

source("scripts/00_setup.R")
if (!file.exists("data/training_data.rds")) source("scripts/01_data_prep.R")

train <- readRDS("data/training_data.rds")
test <- readRDS("data/testing_data.rds")
selection_train <- train %>% slice_head(n = nrow(train) - validation_h)
validation <- train %>% slice_tail(n = validation_h)

train_ts <- ts(train$index, start = c(2010, 1), frequency = 12)
selection_train_ts <- ts(selection_train$index, start = c(2010, 1), frequency = 12)
test_ts <- ts(test$index, start = c(2025, 1), frequency = 12)
out <- "output/plots/Member_D_tslm"

# Select the regression structure with internal validation only.
candidates <- list(
  "Trend only" = function(series) tslm(series ~ trend),
  "Trend + monthly dummies" = function(series) tslm(series ~ trend + season),
  "Quadratic trend + monthly dummies" = function(series) tslm(series ~ trend + I(trend^2) + season)
)

evaluate_candidate <- function(name, candidate_function) {
  tryCatch({
    fit_model <- candidate_function(selection_train_ts)
    fit <- forecast(fit_model, h = validation_h)
    bind_cols(
      tibble(model = name, AICc = fit_model$aicc),
      metrics(validation$index, as.numeric(fit$mean), selection_train$index) %>%
        rename_with(~ paste0("Validation_", .x)),
      residual_diagnostics(fit_model, fitdf = length(coef(fit_model)))
    )
  }, error = function(error) {
    tibble(
      model = name, AICc = NA_real_, Validation_MAE = NA_real_,
      Validation_RMSE = NA_real_, Validation_MAPE = NA_real_, Validation_MASE = NA_real_,
      Ljung_Box_lag = NA_integer_, Ljung_Box_p_value = NA_real_,
      Ljung_Box_acceptable = "Unavailable", Ljung_Box_decision = "Unavailable",
      Residual_ACF_spike_count = NA_integer_, Residual_ACF_spike_lags = "Unavailable",
      Residual_ACF_has_consecutive_spikes = "Unavailable",
      Residual_ACF_pattern = "Unavailable", Residual_ACF_acceptable = "Unavailable",
      Residuals_acceptable = "No", Diagnostic_note = "Candidate model failed to fit."
    )
  })
}

candidate_results <- bind_rows(Map(evaluate_candidate, names(candidates), candidates)) %>%
  arrange(desc(Residuals_acceptable == "Yes"), Validation_RMSE)
write_csv(candidate_results, "output/member_D_candidate_results.csv")
print(candidate_results)

acceptable_results <- candidate_results %>% filter(Residuals_acceptable == "Yes")
if (nrow(acceptable_results) > 0) {
  recommended_name <- acceptable_results$model[which.min(acceptable_results$Validation_RMSE)]
  recommendation_note <- "Recommended: lowest validation-RMSE candidate passing both residual checks."
} else {
  recommended_name <- candidate_results$model[which.min(candidate_results$Validation_RMSE)]
  recommendation_note <- "Fallback: no candidate passed both residual checks; selected the lowest validation-RMSE candidate."
}
fit_model <- candidates[[recommended_name]](train_ts)
cat("\n", recommendation_note, "\nSelected candidate:", recommended_name, "\n", sep = "")
fit <- forecast(fit_model, h = h)

save_plot(autoplot(train_ts) + labs(title = "Tan Wei Ching: Overall CPI Training Series", x = "Year", y = "CPI index") + theme_minimal(), file.path(out, "Member_D_01_autoplot.png"))
save_plot(autoplot(fit) + autolayer(test_ts, series = "Actual test CPI") + labs(title = paste("Tan Wei Ching:", recommended_name, "18-Month Holdout Forecast"), x = "Year", y = "CPI index") + theme_minimal(), file.path(out, "Member_D_02_forecast.png"))
png(file.path(out, "Member_D_03_residuals.png"), width = 3300, height = 2400, res = 300)
checkresiduals(fit_model, lag = 24)
dev.off()

accuracy_D <- metrics(test$index, as.numeric(fit$mean), train$index) %>%
  bind_cols(residual_diagnostics(fit_model, fitdf = length(coef(fit_model)))) %>%
  mutate(
    member = "Tan Wei Ching",
    model_family = "Time-series regression",
    selected_variant = recommended_name,
    model_specification = recommended_name,
    model = model_specification
  ) %>%
  select(member, model_family, selected_variant, model_specification, model,
         MAE, RMSE, MAPE, MASE, everything())
write_csv(accuracy_D, "output/member_D_accuracy.csv")
print(accuracy_D)
