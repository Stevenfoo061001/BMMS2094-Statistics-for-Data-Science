# Ooi Mei Yi - select the best exponential-smoothing candidate.

source("scripts/00_setup.R")
if (!file.exists("data/training_data.rds")) source("scripts/01_data_prep.R")

train <- readRDS("data/training_data.rds")
test <- readRDS("data/testing_data.rds")
selection_train <- train %>% slice_head(n = nrow(train) - validation_h)
validation <- train %>% slice_tail(n = validation_h)

train_ts <- ts(train$index, start = c(2010, 1), frequency = 12)
selection_train_ts <- ts(selection_train$index, start = c(2010, 1), frequency = 12)
test_ts <- ts(test$index, start = c(2025, 1), frequency = 12)
out <- "output/plots/Member_B_holt_winters"

# Variants are selected using the internal 2024 validation period, not the
# final Jan 2025-Jun 2026 test set.
candidates <- list(
  "Holt linear trend" = function(series, horizon) holt(series, damped = FALSE, h = horizon),
  "Damped Holt trend" = function(series, horizon) holt(series, damped = TRUE, h = horizon),
  "Holt-Winters additive" = function(series, horizon) hw(series, seasonal = "additive", damped = FALSE, h = horizon),
  "Damped Holt-Winters additive" = function(series, horizon) hw(series, seasonal = "additive", damped = TRUE, h = horizon)
)

evaluate_candidate <- function(name, candidate_function) {
  tryCatch({
    fit <- candidate_function(selection_train_ts, validation_h)
    bind_cols(
      tibble(model = name),
      metrics(validation$index, as.numeric(fit$mean), selection_train$index) %>%
        rename_with(~ paste0("Validation_", .x)),
      ljung_box_diagnostic(fit, fitdf = 0),
      residual_acf_diagnostic(fit)
    )
  }, error = function(error) {
    tibble(model = name, Validation_MAE = NA_real_, Validation_RMSE = NA_real_, Validation_MAPE = NA_real_, Validation_MASE = NA_real_, Ljung_Box_lag = NA_real_, Ljung_Box_p_value = NA_real_, Ljung_Box_acceptable = NA_character_, Residual_ACF_acceptable = NA_character_, Residual_ACF_spike_lags = NA_character_)
  })
}

candidate_results <- bind_rows(Map(evaluate_candidate, names(candidates), candidates)) %>%
  mutate(Residuals_acceptable = if_else(
    Ljung_Box_acceptable == "Yes" & Residual_ACF_acceptable == "Yes", "Yes", "No"
  )) %>%
  arrange(desc(Residuals_acceptable == "Yes"), Validation_RMSE)
write_csv(candidate_results, "output/member_B_candidate_results.csv")
print(candidate_results)

acceptable_results <- candidate_results %>% filter(Residuals_acceptable == "Yes")
if (nrow(acceptable_results) > 0) {
  recommended_name <- acceptable_results$model[which.min(acceptable_results$Validation_RMSE)]
  recommendation_note <- "Recommended: lowest validation-RMSE candidate passing both residual checks."
} else {
  recommended_name <- candidate_results$model[which.min(candidate_results$Validation_RMSE)]
  recommendation_note <- "Fallback: no candidate passed both residual checks; selected the lowest validation-RMSE candidate."
}
fit <- candidates[[recommended_name]](train_ts, h)
cat("\n", recommendation_note, "\nSelected candidate:", recommended_name, "\n", sep = "")

save_plot(autoplot(train_ts) + labs(title = "Ooi Mei Yi: Overall CPI Training Series", x = "Year", y = "CPI index") + theme_minimal(), file.path(out, "Member_B_01_autoplot.png"))
save_plot(ggseasonplot(train_ts, year.labels = TRUE) + labs(title = "Ooi Mei Yi: CPI Seasonal Plot", y = "CPI index") + theme_minimal(), file.path(out, "Member_B_02_season.png"))
save_plot(autoplot(fit) + autolayer(test_ts, series = "Actual test CPI") + labs(title = paste("Ooi Mei Yi:", recommended_name, "18-Month Holdout Forecast"), x = "Year", y = "CPI index") + theme_minimal(), file.path(out, "Member_B_03_forecast.png"))
png(file.path(out, "Member_B_04_residuals.png"), width = 3300, height = 2400, res = 300)
checkresiduals(fit, lag = 24)
dev.off()

accuracy_B <- metrics(test$index, as.numeric(fit$mean), train$index) %>%
  bind_cols(ljung_box_diagnostic(fit, fitdf = 0), residual_acf_diagnostic(fit)) %>%
  mutate(
    Residuals_acceptable = if_else(
      Ljung_Box_acceptable == "Yes" & Residual_ACF_acceptable == "Yes", "Yes", "No"
    ),
    member = "Ooi Mei Yi",
    model = recommended_name
  ) %>%
  select(member, model, everything())
write_csv(accuracy_B, "output/member_B_accuracy.csv")
print(accuracy_B)
