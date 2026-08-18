# 07_Steven_sarima_testing.R - test alternative SARIMA specifications.
# Run after 01_data_prep.R. This script does not overwrite member_C_accuracy.csv.

source("scripts/00_setup.R")
if (!file.exists("data/training_data.rds")) source("scripts/01_data_prep.R")

train <- readRDS("data/training_data.rds")
test <- readRDS("data/testing_data.rds")
train_ts <- ts(train$index, start = c(2010, 1), frequency = 12)
test_ts <- ts(test$index, start = c(2025, 7), frequency = 12)
out <- "output/plots/Member_C_sarima"

# Ljung-Box p-value: values above 0.05 indicate no statistically significant
# residual autocorrelation at the chosen lag.
ljung_box_p_value <- function(model, lag = 24) {
  number_of_parameters <- length(coef(model))
  Box.test(residuals(model), lag = lag, type = "Ljung-Box", fitdf = number_of_parameters)$p.value
}

# Fit a model safely. A failed or non-convergent candidate is recorded as NA.
evaluate_candidate <- function(name, model_function) {
  tryCatch({
    model <- model_function()
    forecast_values <- as.numeric(forecast(model, h = h)$mean)
    bind_cols(
      tibble(model = name, AICc = model$aicc, Ljung_Box_p_value = ljung_box_p_value(model)),
      metrics(test$index, forecast_values, train$index)
    )
  }, error = function(error) {
    tibble(model = name, AICc = NA_real_, Ljung_Box_p_value = NA_real_, MAE = NA_real_, RMSE = NA_real_, MAPE = NA_real_, MASE = NA_real_)
  })
}

# The first model is Steven's original model. The next four are common seasonal
# alternatives; the final model is selected automatically by AICc.
candidates <- list(
  "SARIMA(1,1,1)(1,1,1)[12]" = function() Arima(train_ts, order = c(1, 1, 1), seasonal = c(1, 1, 1)),
  "SARIMA(0,1,1)(0,1,1)[12]" = function() Arima(train_ts, order = c(0, 1, 1), seasonal = c(0, 1, 1)),
  "SARIMA(1,1,0)(1,1,0)[12]" = function() Arima(train_ts, order = c(1, 1, 0), seasonal = c(1, 1, 0)),
  "SARIMA(1,1,1)(1,1,0)[12]" = function() Arima(train_ts, order = c(1, 1, 1), seasonal = c(1, 1, 0)),
  "SARIMA(1,1,0)(0,1,1)[12]" = function() Arima(train_ts, order = c(1, 1, 0), seasonal = c(0, 1, 1)),
  "auto.arima selected model" = function() auto.arima(train_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
)

candidate_results <- bind_rows(Map(evaluate_candidate, names(candidates), candidates)) %>%
  mutate(residuals_acceptable = if_else(Ljung_Box_p_value > 0.05, "Yes", "No")) %>%
  arrange(desc(residuals_acceptable == "Yes"), RMSE)

write_csv(candidate_results, "output/steven_sarima_candidate_results.csv")
print(candidate_results)

# Prefer a model with acceptable residuals. If none pass, select the smallest
# Ljung-Box issue first, then report that further refinement is required.
acceptable_results <- candidate_results %>% filter(residuals_acceptable == "Yes")
if (nrow(acceptable_results) > 0) {
  recommended_name <- acceptable_results$model[which.min(acceptable_results$RMSE)]
  recommendation_note <- "Recommended: lowest-RMSE candidate with Ljung-Box p-value above 0.05."
} else {
  recommended_name <- candidate_results$model[which.max(candidate_results$Ljung_Box_p_value)]
  recommendation_note <- "No candidate passed the Ljung-Box test; further SARIMA refinement is needed."
}

recommended_model <- candidates[[recommended_name]]()
recommended_forecast <- forecast(recommended_model, h = h)
cat("\n", recommendation_note, "\nSelected candidate:", recommended_name, "\n", sep = "")

comparison_plot <- ggplot(candidate_results, aes(reorder(model, RMSE), RMSE, fill = residuals_acceptable)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c("Yes" = "#1b9e77", "No" = "#d95f02")) +
  labs(title = "Steven: SARIMA Candidate Comparison", subtitle = "Green = Ljung-Box p-value > 0.05", x = "SARIMA candidate", y = "Test RMSE", fill = "Residuals acceptable") +
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
