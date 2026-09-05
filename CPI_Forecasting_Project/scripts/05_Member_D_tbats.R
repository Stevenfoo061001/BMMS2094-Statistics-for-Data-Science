# Steven - TBATS forecasting model.
# Run after 01_data_prep.R. TBATS is one of the four core group models.

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
out <- "output/plots/Member_D_tbats"

# TBATS supports complex seasonal structure while retaining an explicit
# time-series model that can be assessed with the same diagnostics as the rest
# of the group models.
fit_tbats <- function(series, horizon) {
  model <- tbats(series, seasonal.periods = seasonal_period, use.box.cox = NULL)
  list(model = model, forecast = forecast(model, h = horizon))
}

validation_fit <- fit_tbats(selection_train_ts, validation_h)
validation_results <- bind_cols(
  metrics(validation$index, as.numeric(validation_fit$forecast$mean), selection_train$index) %>%
    rename_with(~ paste0("Validation_", .x)),
  residual_diagnostics(validation_fit$model, fitdf = 0),
  overfitting_diagnostic(validation_fit$forecast, validation_ts) %>%
    rename_with(~ paste0("Validation_", .x))
)
write_csv(
  bind_cols(tibble(model = "TBATS (annual seasonality = 12)"), validation_results),
  "output/member_D_candidate_results.csv"
)

fit <- fit_tbats(train_ts, h)
forecast_D <- fit$forecast

save_plot(
  autoplot(train_ts) +
    labs(title = "Steven: Overall CPI Training Series", x = "Year", y = "CPI index") +
    theme_minimal(),
  file.path(out, "Member_D_01_autoplot.png")
)

save_plot(
  autoplot(forecast_D) +
    autolayer(test_ts, series = "Actual test CPI") +
    labs(title = "Steven: TBATS 18-Month Holdout Forecast", x = "Year", y = "CPI index") +
    theme_minimal(),
  file.path(out, "Member_D_02_forecast.png")
)

png(file.path(out, "Member_D_03_residuals.png"), width = 3300, height = 2400, res = 300)
checkresiduals(fit$model, lag = 24)
dev.off()

accuracy_D <- metrics(test$index, as.numeric(forecast_D$mean), train$index) %>%
  bind_cols(
    residual_diagnostics(fit$model, fitdf = 0),
    overfitting_diagnostic(forecast_D, test_ts)
  ) %>%
  mutate(
    member = "Steven",
    model_family = "TBATS",
    selected_variant = "TBATS (annual seasonality = 12)",
    model_specification = selected_variant,
    model = model_specification
  ) %>%
  select(member, model_family, selected_variant, model_specification, model,
         MAE, RMSE, MAPE, MASE, everything())
write_csv(accuracy_D, "output/member_D_accuracy.csv")
print(validation_results)
print(accuracy_D)
