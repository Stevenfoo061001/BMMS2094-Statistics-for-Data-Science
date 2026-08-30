# Huxley - Seasonal Naive baseline. Run after 01_data_prep.R.

source("scripts/00_setup.R")
if (!file.exists("data/training_data.rds")) source("scripts/01_data_prep.R")
train <- readRDS("data/training_data.rds"); test <- readRDS("data/testing_data.rds")
train_ts <- ts(train$index, start = c(2010, 1), frequency = 12)
test_ts <- ts(test$index, start = c(2025, 1), frequency = 12)
out <- "output/plots/Member_A_snaive"

save_plot(autoplot(train_ts) + labs(title = "Huxley: Overall CPI Training Series", x = "Year", y = "CPI index") + theme_minimal(), file.path(out, "Member_A_01_autoplot.png"))
save_plot(ggseasonplot(train_ts, year.labels = TRUE) + labs(title = "Huxley: CPI Seasonal Plot", y = "CPI index") + theme_minimal(), file.path(out, "Member_A_02_season.png"))
fit <- snaive(train_ts, h = h)
save_plot(autoplot(fit) + autolayer(test_ts, series = "Actual test CPI") + labs(title = "Huxley: Seasonal Naive 18-Month Holdout Forecast", x = "Year", y = "CPI index") + theme_minimal(), file.path(out, "Member_A_03_forecast.png"))
png(file.path(out, "Member_A_04_residuals.png"), width = 3300, height = 2400, res = 300)
checkresiduals(fit, lag = 24)
dev.off()
accuracy_A <- metrics(test$index, as.numeric(fit$mean), train$index) %>%
  bind_cols(residual_diagnostics(fit, fitdf = 0)) %>%
  mutate(
    member = "Huxley",
    model_family = "Seasonal Naive",
    selected_variant = "Seasonal Naive",
    model_specification = "Seasonal Naive (monthly period = 12)",
    model = model_specification
  ) %>%
  select(member, model_family, selected_variant, model_specification, model,
         MAE, RMSE, MAPE, MASE, everything())
write_csv(accuracy_A, "output/member_A_accuracy.csv")
print(accuracy_A)
