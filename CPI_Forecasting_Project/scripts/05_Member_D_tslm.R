# Tan Wei Ching - time-series linear regression with trend and monthly seasonality.

source("scripts/00_setup.R")
if (!file.exists("data/training_data.rds")) source("scripts/01_data_prep.R")
train <- readRDS("data/training_data.rds"); test <- readRDS("data/testing_data.rds")
train_ts <- ts(train$index, start = c(2010, 1), frequency = 12)
test_ts <- ts(test$index, start = c(2025, 7), frequency = 12)
out <- "output/plots/Member_D_tslm"

save_plot(autoplot(train_ts) + labs(title = "Tan Wei Ching: Overall CPI Training Series", x = "Year", y = "CPI index") + theme_minimal(), file.path(out, "Member_D_01_autoplot.png"))
fit_model <- tslm(train_ts ~ trend + season)
fit <- forecast(fit_model, h = h)
save_plot(autoplot(fit) + autolayer(test_ts, series = "Actual test CPI") + labs(title = "Tan Wei Ching: Trend and Monthly-Seasonality Forecast", x = "Year", y = "CPI index") + theme_minimal(), file.path(out, "Member_D_02_forecast.png"))
png(file.path(out, "Member_D_03_residuals.png"), width = 3300, height = 2400, res = 300); checkresiduals(fit_model); dev.off()
accuracy_D <- metrics(test$index, as.numeric(fit$mean), train$index) %>% mutate(member = "Tan Wei Ching", model = "Trend + Monthly Dummies") %>% select(member, model, everything())
write_csv(accuracy_D, "output/member_D_accuracy.csv")
print(accuracy_D)
