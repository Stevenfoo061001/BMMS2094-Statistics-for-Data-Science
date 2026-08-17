# Steven - SARIMA(1,1,1)(1,1,1)[12].

source("scripts/00_setup.R")
if (!file.exists("data/training_data.rds")) source("scripts/01_data_prep.R")
train <- readRDS("data/training_data.rds"); test <- readRDS("data/testing_data.rds")
train_ts <- ts(train$index, start = c(2010, 1), frequency = 12)
test_ts <- ts(test$index, start = c(2025, 7), frequency = 12)
out <- "output/plots/Member_C_sarima"

save_plot(autoplot(train_ts) + labs(title = "Steven: Overall CPI Training Series", x = "Year", y = "CPI index") + theme_minimal(), file.path(out, "Member_C_01_autoplot.png"))
png(file.path(out, "Member_C_02_acf.png"), width = 3000, height = 1500, res = 300); Acf(train_ts, lag.max = 24, main = "Steven: CPI ACF"); dev.off()
png(file.path(out, "Member_C_03_pacf.png"), width = 3000, height = 1500, res = 300); Pacf(train_ts, lag.max = 24, main = "Steven: CPI PACF"); dev.off()
fit_model <- Arima(train_ts, order = c(1, 1, 1), seasonal = c(1, 1, 1))
fit <- forecast(fit_model, h = h)
save_plot(autoplot(fit) + autolayer(test_ts, series = "Actual test CPI") + labs(title = "Steven: SARIMA 12-Month Forecast", x = "Year", y = "CPI index") + theme_minimal(), file.path(out, "Member_C_04_forecast.png"))
png(file.path(out, "Member_C_05_residuals.png"), width = 3300, height = 2400, res = 300); checkresiduals(fit_model); dev.off()
accuracy_C <- metrics(test$index, as.numeric(fit$mean), train$index) %>% mutate(member = "Steven", model = "SARIMA(1,1,1)(1,1,1)[12]") %>% select(member, model, everything())
write_csv(accuracy_C, "output/member_C_accuracy.csv")
print(accuracy_C)
