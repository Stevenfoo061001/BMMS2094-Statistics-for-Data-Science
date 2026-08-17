# Ooi Mei Yi - Holt-Winters additive exponential smoothing.

source("scripts/00_setup.R")
if (!file.exists("data/training_data.rds")) source("scripts/01_data_prep.R")
train <- readRDS("data/training_data.rds"); test <- readRDS("data/testing_data.rds")
train_ts <- ts(train$index, start = c(2010, 1), frequency = 12)
test_ts <- ts(test$index, start = c(2025, 7), frequency = 12)
out <- "output/plots/Member_B_holt_winters"

save_plot(autoplot(train_ts) + labs(title = "Ooi Mei Yi: Overall CPI Training Series", x = "Year", y = "CPI index") + theme_minimal(), file.path(out, "Member_B_01_autoplot.png"))
save_plot(ggseasonplot(train_ts, year.labels = TRUE) + labs(title = "Ooi Mei Yi: CPI Seasonal Plot", y = "CPI index") + theme_minimal(), file.path(out, "Member_B_02_season.png"))
fit <- hw(train_ts, seasonal = "additive", h = h)
save_plot(autoplot(fit) + autolayer(test_ts, series = "Actual test CPI") + labs(title = "Ooi Mei Yi: Holt-Winters 12-Month Forecast", x = "Year", y = "CPI index") + theme_minimal(), file.path(out, "Member_B_03_forecast.png"))
png(file.path(out, "Member_B_04_residuals.png"), width = 3300, height = 2400, res = 300); checkresiduals(fit); dev.off()
accuracy_B <- metrics(test$index, as.numeric(fit$mean), train$index) %>% mutate(member = "Ooi Mei Yi", model = "Holt-Winters") %>% select(member, model, everything())
write_csv(accuracy_B, "output/member_B_accuracy.csv")
print(accuracy_B)
