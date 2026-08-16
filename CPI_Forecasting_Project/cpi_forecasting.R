# Forecast Malaysia's low-income household CPI using four time-series methods.
# Run from this folder in Posit Cloud with: Rscript cpi_forecasting.R

required_packages <- c("readr", "dplyr", "ggplot2", "forecast")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing package(s): ", paste(missing_packages, collapse = ", "),
      ". Install them with: install.packages(c(",
      paste(sprintf("'%s'", missing_packages), collapse = ", "), "))"
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(forecast)
})

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
data_file <- file.path(project_dir, "cpi_2d_lowincome.csv")
figures_dir <- file.path(project_dir, "outputs", "figures")
tables_dir <- file.path(project_dir, "outputs", "tables")
forecast_horizon <- 12
seasonal_period <- 12

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

save_plot <- function(plot_object, filename, width = 11, height = 5) {
  ggsave(
    filename = file.path(figures_dir, filename), plot = plot_object,
    width = width, height = height, units = "in", dpi = 300, bg = "white"
  )
}

fail <- function(message) {
  stop(paste("DATA VALIDATION FAILED:", message), call. = FALSE)
}

make_monthly_ts <- function(values, dates) {
  ts(values, start = c(as.integer(format(min(dates), "%Y")), as.integer(format(min(dates), "%m"))), frequency = 12)
}

metric_values <- function(actual, predicted) {
  errors <- actual - predicted
  tibble(
    MAE = mean(abs(errors)),
    RMSE = sqrt(mean(errors^2)),
    `MAPE (%)` = mean(abs(errors / actual)) * 100
  )
}

if (!file.exists(data_file)) {
  fail("Cannot find cpi_2d_lowincome.csv beside cpi_forecasting.R.")
}

data <- read_csv(data_file, show_col_types = FALSE)
cat("Forecasting Malaysia's Monthly Consumer Price Index for Low-Income Households\n\n")
cat("--- Raw dataset preview ---\n")
cat("Shape:", nrow(data), "rows and", ncol(data), "columns\n")
cat("Columns:", paste(names(data), collapse = ", "), "\n")
cat("Data types before conversion:\n")
print(vapply(data, class, character(1)))
cat("First five rows:\n")
print(head(data, 5))

required_columns <- c("date", "division", "index")
absent_columns <- setdiff(required_columns, names(data))
if (length(absent_columns) > 0) fail(paste("Essential column(s) missing:", paste(absent_columns, collapse = ", ")))

data <- data %>% mutate(date = as.Date(date), index = suppressWarnings(as.numeric(index)))
if (any(is.na(data$date))) fail("The date column contains values that cannot be converted to dates.")
if (any(is.na(data$index))) fail("The index column contains values that cannot be converted to numeric values.")

missing_counts <- colSums(is.na(data))
duplicate_records <- sum(duplicated(data[c("date", "division")]))
if (any(missing_counts > 0)) fail(paste("Missing values found in:", paste(names(missing_counts)[missing_counts > 0], collapse = ", ")))
if (duplicate_records > 0) fail(paste("Found", duplicate_records, "duplicate date-division record(s)."))

divisions <- sort(unique(data$division))
if (!("overall" %in% divisions)) fail("The required division 'overall' is not available.")
overall <- data %>% filter(division == "overall") %>% arrange(date)
if (nrow(overall) <= forecast_horizon + seasonal_period) fail("The overall series is too short for a seasonal model and 12-month test set.")

expected_dates <- seq(min(overall$date), max(overall$date), by = "month")
missing_months <- setdiff(expected_dates, overall$date)
if (length(missing_months) > 0) fail(paste("Missing month(s) in overall CPI:", paste(format(missing_months, "%Y-%m"), collapse = ", ")))

cat("\n--- Validation summary ---\n")
cat("Missing values:", sum(missing_counts), "\n")
cat("Duplicate date-division combinations:", duplicate_records, "\n")
cat("Available divisions (", length(divisions), "): ", paste(divisions, collapse = ", "), "\n", sep = "")
cat("Overall CPI observations:", nrow(overall), "\n")
cat("Overall CPI date range:", format(min(overall$date), "%B %Y"), "to", format(max(overall$date), "%B %Y"), "\n")
cat("Overall CPI range:", sprintf("%.1f", min(overall$index)), "to", sprintf("%.1f", max(overall$index)), "\n")
cat("Missing months in overall CPI:", length(missing_months), "\n")
cat("Result: all essential validation checks passed.\n")

prepared <- overall %>%
  mutate(
    monthly_change_pct = (index / lag(index) - 1) * 100,
    yoy_inflation_pct = (index / lag(index, 12) - 1) * 100,
    year = as.integer(format(date, "%Y")),
    month_number = as.integer(format(date, "%m")),
    month_name = format(date, "%B")
  )
write_csv(prepared, file.path(tables_dir, "cleaned_overall_cpi.csv"), na = "")

descriptive_statistics <- bind_rows(lapply(c("index", "monthly_change_pct", "yoy_inflation_pct"), function(variable) {
  values <- prepared[[variable]]
  tibble(
    variable = variable, count = sum(!is.na(values)), mean = mean(values, na.rm = TRUE),
    sd = sd(values, na.rm = TRUE), min = min(values, na.rm = TRUE),
    q1 = quantile(values, 0.25, na.rm = TRUE), median = median(values, na.rm = TRUE),
    q3 = quantile(values, 0.75, na.rm = TRUE), max = max(values, na.rm = TRUE)
  )
}))
write_csv(descriptive_statistics, file.path(tables_dir, "descriptive_statistics.csv"), na = "")

latest <- prepared[nrow(prepared), ]
max_yoy <- prepared %>% filter(yoy_inflation_pct == max(yoy_inflation_pct, na.rm = TRUE)) %>% slice(1)
min_yoy <- prepared %>% filter(yoy_inflation_pct == min(yoy_inflation_pct, na.rm = TRUE)) %>% slice(1)
cat("\n--- Factual descriptive interpretations ---\n")
cat("Latest CPI is", sprintf("%.1f", latest$index), "in", format(latest$date, "%B %Y"), ".\n")
cat("Latest monthly change is", sprintf("%.2f%%", latest$monthly_change_pct), "and latest year-on-year inflation is", sprintf("%.2f%%", latest$yoy_inflation_pct), ".\n")
cat("Highest year-on-year inflation is", sprintf("%.2f%%", max_yoy$yoy_inflation_pct), "in", format(max_yoy$date, "%B %Y"), ".\n")
cat("Lowest year-on-year inflation is", sprintf("%.2f%%", min_yoy$yoy_inflation_pct), "in", format(min_yoy$date, "%B %Y"), ".\n")

base_theme <- theme_minimal(base_size = 12) + theme(panel.background = element_rect(fill = "white", colour = NA))
save_plot(ggplot(prepared, aes(date, index)) + geom_line(colour = "#1f5a99", linewidth = 0.8) + labs(title = "Malaysia Low-Income Household Overall CPI", x = "Date", y = "CPI index") + base_theme, "01_overall_cpi_line.png")
save_plot(ggplot(prepared, aes(date, monthly_change_pct)) + geom_line(colour = "#d95f02", linewidth = 0.7) + geom_hline(yintercept = 0) + labs(title = "Monthly Percentage Change in Overall CPI", x = "Date", y = "Monthly change (%)") + base_theme, "02_monthly_percentage_change.png")
save_plot(ggplot(prepared, aes(date, yoy_inflation_pct)) + geom_line(colour = "#7570b3", linewidth = 0.7) + geom_hline(yintercept = 0) + labs(title = "Year-on-Year Inflation in Overall CPI", x = "Date", y = "Year-on-year inflation (%)") + base_theme, "03_yoy_inflation.png")
save_plot(ggplot(prepared, aes(factor(month_number, levels = 1:12, labels = month.abb), index)) + geom_boxplot(fill = "#8dd3c7") + labs(title = "Seasonal Distribution of Overall CPI by Calendar Month", x = "Calendar month", y = "CPI index") + base_theme, "04_seasonal_cpi_boxplot.png")

overall_ts <- make_monthly_ts(prepared$index, prepared$date)
decomposition <- decompose(overall_ts, type = "additive")
png(file.path(figures_dir, "05_seasonal_trend_decomposition.png"), width = 3300, height = 2400, res = 300)
plot(decomposition, main = "Additive Seasonal-Trend Decomposition of Overall CPI")
dev.off()
png(file.path(figures_dir, "06_acf.png"), width = 3000, height = 1500, res = 300)
Acf(overall_ts, lag.max = 24, main = "Autocorrelation Function of Overall CPI", xlab = "Lag (months)")
dev.off()
png(file.path(figures_dir, "07_pacf.png"), width = 3000, height = 1500, res = 300)
Pacf(overall_ts, lag.max = 24, main = "Partial Autocorrelation Function of Overall CPI", xlab = "Lag (months)")
dev.off()

training <- prepared %>% slice_head(n = n() - forecast_horizon)
testing <- prepared %>% slice_tail(n = forecast_horizon)
write_csv(training, file.path(tables_dir, "training_data.csv"), na = "")
write_csv(testing, file.path(tables_dir, "testing_data.csv"), na = "")
cat("\n--- Chronological train-test split ---\n")
cat("Training:", nrow(training), "observations,", format(min(training$date), "%Y-%m"), "to", format(max(training$date), "%Y-%m"), "\n")
cat("Testing:", nrow(testing), "observations,", format(min(testing$date), "%Y-%m"), "to", format(max(testing$date), "%Y-%m"), "\n")
split_plot_data <- bind_rows(training %>% transmute(date, index, period = "Training"), testing %>% transmute(date, index, period = "Testing"))
save_plot(ggplot(split_plot_data, aes(date, index, colour = period)) + geom_line(linewidth = 0.8) + geom_vline(xintercept = as.numeric(min(testing$date)), linetype = "dashed") + scale_colour_manual(values = c("Training" = "#1f5a99", "Testing" = "#e7298a")) + labs(title = "Chronological Training and Testing Periods", x = "Date", y = "CPI index", colour = NULL) + base_theme, "08_training_testing_split.png")

train_ts <- make_monthly_ts(training$index, training$date)
seasonal_naive_fc <- snaive(train_ts, h = forecast_horizon)
holt_winters_fc <- hw(train_ts, seasonal = "additive", h = forecast_horizon)
sarima_model <- Arima(train_ts, order = c(1, 1, 1), seasonal = c(1, 1, 1))
sarima_fc <- forecast(sarima_model, h = forecast_horizon)
regression_model <- tslm(train_ts ~ trend + season)
regression_fc <- forecast(regression_model, h = forecast_horizon)

forecasts <- list(
  "Seasonal Naive" = as.numeric(seasonal_naive_fc$mean),
  "Holt-Winters" = as.numeric(holt_winters_fc$mean),
  "SARIMA" = as.numeric(sarima_fc$mean),
  "Trend + Monthly Dummies" = as.numeric(regression_fc$mean)
)
forecast_file_names <- c(
  "Seasonal Naive" = "seasonal_naive",
  "Holt-Winters" = "holt_winters",
  "SARIMA" = "sarima",
  "Trend + Monthly Dummies" = "trend_plus_monthly_dummies"
)
results <- testing %>% transmute(date, actual_cpi = index)
metrics <- tibble()
for (model_name in names(forecasts)) {
  prediction <- forecasts[[model_name]]
  safe_name <- forecast_file_names[[model_name]]
  results[[safe_name]] <- prediction
  metrics <- bind_rows(metrics, bind_cols(tibble(model = model_name), metric_values(testing$index, prediction)))
  plot_data <- bind_rows(
    training %>% transmute(date, index, series = "Training"),
    testing %>% transmute(date, index, series = "Actual test CPI"),
    tibble(date = testing$date, index = prediction, series = paste(model_name, "forecast"))
  )
  forecast_label <- paste(model_name, "forecast")
  colours <- c("Training" = "#bdbdbd", "Actual test CPI" = "#e7298a")
  colours[forecast_label] <- "#1f5a99"
  save_plot(ggplot(plot_data, aes(date, index, colour = series, linetype = series)) + geom_line(linewidth = 0.8) + geom_point(data = filter(plot_data, series != "Training"), size = 1.6) + scale_colour_manual(values = colours) + labs(title = paste(model_name, ": 12-Month CPI Forecast"), x = "Date", y = "CPI index", colour = NULL, linetype = NULL) + base_theme, paste0("09_forecast_", safe_name, ".png"))
}
metrics <- metrics %>% arrange(RMSE)
write_csv(results, file.path(tables_dir, "test_forecasts.csv"))
write_csv(metrics, file.path(tables_dir, "forecast_accuracy_metrics.csv"))
cat("\n--- Forecast accuracy on the final 12 months ---\n")
print(metrics)
cat("Lowest RMSE model:", metrics$model[1], "(", sprintf("%.3f", metrics$RMSE[1]), ").\n", sep = "")
save_plot(ggplot(metrics, aes(reorder(model, RMSE), RMSE, fill = model)) + geom_col(show.legend = FALSE) + coord_flip() + labs(title = "Forecast Accuracy Comparison: Lower RMSE Is Better", x = "Forecasting model", y = "RMSE (CPI index points)") + base_theme, "10_forecast_accuracy_comparison.png")

cat("\nCompleted successfully. Tables and figures are in outputs/.\n")
