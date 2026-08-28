# 01_data_prep.R - shared validation, preparation, and chronological split.

source("scripts/00_setup.R")
raw_data <- read_csv("data/cpi_2d_lowincome.csv", show_col_types = FALSE)
required_columns <- c("date", "division", "index")
if (length(setdiff(required_columns, names(raw_data))) > 0) stop("Required columns are missing.")

data <- raw_data %>% mutate(date = as.Date(date), index = as.numeric(index))
if (anyNA(data)) stop("Validation failed: missing values found.")
if (any(duplicated(data[c("date", "division")]))) stop("Validation failed: duplicate date-division records found.")

overall <- data %>% filter(division == "overall") %>% arrange(date)
if (nrow(overall) == 0) stop("Validation failed: overall CPI series not found.")
expected_dates <- seq(min(overall$date), max(overall$date), by = "month")
if (length(setdiff(expected_dates, overall$date)) > 0) stop("Validation failed: missing month(s) in overall CPI.")

prepared <- overall %>% mutate(
  monthly_change_pct = (index / lag(index) - 1) * 100,
  yoy_inflation_pct = (index / lag(index, 12) - 1) * 100,
  year = as.integer(format(date, "%Y")),
  month_number = as.integer(format(date, "%m")),
  month_name = format(date, "%B")
)
training <- prepared %>% slice_head(n = nrow(prepared) - h)
testing <- prepared %>% slice_tail(n = h)
train_ts <- ts(training$index, start = c(training$year[1], training$month_number[1]), frequency = seasonal_period)

# Validate the training data before model fitting. The held-out period is not
# used here, so these diagnostics do not leak test information into modelling.
adf_result <- adf.test(train_ts)
kpss_result <- kpss.test(train_ts)
first_difference <- diff(train_ts)
first_difference_acf <- Acf(first_difference, lag.max = 24, plot = FALSE)
lag_12_acf <- as.numeric(first_difference_acf$acf[13])
acf_significance_limit <- qnorm(0.975) / sqrt(length(first_difference))

stationarity_diagnostics <- tibble(
  diagnostic = c("ADF test", "KPSS test", "Seasonal ACF at lag 12 (first difference)"),
  null_hypothesis = c(
    "Series has a unit root (non-stationary)",
    "Series is stationary",
    "No annual autocorrelation after first differencing"
  ),
  statistic = c(unname(adf_result$statistic), unname(kpss_result$statistic), lag_12_acf),
  p_value = c(adf_result$p.value, kpss_result$p.value, NA_real_),
  conclusion = c(
    if_else(adf_result$p.value < 0.05, "Reject unit root", "Do not reject unit root"),
    if_else(kpss_result$p.value > 0.05, "Do not reject stationarity", "Reject stationarity"),
    if_else(abs(lag_12_acf) > acf_significance_limit,
            "Evidence of annual seasonal correlation", "No clear annual seasonal correlation")
  )
)

level_stationary <- adf_result$p.value < 0.05 && kpss_result$p.value > 0.05
cat("Level series stationary:", if_else(level_stationary, "Yes", "No"), "\n")
cat("Seasonal ACF at lag 12 after first differencing:", round(lag_12_acf, 3),
    "(approximate 95% limit: +/-", round(acf_significance_limit, 3), ")\n", sep = "")

# Early exploratory plot: inspect the raw series for trend, level changes,
# and possible seasonality before fitting any forecasting model.
initial_series_plot <- ggplot(prepared, aes(x = date, y = index)) +
  geom_line(linewidth = 0.7, colour = "#1F4E79") +
  labs(
    title = "Malaysia Overall CPI Time Series",
    subtitle = "Monthly CPI index; inspect trend and potential seasonality before modelling",
    x = "Date",
    y = "CPI index"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

print(initial_series_plot)
save_plot(initial_series_plot, "output/plots/01_initial_cpi_time_series.png")

seasonal_plot <- ggseasonplot(train_ts, year.labels = TRUE) +
  labs(
    title = "CPI Seasonal Plot: Training Data",
    subtitle = "Compare the month-by-month shape across years",
    x = "Month",
    y = "CPI index"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
save_plot(seasonal_plot, "output/plots/02_training_seasonal_plot.png")

png("output/plots/03_training_acf.png", width = 3300, height = 1500, res = 300)
Acf(train_ts, lag.max = 36, main = "Training CPI ACF: Raw Series")
dev.off()

png("output/plots/04_first_difference_acf.png", width = 3300, height = 1500, res = 300)
Acf(first_difference, lag.max = 36, main = "Training CPI ACF: First-Differenced Series")
dev.off()

dir.create("output", showWarnings = FALSE)
write_csv(stationarity_diagnostics, "output/stationarity_diagnostics.csv")
write_csv(prepared, "output/cleaned_overall_cpi.csv", na = "")
write_csv(training, "output/training_data.csv", na = "")
write_csv(testing, "output/testing_data.csv", na = "")
saveRDS(prepared, "data/overall_cpi_prepared.rds")
saveRDS(training, "data/training_data.rds")
saveRDS(testing, "data/testing_data.rds")

descriptive_statistics <- bind_rows(lapply(c("index", "monthly_change_pct", "yoy_inflation_pct"), function(variable) {
  values <- prepared[[variable]]
  tibble(variable = variable, count = sum(!is.na(values)), mean = mean(values, na.rm = TRUE), sd = sd(values, na.rm = TRUE), min = min(values, na.rm = TRUE), q1 = quantile(values, .25, na.rm = TRUE), median = median(values, na.rm = TRUE), q3 = quantile(values, .75, na.rm = TRUE), max = max(values, na.rm = TRUE))
}))
write_csv(descriptive_statistics, "output/descriptive_statistics.csv")

cat("Validation passed:", nrow(data), "rows;", nrow(overall), "overall monthly observations;", format(min(overall$date), "%b %Y"), "to", format(max(overall$date), "%b %Y"), "\n")
cat("Train:", nrow(training), "observations. Test:", nrow(testing), "observations.", "\n")
