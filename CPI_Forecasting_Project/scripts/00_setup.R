# 00_setup.R - shared packages and helper functions.
# Run once per Posit Cloud session. Then run the numbered scripts in order.

packages <- c("readr", "dplyr", "ggplot2", "forecast", "tseries")
missing <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(missing) > 0) install.packages(missing)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(forecast)
  library(tseries)
})

# Hold out Jan 2025 to Jun 2026 (18 monthly observations) for final testing.
h <- 18
# Internal validation period used only to choose variants within each family.
# The final 18-month holdout remains untouched until final evaluation.
validation_h <- 12
# Forecast horizon after the final observed month (June 2026).
future_h <- 12
seasonal_period <- 12

save_plot <- function(plot_object, filename, width = 11, height = 5) {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggsave(filename, plot_object, width = width, height = height, units = "in", dpi = 300, bg = "white")
}

mase <- function(actual, predicted, training, m = 12) {
  mean(abs(actual - predicted)) / mean(abs(diff(training, lag = m)))
}

metrics <- function(actual, predicted, training) {
  tibble(
    MAE = mean(abs(actual - predicted)),
    RMSE = sqrt(mean((actual - predicted)^2)),
    MAPE = mean(abs((actual - predicted) / actual)) * 100,
    MASE = mase(actual, predicted, training)
  )
}

# A p-value above 0.05 suggests that residual autocorrelation is not detected.
# For monthly data, test up to two annual cycles (24 lags).
ljung_box_diagnostic <- function(model, lag = 24, fitdf = 0) {
  residual_values <- na.omit(as.numeric(residuals(model)))
  test_lag <- min(lag, length(residual_values) - 1)
  if (test_lag <= fitdf) stop("Ljung-Box lag must be greater than fitdf.")

  result <- Box.test(residual_values, lag = test_lag,
                     type = "Ljung-Box", fitdf = fitdf)
  tibble(
    Ljung_Box_lag = test_lag,
    Ljung_Box_p_value = result$p.value,
    Ljung_Box_acceptable = if_else(result$p.value > 0.05, "Yes", "No")
  )
}

# Residual ACF spikes outside the approximate 95% bounds indicate remaining
# autocorrelation. A sound model should pass this check and Ljung-Box.
residual_acf_diagnostic <- function(model, lag = 24) {
  residual_values <- na.omit(as.numeric(residuals(model)))
  acf_result <- Acf(residual_values, lag.max = min(lag, length(residual_values) - 1), plot = FALSE)
  acf_values <- as.numeric(acf_result$acf)[-1]
  acf_lags <- as.integer(round(as.numeric(acf_result$lag)[-1]))
  limit <- qnorm(0.975) / sqrt(length(residual_values))
  spike_lags <- acf_lags[abs(acf_values) > limit]

  tibble(
    Residual_ACF_acceptable = if_else(length(spike_lags) == 0, "Yes", "No"),
    Residual_ACF_spike_lags = if_else(
      length(spike_lags) == 0, "None", paste(spike_lags, collapse = ", ")
    )
  )
}
