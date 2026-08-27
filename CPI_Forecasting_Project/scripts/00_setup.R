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
    Residuals_acceptable = if_else(result$p.value > 0.05, "Yes", "No")
  )
}
