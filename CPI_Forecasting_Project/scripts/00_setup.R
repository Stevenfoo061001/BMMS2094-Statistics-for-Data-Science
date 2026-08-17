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

h <- 12
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
