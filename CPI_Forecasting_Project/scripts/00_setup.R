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
# A small number of isolated residual-ACF spikes can occur by chance. Spikes
# are rejected only when they are numerous or form a consecutive pattern.
max_isolated_acf_spikes <- 3

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
safe_residuals <- function(model) {
  tryCatch({
    values <- as.numeric(residuals(model))
    values[is.finite(values)]
  }, error = function(error) numeric(0))
}

ljung_box_diagnostic <- function(model, lag = 24, fitdf = 0) {
  residual_values <- safe_residuals(model)
  test_lag <- min(lag, length(residual_values) - 1)

  if (length(residual_values) < 3 || test_lag <= fitdf || test_lag < 1) {
    return(tibble(
      Ljung_Box_lag = NA_integer_, Ljung_Box_p_value = NA_real_,
      Ljung_Box_acceptable = "Unavailable", Ljung_Box_decision = "Unavailable"
    ))
  }

  result <- tryCatch(
    Box.test(residual_values, lag = test_lag, type = "Ljung-Box", fitdf = fitdf),
    error = function(error) NULL
  )
  if (is.null(result) || !is.finite(result$p.value)) {
    return(tibble(
      Ljung_Box_lag = test_lag, Ljung_Box_p_value = NA_real_,
      Ljung_Box_acceptable = "Unavailable", Ljung_Box_decision = "Unavailable"
    ))
  }

  decision <- if_else(result$p.value > 0.05, "Yes", "No")
  tibble(
    Ljung_Box_lag = test_lag, Ljung_Box_p_value = result$p.value,
    Ljung_Box_acceptable = decision, Ljung_Box_decision = decision
  )
}

# Residual ACF spikes outside the approximate 95% bounds indicate remaining
# autocorrelation. A sound model should pass this check and Ljung-Box.
residual_acf_diagnostic <- function(model, lag = 24) {
  residual_values <- safe_residuals(model)
  if (length(residual_values) < 3) {
    return(tibble(
      Residual_ACF_spike_count = NA_integer_, Residual_ACF_spike_lags = "Unavailable",
      Residual_ACF_has_consecutive_spikes = "Unavailable",
      Residual_ACF_pattern = "Unavailable", Residual_ACF_acceptable = "Unavailable"
    ))
  }

  acf_result <- tryCatch(
    Acf(residual_values, lag.max = min(lag, length(residual_values) - 1), plot = FALSE),
    error = function(error) NULL
  )
  if (is.null(acf_result)) {
    return(tibble(
      Residual_ACF_spike_count = NA_integer_, Residual_ACF_spike_lags = "Unavailable",
      Residual_ACF_has_consecutive_spikes = "Unavailable",
      Residual_ACF_pattern = "Unavailable", Residual_ACF_acceptable = "Unavailable"
    ))
  }

  acf_values <- as.numeric(acf_result$acf)[-1]
  acf_lags <- as.integer(round(as.numeric(acf_result$lag)[-1]))
  limit <- qnorm(0.975) / sqrt(length(residual_values))
  spike_lags <- acf_lags[abs(acf_values) > limit]
  spike_count <- length(spike_lags)
  consecutive <- spike_count > 1 && any(diff(spike_lags) == 1)
  acceptable <- spike_count <= max_isolated_acf_spikes && !consecutive
  pattern <- case_when(
    spike_count == 0 ~ "No significant spikes",
    acceptable ~ "Few isolated spikes",
    TRUE ~ "Consecutive or numerous spikes"
  )

  tibble(
    Residual_ACF_spike_count = spike_count,
    Residual_ACF_spike_lags = if_else(spike_count == 0, "None", paste(spike_lags, collapse = ", ")),
    Residual_ACF_has_consecutive_spikes = if_else(consecutive, "Yes", "No"),
    Residual_ACF_pattern = pattern,
    Residual_ACF_acceptable = if_else(acceptable, "Yes", "No")
  )
}

residual_diagnostics <- function(model, fitdf = 0) {
  bind_cols(ljung_box_diagnostic(model, fitdf = fitdf), residual_acf_diagnostic(model)) %>%
    mutate(
      Residuals_acceptable = if_else(
        !is.na(Ljung_Box_p_value) & Ljung_Box_p_value > 0.05 & Residual_ACF_acceptable == "Yes",
        "Yes", "No"
      ),
      Diagnostic_note = case_when(
        is.na(Ljung_Box_p_value) ~ "Residual diagnostics were unavailable because the residual series was too short or invalid.",
        Ljung_Box_p_value <= 0.05 ~ paste0(
          "The Ljung-Box test detected significant overall residual autocorrelation (p = ",
          formatC(Ljung_Box_p_value, format = "f", digits = 4), "). ",
          "Residual ACF pattern: ", Residual_ACF_pattern, "."
        ),
        Residual_ACF_acceptable == "Yes" & Residual_ACF_spike_count == 0 ~
          "The Ljung-Box test did not detect significant overall residual autocorrelation, and no significant residual ACF spikes were observed.",
        Residual_ACF_acceptable == "Yes" ~ paste0(
          "The Ljung-Box test did not detect significant overall residual autocorrelation. ",
          Residual_ACF_spike_count, " isolated ACF spike", if_else(Residual_ACF_spike_count == 1, " was", "s were"),
          " observed, but they did not form a consecutive pattern."
        ),
        TRUE ~ paste0(
          "The Ljung-Box test did not detect significant overall residual autocorrelation, but the residual ACF showed ",
          Residual_ACF_pattern, "."
        )
      )
    )
}
