# 08_final_future_forecast.R - refit the selected eligible model and forecast future CPI.

source("scripts/00_setup.R")
if (!file.exists("data/overall_cpi_prepared.rds")) source("scripts/01_data_prep.R")
if (!file.exists("output/selected_final_model.csv")) {
  stop("Run scripts 02-06 first. A future forecast requires output/selected_final_model.csv from the eligible-model selection.")
}

prepared <- readRDS("data/overall_cpi_prepared.rds")
winner <- read_csv("output/selected_final_model.csv", show_col_types = FALSE)
if (nrow(winner) != 1 || winner$Residuals_acceptable[[1]] != "Yes") {
  stop("selected_final_model.csv must contain exactly one diagnostically acceptable model.")
}

all_ts <- ts(prepared$index, start = c(prepared$year[1], prepared$month_number[1]), frequency = seasonal_period)

fit_final_model <- function(winner, series, horizon) {
  member <- winner$member[[1]]
  variant <- winner$selected_variant[[1]]

  if (member == "Huxley") return(snaive(series, h = horizon))

  if (member == "Tan Wei Ching") {
    candidates <- list(
      "Holt linear trend" = function() holt(series, damped = FALSE, h = horizon),
      "Damped Holt trend" = function() holt(series, damped = TRUE, h = horizon),
      "Holt-Winters additive" = function() hw(series, seasonal = "additive", damped = FALSE, h = horizon),
      "Damped Holt-Winters additive" = function() hw(series, seasonal = "additive", damped = TRUE, h = horizon)
    )
    if (!variant %in% names(candidates)) stop("Unrecognised exponential-smoothing variant: ", variant)
    return(candidates[[variant]]())
  }

  if (member == "Ooi Mei Yi") {
    required_order_fields <- c("p", "d", "q", "P", "D", "Q", "seasonal_period", "includes_drift", "includes_mean")
    if (!all(required_order_fields %in% names(winner))) {
      stop("Ooi Mei Yi's selected model is missing its saved ARIMA order; re-run scripts 04 and 06.")
    }
    order <- as.integer(unlist(winner[1, c("p", "d", "q")]))
    seasonal_order <- as.integer(unlist(winner[1, c("P", "D", "Q")]))
    period <- as.integer(winner$seasonal_period[[1]])
    include_drift <- identical(winner$includes_drift[[1]], "Yes")
    include_mean <- identical(winner$includes_mean[[1]], "Yes")
    refitted <- forecast::Arima(
      series,
      order = order,
      seasonal = list(order = seasonal_order, period = period),
      include.drift = include_drift,
      include.mean = include_mean
    )
    return(forecast::forecast(refitted, h = horizon))
  }

  if (member == "Steven") {
    if (variant != "TBATS (annual seasonality = 12)") {
      stop("Unrecognised TBATS variant: ", variant)
    }
    model <- tbats(series, seasonal.periods = seasonal_period, use.box.cox = NULL)
    return(forecast(model, h = horizon))
  }

  stop("The selected model is not recognised by the final forecasting script.")
}

final_forecast <- fit_final_model(winner, all_ts, future_h)
future_dates <- seq(max(prepared$date), by = "month", length.out = future_h + 1)[-1]
future_output <- tibble(
  date = future_dates,
  point_forecast = as.numeric(final_forecast$mean),
  lower_80 = as.numeric(final_forecast$lower[, "80%"]),
  upper_80 = as.numeric(final_forecast$upper[, "80%"]),
  lower_95 = as.numeric(final_forecast$lower[, "95%"]),
  upper_95 = as.numeric(final_forecast$upper[, "95%"]),
  selected_member = winner$member[[1]],
  model_family = winner$model_family[[1]],
  model_specification = winner$model_specification[[1]],
  forecast_note = "Forecast values are estimates, not guaranteed future outcomes."
)
write_csv(future_output, "output/future_cpi_forecast.csv")

final_plot <- autoplot(final_forecast) +
  labs(
    title = paste("Future CPI Forecast:", winner$model_specification[[1]]),
    subtitle = paste("Selected eligible model refitted on observations through", format(max(prepared$date), "%B %Y")),
    x = "Year", y = "CPI index"
  ) +
  theme_minimal()
save_plot(final_plot, "output/final_future_cpi_forecast.png")

print(winner %>% select(member, model_family, model_specification, RMSE, Diagnostic_note))
print(future_output)
