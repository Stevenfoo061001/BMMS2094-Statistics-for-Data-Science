# 08_final_future_forecast.R - refit the selected winner and forecast future CPI.

source("scripts/00_setup.R")
if (!file.exists("data/overall_cpi_prepared.rds")) source("scripts/01_data_prep.R")
if (!file.exists("output/model_comparison_summary.csv")) {
  stop("Run scripts 02-06 before creating the final future forecast.")
}

prepared <- readRDS("data/overall_cpi_prepared.rds")
winner <- read_csv("output/model_comparison_summary.csv", show_col_types = FALSE) %>% slice_min(RMSE, n = 1, with_ties = FALSE)
all_ts <- ts(prepared$index, start = c(prepared$year[1], prepared$month_number[1]), frequency = seasonal_period)

fit_final_model <- function(member, model, series, horizon) {
  if (member == "Huxley") return(snaive(series, h = horizon))

  if (member == "Ooi Mei Yi") {
    candidates <- list(
      "Holt linear trend" = function() holt(series, damped = FALSE, h = horizon),
      "Damped Holt trend" = function() holt(series, damped = TRUE, h = horizon),
      "Holt-Winters additive" = function() hw(series, seasonal = "additive", damped = FALSE, h = horizon),
      "Damped Holt-Winters additive" = function() hw(series, seasonal = "additive", damped = TRUE, h = horizon)
    )
    return(candidates[[model]]())
  }

  if (member == "Steven") {
    candidates <- list(
      "SARIMA(1,1,1)(1,1,1)[12]" = function() Arima(series, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 12)),
      "SARIMA(0,1,1)(0,1,1)[12]" = function() Arima(series, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)),
      "SARIMA(1,1,0)(1,1,0)[12]" = function() Arima(series, order = c(1, 1, 0), seasonal = list(order = c(1, 1, 0), period = 12)),
      "SARIMA(1,1,1)(1,1,0)[12]" = function() Arima(series, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 0), period = 12)),
      "SARIMA(1,1,0)(0,1,1)[12]" = function() Arima(series, order = c(1, 1, 0), seasonal = list(order = c(0, 1, 1), period = 12)),
      "auto.arima selected model" = function() auto.arima(series, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
    )
    return(forecast(candidates[[model]](), h = horizon))
  }

  if (member == "Tan Wei Ching") {
    candidates <- list(
      "Trend only" = function() tslm(series ~ trend),
      "Trend + monthly dummies" = function() tslm(series ~ trend + season),
      "Quadratic trend + monthly dummies" = function() tslm(series ~ trend + I(trend^2) + season)
    )
    return(forecast(candidates[[model]](), h = horizon))
  }

  stop("The selected model is not recognised by the final forecasting script.")
}

final_forecast <- fit_final_model(winner$member, winner$model, all_ts, future_h)
future_dates <- seq(max(prepared$date), by = "month", length.out = future_h + 1)[-1]
future_output <- tibble(
  date = future_dates,
  forecast_cpi = as.numeric(final_forecast$mean),
  lower_80 = as.numeric(final_forecast$lower[, "80%"]),
  upper_80 = as.numeric(final_forecast$upper[, "80%"]),
  lower_95 = as.numeric(final_forecast$lower[, "95%"]),
  upper_95 = as.numeric(final_forecast$upper[, "95%"]),
  selected_model = winner$model
)
write_csv(future_output, "output/future_cpi_forecast.csv")

final_plot <- autoplot(final_forecast) +
  labs(
    title = paste("Future CPI Forecast:", winner$model),
    subtitle = paste("Model refitted on all observed data through", format(max(prepared$date), "%B %Y")),
    x = "Year",
    y = "CPI index"
  ) +
  theme_minimal()
save_plot(final_plot, "output/final_future_cpi_forecast.png")

print(winner)
print(future_output)
