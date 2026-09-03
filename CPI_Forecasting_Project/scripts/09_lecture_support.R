# 09_lecture_support.R - create lecture-ready explanatory outputs.
# Run after scripts 01-06 and 08. It does not change the data or any model.

source("scripts/00_setup.R")

required_files <- c(
  "data/training_data.rds",
  "output/model_comparison_summary.csv",
  "output/eligible_model_comparison.csv",
  "output/selected_final_model.csv",
  "output/future_cpi_forecast.csv"
)
if (!all(file.exists(required_files))) {
  stop("Run scripts 01-06 and 08 before creating lecture-support outputs.")
}

train <- readRDS("data/training_data.rds")
comparison <- read_csv("output/model_comparison_summary.csv", show_col_types = FALSE)
winner <- read_csv("output/selected_final_model.csv", show_col_types = FALSE)
future <- read_csv("output/future_cpi_forecast.csv", show_col_types = FALSE)
out <- "output/plots/lecture_support"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

train_ts <- ts(train$index,
               start = c(train$year[1], train$month_number[1]),
               frequency = seasonal_period)

# STL separates the observed CPI into its trend, repeating annual seasonal
# pattern, and remainder. Robust fitting reduces the influence of unusual
# observations; it does not replace or alter the original COVID-period data.
stl_fit <- stl(train_ts, s.window = "periodic", robust = TRUE)
png(file.path(out, "Lecture_01_stl_decomposition.png"), width = 3300, height = 3000, res = 300)
plot(stl_fit)
title(main = "STL Decomposition of Monthly Overall CPI (Training Data)")
dev.off()

covid_start <- as.Date("2020-03-01")
covid_end <- as.Date("2021-12-01")
covid_plot <- ggplot(train, aes(date, index)) +
  annotate("rect", xmin = covid_start, xmax = covid_end, ymin = -Inf, ymax = Inf,
           fill = "#FEE8C8", alpha = 0.45) +
  geom_line(linewidth = 0.75, colour = "#1F4E79") +
  geom_point(size = 1.25, colour = "#1F4E79") +
  annotate("text", x = as.Date("2020-12-01"), y = max(train$index),
           label = "COVID-19 period", vjust = -0.7, colour = "#8C2D04", size = 4) +
  labs(
    title = "Monthly Overall CPI with COVID-19 Period Marked",
    subtitle = "The shaded period is contextual only; all observed CPI values are retained unchanged",
    x = "Date", y = "CPI index"
  ) +
  theme_minimal(base_size = 12)
save_plot(covid_plot, file.path(out, "Lecture_02_covid_context.png"))

stl_components <- as.data.frame(stl_fit$time.series) %>%
  mutate(date = seq(min(train$date), by = "month", length.out = n()))
remainder_plot <- ggplot(stl_components, aes(date, remainder)) +
  annotate("rect", xmin = covid_start, xmax = covid_end, ymin = -Inf, ymax = Inf,
           fill = "#FEE8C8", alpha = 0.45) +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = "#555555") +
  geom_col(fill = "#6A51A3", width = 20) +
  labs(
    title = "STL Remainder: Irregular CPI Movements",
    subtitle = "Large deviations during the shaded COVID-19 period are retained as real observed variation",
    x = "Date", y = "Remainder (CPI index points)"
  ) +
  theme_minimal(base_size = 12)
save_plot(remainder_plot, file.path(out, "Lecture_03_stl_remainder.png"))

lecture_summary <- comparison %>%
  mutate(
    Holdout_rank = row_number(),
    Final_selection_status = if_else(model_specification == winner$model_specification[[1]],
                                     "Selected final model", Final_eligible)
  ) %>%
  select(Holdout_rank, member, model_family, model_specification,
         MAE, RMSE, MAPE, MASE, Ljung_Box_p_value,
         Residual_ACF_pattern, Residuals_acceptable, Final_selection_status,
         Overfitting_assessment)
write_csv(lecture_summary, "output/lecturer_model_summary.csv")

winner_row <- lecture_summary %>% filter(Final_selection_status == "Selected final model")
future_start <- min(as.Date(future$date))
future_end <- max(as.Date(future$date))
brief_lines <- c(
  "# Malaysia Overall CPI Forecast: Lecture Brief",
  "",
  "## Main conclusion",
  "",
  sprintf("TBATS was selected as the final model because it had the lowest holdout RMSE among models passing the residual checks (RMSE = %.3f).", winner_row$RMSE[[1]]),
  "",
  "## What was forecast",
  "",
  "- Target: Malaysia monthly overall CPI index for the low-income dataset.",
  "- Training period: January 2010 to December 2024.",
  "- Untouched final holdout: January 2025 to June 2026 (18 months).",
  sprintf("- Future forecast: %s to %s (12 months).", format(future_start, "%B %Y"), format(future_end, "%B %Y")),
  "",
  "## Why TBATS was selected",
  "",
  sprintf("- TBATS holdout RMSE: %.3f; MAE: %.3f; MAPE: %.3f%%.", winner_row$RMSE[[1]], winner_row$MAE[[1]], winner_row$MAPE[[1]]),
  sprintf("- Ljung-Box p-value: %.3f, so residual autocorrelation was not detected at the 5%% level.", winner_row$Ljung_Box_p_value[[1]]),
  sprintf("- Residual ACF result: %s.", winner_row$Residual_ACF_pattern[[1]]),
  "- The final selection used both holdout accuracy and residual diagnostics; RMSE alone was not enough.",
  "",
  "## COVID-19 interpretation",
  "",
  "- The STL decomposition separates the CPI into trend, annual seasonal movement, and an irregular remainder.",
  "- Large COVID-period remainder movements are treated as real economic observations, not missing data.",
  "- No smoothing or imputation was applied, because that could remove genuine pandemic-era variation and overstate forecast performance.",
  "",
  "## Important caveat",
  "",
  "- TBATS had a validation-period warning before the final holdout. This is reported transparently, while the final choice remains supported by its strong untouched-holdout accuracy and acceptable residual diagnostics.",
  "- CPI indicates cost-of-living pressure; it is not itself a direct measure of income inequality or the bottom 40% population.",
  "",
  "## Files to show during the lecture",
  "",
  "1. `plots/lecture_support/Lecture_01_stl_decomposition.png` — trend, seasonality, and irregular remainder.",
  "2. `plots/lecture_support/Lecture_02_covid_context.png` — original CPI with the COVID-19 period marked.",
  "3. `plots/lecture_support/Lecture_03_stl_remainder.png` — irregular movements, including the COVID period.",
  "4. `model_comparison_rmse.png` — four-model holdout comparison and eligibility.",
  "5. `final_future_cpi_forecast.png` — selected TBATS forecast with 80% and 95% intervals.",
  "6. `lecturer_model_summary.csv` — exact metrics and diagnostics for reference."
)
writeLines(brief_lines, "output/LECTURER_BRIEF.md")

print(lecture_summary)
cat("\nLecture-support files created in output/ and output/plots/lecture_support/.\n")
