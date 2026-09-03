# Malaysia Overall CPI Forecast: Lecture Brief

## Main conclusion

TBATS was selected as the final model because it had the lowest holdout RMSE among models passing the residual checks (RMSE = 0.254).

## What was forecast

- Target: Malaysia monthly overall CPI index for the low-income dataset.
- Training period: January 2010 to December 2024.
- Untouched final holdout: January 2025 to June 2026 (18 months).
- Future forecast: July 2026 to June 2027 (12 months).

## Why TBATS was selected

- TBATS holdout RMSE: 0.254; MAE: 0.225; MAPE: 0.166%.
- Ljung-Box p-value: 0.071, so residual autocorrelation was not detected at the 5% level.
- Residual ACF result: Few isolated spikes.
- The final selection used both holdout accuracy and residual diagnostics; RMSE alone was not enough.

## COVID-19 interpretation

- The STL decomposition separates the CPI into trend, annual seasonal movement, and an irregular remainder.
- Large COVID-period remainder movements are treated as real economic observations, not missing data.
- No smoothing or imputation was applied, because that could remove genuine pandemic-era variation and overstate forecast performance.

## Important caveat

- TBATS had a validation-period warning before the final holdout. This is reported transparently, while the final choice remains supported by its strong untouched-holdout accuracy and acceptable residual diagnostics.
- CPI indicates cost-of-living pressure; it is not itself a direct measure of income inequality or the bottom 40% population.

## Files to show during the lecture

1. `plots/lecture_support/Lecture_01_stl_decomposition.png` — trend, seasonality, and irregular remainder.
2. `plots/lecture_support/Lecture_02_covid_context.png` — original CPI with the COVID-19 period marked.
3. `plots/lecture_support/Lecture_03_stl_remainder.png` — irregular movements, including the COVID period.
4. `model_comparison_rmse.png` — four-model holdout comparison and eligibility.
5. `final_future_cpi_forecast.png` — selected TBATS forecast with 80% and 95% intervals.
6. `lecturer_model_summary.csv` — exact metrics and diagnostics for reference.
