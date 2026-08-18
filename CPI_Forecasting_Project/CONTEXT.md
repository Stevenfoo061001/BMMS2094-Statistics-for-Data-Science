# CPI Forecasting Project Context

## Objective

Forecast Malaysia's monthly overall Consumer Price Index for low-income households and interpret the analysis in relation to SDG 10: Reduced Inequalities.

## Shared modelling rules

- Target: the `index` value for `division == "overall"`.
- Frequency: monthly; seasonal period: 12 months.
- Split: chronological, with the final 12 months as the test set. No random shuffling.
- Common evaluation: MASE, RMSE, MAE, and MAPE. Lower values are better.
- Data source file: `data/cpi_2d_lowincome.csv`; never edit it in place.

## Group allocation

| Member | Script | Model | Relative difficulty |
| --- | --- | --- | --- |
| Huxley | `02_Member_A_snaive.R` | Seasonal Naive baseline | Easiest |
| Tan Wei Ching | `05_Member_D_tslm.R` | Trend plus monthly seasonal dummy regression | Intermediate |
| Ooi Mei Yi | `03_Member_B_holt_winters.R` | Holt-Winters additive exponential smoothing | Advanced |
| Steven | `04_Member_C_sarima.R` | SARIMA(1,1,1)(1,1,1)[12] | Most advanced |

The output directories retain Member A-D labels to match the reference project format; chart titles and accuracy tables use the group members' real names.

## SARIMA diagnostic refinement

If Steven's residual ACF has spikes beyond its confidence limits or the Ljung-Box p-value is below 0.05, run `scripts/07_Steven_sarima_testing.R`. It tests several seasonal ARIMA alternatives and compares their residual diagnostics and final-12-month accuracy. Do not overwrite the main SARIMA model unless the group approves the candidate.
