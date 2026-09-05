# CPI Forecasting Project Context (V2)

## Objective

Forecast Malaysia's monthly overall Consumer Price Index for low-income households and interpret the analysis in relation to SDG 10: Reduced Inequalities.

## Scope and data rules

- Target: the `index` value where `division == "overall"`.
- Frequency: monthly; seasonal period: 12.
- Original source: `data/cpi_2d_lowincome.csv`; never edit it in place.
- `ResultV1/` is archived. V2 analysis uses only `data/`, `scripts/`, and `output/`.

## Evaluation and selection rules

- Final test period: January 2025–June 2026 (18 months), held out chronologically.
- Internal validation: January–December 2024 (12 months), used only to choose variants inside a member's assigned family.
- Future forecast horizon: July 2026–June 2027 (12 months).
- Accuracy: MASE, RMSE, MAE, and MAPE; lower is better.
- The Ljung–Box p-value must be above 0.05 for residuals to be acceptable.
- Up to three isolated residual-ACF spikes are acceptable. Consecutive spikes or more than three spikes are unacceptable.
- Training-versus-final-test RMSE/MAPE is recorded as an overfitting check. A test-to-training RMSE ratio of 1.5 or above makes a model ineligible for final selection.
- The final model is the lowest-final-holdout-RMSE model among models passing the final Ljung–Box, residual-ACF, and overfitting checks. Ooi Mei Yi's candidate selection also applies the overfitting check during internal validation.

## Group allocation

| Member | Script | Model family | Variant selection |
| --- | --- | --- | --- |
| Huxley | `02_Member_A_snaive.R` | Seasonal Naive | Seasonal Naive baseline |
| Tan Wei Ching | `03_Member_B_holt_winters.R` | Exponential smoothing | Holt and Holt-Winters variants |
| Ooi Mei Yi | `04_Member_C_sarima.R` | ARIMA/SARIMA | Manual, automatic, and Fourier-regression candidates |
| Steven | `05_Member_D_tbats.R` | TBATS | Annual seasonality = 12 |

Each script selects its variant with the internal validation period, records the selected specification, then evaluates once on the untouched final holdout. Ooi Mei Yi's output records the actual automatic ARIMA order, including seasonal terms, drift/mean settings, and Fourier order, so it can be reproduced.

## Run order and outputs

Run `01_data_prep.R`, member scripts `02` through `05`, `06_group_comparison.R`, `08_final_future_forecast.R`, then `09_lecture_support.R`. The four core models are Seasonal Naive, exponential smoothing, ARIMA/SARIMA, and TBATS. The group comparison writes all-model and eligible-only rankings plus `selected_final_model.csv`. The final forecast script reads that selected eligible model; it never picks a model only because it has the lowest RMSE. The lecture-support script writes an STL decomposition, COVID-period context figures, a model-summary table, and a concise lecture brief without changing the data or model results.

## SDG 10 framing

Rising CPI may intensify cost-of-living pressure for financially vulnerable households. Forecasts can support planning for targeted assistance, subsidies, and social protection. CPI is not a direct measure of income inequality, and households earning below RM3,000 are not automatically identical to Malaysia's bottom 40% population.
