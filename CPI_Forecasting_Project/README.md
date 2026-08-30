# Malaysia Low-Income CPI Forecast (V2)

R/Posit Cloud group forecasting project for BMMS2094 Statistics for Data Science. It forecasts Malaysia's monthly **overall CPI index** for the low-income dataset and interprets the work as cost-of-living context for SDG 10.

`ResultV1/` is an archived version. Do not modify it or mix its models, figures, or numbers with V2. All current outputs are written to `output/`.

## Project structure

```text
CPI_Forecasting_Project/
|- data/                         # Original CSV and generated RDS inputs
|- scripts/                      # Run in numbered order below
|- output/                       # Current V2 CSV results and plots
|- ResultV1/                     # Archived V1 output — do not use in V2
|- CONTEXT.md
`- project.Rproj
```

## Method

- Target: `index` where `division == "overall"`.
- Frequency: monthly; seasonal period: 12.
- Final holdout: January 2025–June 2026 (18 months).
- Internal variant-validation period: January–December 2024 (12 months).
- Future forecast: July 2026–June 2027 (12 months).
- Accuracy measures: MAE, RMSE, MAPE, and MASE; lower values are better.

Members select a variant within their assigned family using only the internal validation period:

| Member | Model family | Script |
| --- | --- | --- |
| Huxley | Seasonal Naive | `02_Member_A_snaive.R` |
| Ooi Mei Yi | Exponential smoothing (Holt/Holt-Winters variants) | `03_Member_B_holt_winters.R` |
| Steven | ARIMA/SARIMA (manual candidates and automatic ARIMA) | `04_Member_C_sarima.R` |
| Tan Wei Ching | Time-series regression (trend/seasonal/quadratic variants) | `05_Member_D_tslm.R` |

A model is eligible for final selection only when its Ljung–Box p-value is above 0.05 and its residual ACF is acceptable. Up to three isolated significant ACF spikes are allowed; consecutive or more numerous spikes are not. The final model is the eligible model with the lowest **final-holdout RMSE**—never the lowest-RMSE model overall without diagnostics.

## Run order in Posit Cloud

Open `project.Rproj`, then run:

```r
source("scripts/01_data_prep.R")
source("scripts/02_Member_A_snaive.R")
source("scripts/03_Member_B_holt_winters.R")
source("scripts/04_Member_C_sarima.R")
source("scripts/05_Member_D_tslm.R")
source("scripts/06_group_comparison.R")
source("scripts/08_final_future_forecast.R")
```

The first script loads/install packages through `00_setup.R`. Script 06 stops clearly if no model is diagnostically eligible; in that case, do not run script 08 until additional variants have been tested.

## Main generated files

- `member_A_accuracy.csv` through `member_D_accuracy.csv`: selected variant, accuracy, full residual diagnostics, and factual diagnostic note for each member.
- `steven_sarima_candidate_results.csv` and `selected_model_specification.csv`: Steven's tested ARIMA/SARIMA orders and the actual selected ARIMA specification.
- `model_diagnostics_summary.csv`: all models with diagnostics and eligibility.
- `model_comparison_summary.csv`: accuracy ranking of all models.
- `eligible_model_comparison.csv` and `selected_final_model.csv`: final-selection ranking and selected eligible model.
- `model_comparison_rmse.png`: all-model RMSE chart, colour-coded by final eligibility.
- `future_cpi_forecast.csv` and `final_future_cpi_forecast.png`: 12-month estimate with 80% and 95% prediction intervals.

## SDG 10 framing

Rising CPI may increase cost-of-living pressure for financially vulnerable households. Forecasts may support targeted assistance, subsidies, and social-protection planning. CPI is not a direct measure of income inequality, and households earning below RM3,000 are not automatically identical to Malaysia's bottom 40% population.
