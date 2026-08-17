# Malaysia Low-Income CPI Forecast

R/Posit Cloud group forecasting project for BMMS2094 Statistics for Data Science. The project structure follows the same group-work pattern as the supplied Ozone Forecast reference: shared setup and preparation scripts, one model script per member, model-specific plots, and a group comparison script.

## Project structure

```text
CPI_Forecasting_Project/
|- data/
|  |- cpi_2d_lowincome.csv
|  `- generated .rds files after preparation
|- scripts/
|  |- 00_setup.R
|  |- 01_data_prep.R
|  |- 02_Member_A_snaive.R
|  |- 03_Member_B_holt_winters.R
|  |- 04_Member_C_sarima.R
|  |- 05_Member_D_tslm.R
|  `- 06_group_comparison.R
|- output/
|  |- plots/                 # model-specific PNG figures
|  `- model_comparison_summary.csv
|- CONTEXT.md
`- project.Rproj
```

## Group allocation

| Member | Forecasting model | Relative difficulty | Script |
| --- | --- | --- | --- |
| Huxley | Seasonal Naive baseline | Easiest | `02_Member_A_snaive.R` |
| Tan Wei Ching | Trend and monthly seasonal-dummy regression | Intermediate | `05_Member_D_tslm.R` |
| Ooi Mei Yi | Holt-Winters additive exponential smoothing | Advanced | `03_Member_B_holt_winters.R` |
| Steven | SARIMA(1,1,1)(1,1,1)[12] | Most advanced | `04_Member_C_sarima.R` |

The scripts and output labels already use your group members' names.

## Posit Cloud workflow

1. Upload the project folder to Posit Cloud and open `project.Rproj`.
2. Run `scripts/00_setup.R` once per session. It installs and loads `readr`, `dplyr`, `ggplot2`, `forecast`, and `tseries` if required.
3. Run the scripts in this order:

```r
source("scripts/01_data_prep.R")
source("scripts/02_Member_A_snaive.R")
source("scripts/03_Member_B_holt_winters.R")
source("scripts/04_Member_C_sarima.R")
source("scripts/05_Member_D_tslm.R")
source("scripts/06_group_comparison.R")
```

`01_data_prep.R` validates the original CSV, prepares the CPI series, and creates the common final-12-month holdout split. Each member script writes its own forecast accuracy table and 300-DPI plots. The final script combines all four members' MASE, RMSE, MAE, and MAPE values into `output/model_comparison_summary.csv`.

## SDG 10 framing

Rising CPI may intensify cost-of-living pressure for financially vulnerable households. Forecasts can support planning for targeted assistance, subsidies, and social protection. CPI is not a direct measure of income inequality, and households earning below RM3,000 are not automatically identical to Malaysia's bottom 40% population.
