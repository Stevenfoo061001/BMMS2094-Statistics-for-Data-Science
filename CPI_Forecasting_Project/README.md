# CPI Forecasting Project (R for Posit Cloud)

## Purpose

This R project forecasts Malaysia's monthly overall Consumer Price Index (CPI) for low-income households and relates the findings to **SDG 10: Reduced Inequalities**. Rising CPI may increase cost-of-living pressure for financially vulnerable households; forecasts may support the planning of targeted assistance, subsidies, and social protection. CPI does not directly measure income inequality, and households earning below RM3,000 are not automatically identical to Malaysia's bottom 40% population.

## Group allocation

| Group member | Forecasting model |
| --- | --- |
| Member 1 | Seasonal Naive baseline |
| Member 2 | Holt-Winters Exponential Smoothing |
| Member 3 | SARIMA(1,1,1)(1,1,1,12) |
| Member 4 | Time-series regression with linear trend and monthly seasonal effects |

Replace the placeholder labels with the four group members' names before submission. All models use the same chronological split: all observations except the final 12 months for training, and the final 12 months for testing.

## File structure

```text
CPI_Forecasting_Project/
|- cpi_2d_lowincome.csv
|- cpi_forecasting.R
|- CPI_Forecasting_Project.Rproj
|- README.md
`- outputs/
   |- figures/     # PNG charts produced by the R script
   `- tables/      # CSV tables produced by the R script
```

## Run in Posit Cloud

1. Create a new Posit Cloud project and upload the full `CPI_Forecasting_Project` folder.
2. Open `cpi_forecasting.R` and make the project folder your working directory.
3. In the R Console, install the packages once:

```r
install.packages(c("readr", "dplyr", "ggplot2", "forecast"))
```

4. Run the whole script using **Source** in Posit Cloud, or use the Terminal:

```bash
Rscript cpi_forecasting.R
```

The script uses only relative project paths, so no computer-specific file paths are required.

`cpi_forecasting.py` and `requirements.txt` are retained only as the earlier Python version; use `cpi_forecasting.R` for the Posit/R workflow.

## Generated outputs

`outputs/tables/` contains the prepared CPI dataset, descriptive statistics, training and testing datasets, test-period forecasts, and MAE/RMSE/MAPE accuracy metrics.

`outputs/figures/` contains 13 report-ready 300-DPI PNG charts: CPI, monthly change, year-on-year inflation, seasonality, decomposition, ACF/PACF, train-test split, four forecast plots, and the accuracy comparison.

## Troubleshooting

- **Package error:** run the `install.packages(...)` command above and rerun the script.
- **CSV not found:** keep `cpi_2d_lowincome.csv` in the same folder as `cpi_forecasting.R`.
- **Validation error:** read the printed message. The script stops if required columns are absent, values are missing, date-division records are duplicated, the `overall` series is absent, or a month is missing.
- **Different results:** use the unedited supplied CSV. The script never overwrites it.
