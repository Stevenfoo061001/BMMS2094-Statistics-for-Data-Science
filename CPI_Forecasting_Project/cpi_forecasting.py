"""Forecast Malaysia's low-income household CPI using four time-series methods.

Run from this folder with: python3 cpi_forecasting.py
"""

from pathlib import Path
import sys
import warnings

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from sklearn.linear_model import LinearRegression
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from statsmodels.tsa.holtwinters import ExponentialSmoothing
from statsmodels.tsa.seasonal import seasonal_decompose
from statsmodels.tsa.statespace.sarimax import SARIMAX


PROJECT_DIR = Path(__file__).resolve().parent
DATA_FILE = PROJECT_DIR / "cpi_2d_lowincome.csv"
FIGURES_DIR = PROJECT_DIR / "outputs" / "figures"
TABLES_DIR = PROJECT_DIR / "outputs" / "tables"
FORECAST_HORIZON = 12
SEASONAL_PERIOD = 12
COLOUR = "#1f5a99"
MONTH_ORDER = list(range(1, 13))
MONTH_LABELS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]


def setup_output_folders():
    """Create the output folders when they are not already present."""
    FIGURES_DIR.mkdir(parents=True, exist_ok=True)
    TABLES_DIR.mkdir(parents=True, exist_ok=True)


def save_figure(filename):
    """Apply consistent figure saving and release the figure from memory."""
    plt.tight_layout()
    plt.savefig(FIGURES_DIR / filename, dpi=300, bbox_inches="tight")
    plt.close()


def fail(message):
    """Stop the program with an actionable validation message."""
    raise ValueError(f"DATA VALIDATION FAILED: {message}")


def load_and_validate_data():
    """Load the CSV and validate the structure needed for forecasting."""
    if not DATA_FILE.exists():
        fail(f"Cannot find '{DATA_FILE.name}'. Place it beside cpi_forecasting.py and run again.")

    data = pd.read_csv(DATA_FILE)
    print("\n--- Raw dataset preview ---")
    print(f"Shape: {data.shape}")
    print(f"Columns: {data.columns.tolist()}")
    print("Data types before conversion:")
    print(data.dtypes)
    print("First five rows:")
    print(data.head().to_string(index=False))

    required_columns = {"date", "division", "index"}
    missing_columns = required_columns.difference(data.columns)
    if missing_columns:
        fail(f"Essential column(s) missing: {sorted(missing_columns)}")

    try:
        data["date"] = pd.to_datetime(data["date"], errors="raise")
    except (ValueError, TypeError) as error:
        fail(f"The date column cannot be converted to dates: {error}")
    try:
        data["index"] = pd.to_numeric(data["index"], errors="raise")
    except (ValueError, TypeError) as error:
        fail(f"The index column is not numeric: {error}")

    missing_values = data.isna().sum()
    duplicate_records = data.duplicated(subset=["date", "division"]).sum()
    if missing_values.any():
        fail(f"Missing values found: {missing_values[missing_values > 0].to_dict()}")
    if duplicate_records:
        fail(f"Found {duplicate_records} duplicate date-division record(s).")

    divisions = sorted(data["division"].astype(str).unique())
    if "overall" not in divisions:
        fail("The required division 'overall' is not available.")

    overall = data.loc[data["division"] == "overall"].copy().sort_values("date")
    if overall.empty:
        fail("The overall CPI series is empty.")
    expected_dates = pd.date_range(overall["date"].min(), overall["date"].max(), freq="MS")
    missing_months = expected_dates.difference(pd.DatetimeIndex(overall["date"]))
    if len(missing_months):
        fail("Missing month(s) in overall CPI: " + ", ".join(date.strftime("%Y-%m") for date in missing_months))
    if len(overall) <= FORECAST_HORIZON + SEASONAL_PERIOD:
        fail("The overall series is too short for a seasonal model and 12-month test set.")

    print("\n--- Validation summary ---")
    print(f"Missing values: {int(missing_values.sum())}")
    print(f"Duplicate date-division combinations: {duplicate_records}")
    print(f"Available divisions ({len(divisions)}): {divisions}")
    print(f"Overall CPI observations: {len(overall)}")
    print(f"Overall CPI date range: {overall['date'].min():%B %Y} to {overall['date'].max():%B %Y}")
    print(f"Overall CPI range: {overall['index'].min():.1f} to {overall['index'].max():.1f}")
    print(f"Missing months in overall CPI: {len(missing_months)}")
    print("Result: all essential validation checks passed.")
    return data, overall


def prepare_data(overall):
    """Add descriptive time-series variables without changing the original CSV."""
    prepared = overall.copy()
    prepared["monthly_change_pct"] = prepared["index"].pct_change() * 100
    prepared["yoy_inflation_pct"] = prepared["index"].pct_change(12) * 100
    prepared["year"] = prepared["date"].dt.year
    prepared["month_number"] = prepared["date"].dt.month
    prepared["month_name"] = prepared["date"].dt.month_name()
    prepared.to_csv(TABLES_DIR / "cleaned_overall_cpi.csv", index=False)

    descriptive = prepared[["index", "monthly_change_pct", "yoy_inflation_pct"]].describe().T
    descriptive.to_csv(TABLES_DIR / "descriptive_statistics.csv")
    return prepared


def print_interpretations(prepared):
    """Print short interpretations calculated from the current data."""
    latest = prepared.iloc[-1]
    max_yoy = prepared.loc[prepared["yoy_inflation_pct"].idxmax()]
    min_yoy = prepared.loc[prepared["yoy_inflation_pct"].idxmin()]
    print("\n--- Factual descriptive interpretations ---")
    print(f"Latest CPI is {latest['index']:.1f} in {latest['date']:%B %Y}.")
    print(f"Latest monthly change is {latest['monthly_change_pct']:.2f}% and latest year-on-year inflation is {latest['yoy_inflation_pct']:.2f}%.")
    print(f"Highest year-on-year inflation is {max_yoy['yoy_inflation_pct']:.2f}% in {max_yoy['date']:%B %Y}.")
    print(f"Lowest year-on-year inflation is {min_yoy['yoy_inflation_pct']:.2f}% in {min_yoy['date']:%B %Y}.")


def create_exploratory_figures(prepared):
    """Create the required descriptive and diagnostic figures."""
    sns.set_theme(style="whitegrid", context="notebook")

    plt.figure(figsize=(11, 5))
    plt.plot(prepared["date"], prepared["index"], color=COLOUR, linewidth=2)
    plt.title("Malaysia Low-Income Household Overall CPI")
    plt.xlabel("Date")
    plt.ylabel("CPI index")
    plt.xticks(rotation=35)
    save_figure("01_overall_cpi_line.png")

    plt.figure(figsize=(11, 5))
    plt.plot(prepared["date"], prepared["monthly_change_pct"], color="#d95f02", linewidth=1.5)
    plt.axhline(0, color="black", linewidth=0.8)
    plt.title("Monthly Percentage Change in Overall CPI")
    plt.xlabel("Date")
    plt.ylabel("Monthly change (%)")
    plt.xticks(rotation=35)
    save_figure("02_monthly_percentage_change.png")

    plt.figure(figsize=(11, 5))
    plt.plot(prepared["date"], prepared["yoy_inflation_pct"], color="#7570b3", linewidth=1.5)
    plt.axhline(0, color="black", linewidth=0.8)
    plt.title("Year-on-Year Inflation in Overall CPI")
    plt.xlabel("Date")
    plt.ylabel("Year-on-year inflation (%)")
    plt.xticks(rotation=35)
    save_figure("03_yoy_inflation.png")

    plt.figure(figsize=(11, 5))
    sns.boxplot(data=prepared, x="month_number", y="index", order=MONTH_ORDER, color="#8dd3c7")
    plt.title("Seasonal Distribution of Overall CPI by Calendar Month")
    plt.xlabel("Calendar month")
    plt.ylabel("CPI index")
    plt.xticks(ticks=np.arange(12), labels=MONTH_LABELS)
    save_figure("04_seasonal_cpi_boxplot.png")

    decomposition = seasonal_decompose(prepared.set_index("date")["index"], model="additive", period=SEASONAL_PERIOD)
    figure = decomposition.plot()
    figure.set_size_inches(11, 8)
    figure.suptitle("Additive Seasonal-Trend Decomposition of Overall CPI", y=1.02)
    figure.axes[-1].set_xlabel("Date")
    figure.axes[0].set_ylabel("CPI index")
    save_figure("05_seasonal_trend_decomposition.png")

    plt.figure(figsize=(10, 5))
    plot_acf(prepared["index"], lags=min(24, len(prepared) - 1), ax=plt.gca())
    plt.title("Autocorrelation Function of Overall CPI")
    plt.xlabel("Lag (months)")
    plt.ylabel("Autocorrelation")
    save_figure("06_acf.png")

    pacf_lags = min(24, (len(prepared) // 2) - 1)
    plt.figure(figsize=(10, 5))
    plot_pacf(prepared["index"], lags=pacf_lags, method="ywm", ax=plt.gca())
    plt.title("Partial Autocorrelation Function of Overall CPI")
    plt.xlabel("Lag (months)")
    plt.ylabel("Partial autocorrelation")
    save_figure("07_pacf.png")


def split_data(prepared):
    """Create the required chronological final-12-month holdout set."""
    training = prepared.iloc[:-FORECAST_HORIZON].copy()
    testing = prepared.iloc[-FORECAST_HORIZON:].copy()
    training.to_csv(TABLES_DIR / "training_data.csv", index=False)
    testing.to_csv(TABLES_DIR / "testing_data.csv", index=False)
    print("\n--- Chronological train-test split ---")
    print(f"Training: {len(training)} observations, {training['date'].min():%Y-%m} to {training['date'].max():%Y-%m}")
    print(f"Testing: {len(testing)} observations, {testing['date'].min():%Y-%m} to {testing['date'].max():%Y-%m}")

    plt.figure(figsize=(11, 5))
    plt.plot(training["date"], training["index"], label="Training", color=COLOUR, linewidth=2)
    plt.plot(testing["date"], testing["index"], label="Testing", color="#e7298a", linewidth=2)
    plt.axvline(testing["date"].iloc[0], color="black", linestyle="--", linewidth=1, label="Split point")
    plt.title("Chronological Training and Testing Periods")
    plt.xlabel("Date")
    plt.ylabel("CPI index")
    plt.legend()
    plt.xticks(rotation=35)
    save_figure("08_training_testing_split.png")
    return training, testing


def seasonal_naive_forecast(train_values, horizon):
    """Forecast each month using its observed value one seasonal period earlier."""
    return np.resize(np.asarray(train_values.iloc[-SEASONAL_PERIOD:], dtype=float), horizon)


def holt_winters_forecast(train_values, horizon):
    model = ExponentialSmoothing(train_values, trend="add", seasonal="add", seasonal_periods=SEASONAL_PERIOD)
    return np.asarray(model.fit(optimized=True).forecast(horizon), dtype=float)


def sarima_forecast(train_values, horizon):
    model = SARIMAX(
        train_values,
        order=(1, 1, 1),
        seasonal_order=(1, 1, 1, SEASONAL_PERIOD),
        enforce_stationarity=False,
        enforce_invertibility=False,
    )
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        fitted = model.fit(disp=False)
    return np.asarray(fitted.forecast(horizon), dtype=float)


def regression_design(dates, trend_start=0):
    """Build a trend plus monthly-dummy design matrix with a stable set of columns."""
    frame = pd.DataFrame({"trend": np.arange(trend_start, trend_start + len(dates)), "month": dates.dt.month.to_numpy()})
    dummies = pd.get_dummies(frame["month"], prefix="month", dtype=float).reindex(
        columns=[f"month_{month}" for month in MONTH_ORDER], fill_value=0.0
    )
    return pd.concat([frame[["trend"]], dummies.iloc[:, 1:]], axis=1)


def regression_forecast(training, testing):
    """Fit linear trend plus 11 monthly seasonal dummies and forecast the test months."""
    model = LinearRegression()
    train_x = regression_design(training["date"], trend_start=0)
    test_x = regression_design(testing["date"], trend_start=len(training))
    model.fit(train_x, training["index"])
    return np.asarray(model.predict(test_x), dtype=float)


def calculate_metrics(actual, predicted):
    actual = np.asarray(actual, dtype=float)
    predicted = np.asarray(predicted, dtype=float)
    errors = actual - predicted
    return {
        "MAE": np.mean(np.abs(errors)),
        "RMSE": np.sqrt(np.mean(errors ** 2)),
        "MAPE (%)": np.mean(np.abs(errors / actual)) * 100,
    }


def run_forecasting_models(training, testing):
    """Fit the four member models, save comparisons, and create report-ready forecast plots."""
    # Setting the frequency explicitly avoids treating an otherwise regular
    # monthly index as an undated sequence during model fitting.
    train_values = training.set_index("date")["index"].asfreq("MS")
    forecasts = {
        "Seasonal Naive": seasonal_naive_forecast(train_values, len(testing)),
        "Holt-Winters": holt_winters_forecast(train_values, len(testing)),
        "SARIMA": sarima_forecast(train_values, len(testing)),
        "Trend + Monthly Dummies": regression_forecast(training, testing),
    }
    results = testing[["date", "index"]].rename(columns={"index": "actual_cpi"}).copy()
    metric_rows = []
    for name, forecast in forecasts.items():
        column = name.lower().replace(" ", "_").replace("+", "plus").replace("-", "_")
        results[column] = forecast
        metric_rows.append({"model": name, **calculate_metrics(testing["index"], forecast)})

        plt.figure(figsize=(11, 5))
        plt.plot(training["date"], training["index"], label="Training", color="#bdbdbd", linewidth=1.5)
        plt.plot(testing["date"], testing["index"], label="Actual test CPI", color="#e7298a", marker="o", linewidth=2)
        plt.plot(testing["date"], forecast, label=f"{name} forecast", color=COLOUR, marker="o", linestyle="--", linewidth=2)
        plt.title(f"{name}: 12-Month CPI Forecast")
        plt.xlabel("Date")
        plt.ylabel("CPI index")
        plt.legend()
        plt.xticks(rotation=35)
        safe_name = column.replace("__", "_")
        save_figure(f"09_forecast_{safe_name}.png")

    metrics = pd.DataFrame(metric_rows).sort_values("RMSE").reset_index(drop=True)
    results.to_csv(TABLES_DIR / "test_forecasts.csv", index=False)
    metrics.to_csv(TABLES_DIR / "forecast_accuracy_metrics.csv", index=False)
    print("\n--- Forecast accuracy on the final 12 months ---")
    print(metrics.to_string(index=False, float_format=lambda value: f"{value:.3f}"))
    print(f"Lowest RMSE model: {metrics.iloc[0]['model']} ({metrics.iloc[0]['RMSE']:.3f}).")

    plt.figure(figsize=(10, 5))
    sns.barplot(data=metrics, x="RMSE", y="model", hue="model", palette="Blues_d", legend=False)
    plt.title("Forecast Accuracy Comparison: Lower RMSE Is Better")
    plt.xlabel("RMSE (CPI index points)")
    plt.ylabel("Forecasting model")
    save_figure("10_forecast_accuracy_comparison.png")


def main():
    print("Forecasting Malaysia's Monthly Consumer Price Index for Low-Income Households")
    setup_output_folders()
    _, overall = load_and_validate_data()
    prepared = prepare_data(overall)
    print_interpretations(prepared)
    create_exploratory_figures(prepared)
    training, testing = split_data(prepared)
    run_forecasting_models(training, testing)
    print("\nCompleted successfully. Tables and figures are in outputs/.")


if __name__ == "__main__":
    try:
        main()
    except (ValueError, FileNotFoundError) as error:
        print(f"\nERROR: {error}", file=sys.stderr)
        sys.exit(1)
