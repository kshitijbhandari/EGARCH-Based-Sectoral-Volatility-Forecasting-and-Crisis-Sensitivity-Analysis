# Time Series Forecasting Project

This project analyzes Indian equity sector indices using classical time-series methods and volatility models in R. The main script, `sector_volatility_analysis.R`, studies monthly return behavior and daily volatility for five sector indices:

- NIFTY BANK
- NIFTY IT
- NIFTY PHARMA
- NIFTY FMCG
- NIFTY AUTO

## What the script does

The analysis is organized in stages:

1. Load index CSV data and sort by date.
2. Split data into training and test sets using the last 365 days as the test period.
3. Compute daily log returns.
4. Aggregate daily returns into monthly log returns.
5. Examine seasonality using month plots, periodograms, and the Friedman test.
6. Check stationarity with ADF, PP, and KPSS tests.
7. Fit and compare simple ARIMA models, then select an automatic ARIMA model using BIC.
8. Fit SES and Holt models and inspect residual diagnostics.
9. Compute rolling volatility across sectors.
10. Fit and compare `GARCH(1,1)` and `EGARCH(1,1)` models.
11. Summarize volatility persistence and asymmetry across sectors.
12. Evaluate rolling one-step-ahead EGARCH volatility forecasts against realized volatility.

## Main file

- `sector_volatility_analysis.R`: full end-to-end analysis script.

## Data

The repository does not include the raw CSV data anymore. To run the analysis, create an `index_data/` folder in the project root and place the sector CSV files there.

The script currently expects:

- `NIFTY BANK.csv`
- `NIFTY IT.csv`
- `NIFTY PHARMA.csv`
- `NIFTY FMCG.csv`
- `NIFTY AUTO.csv`

The script assumes each CSV contains at least these columns:

- `Date`
- `Close`

## Required R packages

Install these packages before running the script:

```r
install.packages(c(
  "dplyr",
  "lubridate",
  "forecast",
  "tseries",
  "urca",
  "ggplot2",
  "zoo",
  "rugarch",
  "Metrics",
  "gridExtra"
))
```

## How to run

Open `sector_volatility_analysis.R` in RStudio or run it from an R session:

```r
source("sector_volatility_analysis.R")
```

Before running:

1. Create `index_data/` in the project folder.
2. Add the required CSV files.
3. Make sure each CSV has `Date` and `Close` columns.

The script now looks for those files relative to the project folder. If any are missing, it stops with a clear error listing the missing paths.

## Key outputs

Running the script produces:

- Monthly seasonality plots for each sector
- Sample spectrum plots
- ACF and PACF plots
- ARIMA, SES, and Holt residual diagnostics
- Stationarity and seasonality summary tables
- ARIMA model comparison table
- Rolling volatility comparison plot
- GARCH vs EGARCH BIC comparison table
- EGARCH coefficient summary table
- Crisis-period volatility plots and summaries
- Forecast accuracy table using RMSE, MAE, and MAPE

## Functions included

Important helper functions in `sector_volatility_analysis.R`:

- `analyze_index_ts()`
- `get_roll_vol()`
- `get_vol_bic()`
- `fit_egarch_sector()`
- `label_crisis_periods()`
- `plot_crisis_vol()`
- `crisis_summary()`
- `forecast_egarch_rolling()`
- `compute_realized_vol()`
- `volatility_eval()`

## Notes and current limitations

- The script is written as a single exploratory workflow rather than a packaged project.
- The raw dataset is not tracked in this repository and must be provided separately.
- Plot labels show some encoding artifacts in a few places.
- The first test-period return is computed within the test split, so it does not use the last training close.
- Most plots are displayed interactively and are not automatically saved.

## Possible next improvements

- Convert absolute paths to relative paths.
- Save plots and tables to an `outputs/` folder.
- Refactor repeated plotting code into reusable functions.
- Separate exploratory analysis from final forecasting code.
