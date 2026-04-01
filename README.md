# Time Series Forecasting and Volatility Modeling

This repository analyzes five Indian equity sector indices with a results-first focus:

- NIFTY BANK
- NIFTY IT
- NIFTY PHARMA
- NIFTY FMCG
- NIFTY AUTO

If you only want outcomes, start here:

- **Results Gallery:** [RESULTS_GALLERY.md](RESULTS_GALLERY.md)
- **Full written report (PDF):** [sector_volatility_analysis.pdf](sector_volatility_analysis.pdf)

## Executive Snapshot

This project builds an end-to-end workflow for:

1. Monthly return behavior and seasonality testing.
2. ARIMA-family forecasting baseline comparison.
3. Daily volatility estimation with `GARCH(1,1)` and `EGARCH(1,1)`.
4. Cross-sector crisis-period volatility analysis.
5. Rolling one-step-ahead volatility forecast evaluation.

## Analysis Pipeline

The script `sector_volatility_analysis.R` runs these stages:

1. Load and clean index data.
2. Split into train/test with the last 365 days as test.
3. Compute daily log returns.
4. Aggregate to monthly log returns.
5. Test seasonality via month plots, spectra, and Friedman tests.
6. Test stationarity via ADF, PP, and KPSS.
7. Fit/compare ARIMA models and select by BIC.
8. Fit SES/Holt baselines and run residual checks.
9. Estimate rolling volatility.
10. Compare `GARCH(1,1)` vs `EGARCH(1,1)` by BIC.
11. Summarize persistence and asymmetry from EGARCH.
12. Evaluate volatility forecasts with RMSE, MAE, and MAPE.

## Data Requirements

The raw CSV files are not committed. Create `index_data/` at repo root with:

- `NIFTY BANK.csv`
- `NIFTY IT.csv`
- `NIFTY PHARMA.csv`
- `NIFTY FMCG.csv`
- `NIFTY AUTO.csv`

Each file needs at least:

- `Date`
- `Close`

## Run Locally

Install required R packages:

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

Run:

```r
source("sector_volatility_analysis.R")
```

If files are missing, the script stops with a clear error listing missing paths.

## Core Outputs

The workflow generates:

- Seasonality plots and tests.
- Stationarity test table.
- ARIMA/BIC model comparison.
- Residual diagnostics for ARIMA/SES/Holt.
- Rolling volatility comparison.
- GARCH vs EGARCH comparison table.
- EGARCH coefficient summary table.
- Crisis-period volatility visuals and summaries.
- Forecast accuracy table (RMSE, MAE, MAPE).

For a no-code walkthrough of these outputs, use [RESULTS_GALLERY.md](RESULTS_GALLERY.md).
