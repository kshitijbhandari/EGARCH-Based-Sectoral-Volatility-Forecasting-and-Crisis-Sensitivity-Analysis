# Results Gallery

This page is the no-code walkthrough of the project results.  
Goal: a reviewer can understand the analysis story without opening the R script.

## 1) What Question Was This Project Solving?

How do major Indian sector indices behave in terms of:

- Monthly return dynamics and seasonality?
- Statistical stationarity?
- Volatility clustering and asymmetry?
- Forecastable short-horizon volatility?

Sectors analyzed:

- NIFTY BANK
- NIFTY IT
- NIFTY PHARMA
- NIFTY FMCG
- NIFTY AUTO

## 2) End-to-End Result Flow

The analysis follows this evidence chain:

1. Build daily and monthly return series from raw closes.
2. Check seasonality and stationarity assumptions.
3. Compare ARIMA-family baselines using BIC and residual diagnostics.
4. Model daily conditional volatility with GARCH and EGARCH.
5. Compare sectors during high-volatility (crisis) windows.
6. Validate one-step-ahead EGARCH forecasts against realized volatility.

## 3) Result Artifacts and What They Mean

| Result block | Why it matters | Where to view |
|---|---|---|
| Month plots + spectra + Friedman tests | Tests whether monthly behavior is seasonal or regime-like | [sector_volatility_analysis.pdf](sector_volatility_analysis.pdf) |
| ADF/PP/KPSS stationarity outputs | Confirms whether modeling assumptions are statistically reasonable | [sector_volatility_analysis.pdf](sector_volatility_analysis.pdf) |
| ARIMA model comparison (BIC) | Identifies parsimonious mean models for monthly returns | [sector_volatility_analysis.pdf](sector_volatility_analysis.pdf) |
| Residual diagnostics (ARIMA/SES/Holt) | Checks leftover structure and model adequacy | [sector_volatility_analysis.pdf](sector_volatility_analysis.pdf) |
| Rolling volatility comparison | Shows cross-sector risk variation over time | [sector_volatility_analysis.pdf](sector_volatility_analysis.pdf) |
| GARCH vs EGARCH BIC table | Tests whether asymmetry/leverage effects improve fit | [sector_volatility_analysis.pdf](sector_volatility_analysis.pdf) |
| EGARCH coefficient summary | Quantifies persistence and asymmetric shock response | [sector_volatility_analysis.pdf](sector_volatility_analysis.pdf) |
| Crisis-period plots and summaries | Compares sector behavior during stress windows | [sector_volatility_analysis.pdf](sector_volatility_analysis.pdf) |
| Forecast accuracy (RMSE/MAE/MAPE) | Measures out-of-sample volatility forecast quality | [sector_volatility_analysis.pdf](sector_volatility_analysis.pdf) |

## 4) How To Read This Gallery In 3 Minutes

1. Open [sector_volatility_analysis.pdf](sector_volatility_analysis.pdf) and scan model comparison and forecast accuracy tables first.
2. Review rolling-volatility and crisis plots to compare sector risk profiles.
3. Use stationarity/seasonality test outputs to interpret how robust the modeling setup is.
4. Use the GARCH vs EGARCH comparison to judge whether asymmetric volatility effects are material.

## 5) Key Methods Used

- Monthly return modeling: ARIMA, SES, Holt.
- Daily volatility modeling: `GARCH(1,1)` and `EGARCH(1,1)`.
- Statistical tests: Friedman, ADF, PP, KPSS, Ljung-Box.
- Forecast evaluation: RMSE, MAE, MAPE on rolling one-step-ahead volatility forecasts.

## 6) Reproducibility Notes

- Main script: `sector_volatility_analysis.R`
- Required local input folder: `index_data/`
- Required files: `NIFTY BANK.csv`, `NIFTY IT.csv`, `NIFTY PHARMA.csv`, `NIFTY FMCG.csv`, `NIFTY AUTO.csv`
- Required fields in each CSV: `Date`, `Close`

## 7) Intended Audience

This gallery is designed for:

- Recruiters and interviewers evaluating outcomes quickly.
- Teammates interested in findings before implementation details.
- Non-technical readers who need the result narrative first.
