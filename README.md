# EGARCH-Based Sectoral Volatility Forecasting and Crisis Sensitivity Analysis

This project applies a full time-series pipeline to five Indian equity sector indices (NIFTY BANK, IT, PHARMA, FMCG, AUTO) spanning over 20 years of daily price history. The central question is whether an EGARCH(1,1) model fitted to monthly log returns can produce reliable one-step-ahead volatility forecasts, and whether the model's asymmetry and persistence parameters differentiate meaningfully across sectors and crisis regimes.

---

## Data and Preprocessing

Daily closing prices for each sector index are converted to log returns: `r_t = ln(P_t) - ln(P_{t-1})`. Daily returns are then aggregated to monthly log returns by summing within each calendar month. This aggregation reduces microstructure noise and aligns the modeling frequency with the horizon of interest.

The last 365 calendar days are held out as the test set. Everything prior is training data. No look-ahead is used at any stage: the rolling forecast procedure re-estimates on an expanding window at each step, using only information available at that date.

---

## Seasonality Testing

Month plots and periodograms are produced for each sector before any model is fitted. The Friedman non-parametric test formally tests whether calendar month has a significant effect on return levels. The null hypothesis of no seasonal effect is not rejected for any of the five sectors. Spectral plots confirm this: the periodograms are broadly flat with no dominant frequency spike at monthly or quarterly cycles. As a result, no seasonal differencing is applied and non-seasonal ARIMA is used throughout.

---

## Stationarity Testing

Three unit-root and stationarity tests are applied to each sector's monthly log return series:

- **ADF (Augmented Dickey-Fuller):** null hypothesis is a unit root. Rejected at 1% for all sectors.
- **Phillips-Perron:** null hypothesis is a unit root. Rejected at 1% for all sectors.
- **KPSS:** null hypothesis is stationarity. Fails to reject for all sectors.

The three-test consensus confirms that monthly log returns are stationary in mean. No differencing is required, so `d = 0` in all ARIMA specifications.

ACF and PACF plots show that almost all lags fall within the 95% confidence bands, consistent with near-white-noise behavior in the mean. This rules out high-order AR or MA components and guides auto.arima toward parsimonious models.

---

## Mean Model Fitting

Four candidate ARIMA models are manually fitted for each sector: ARIMA(0,0,0), (1,0,0), (0,0,1), and (1,0,1). An exhaustive auto.arima search using BIC as the selection criterion is then run. BIC's stronger penalty for complexity means most sectors are selected as ARIMA(0,0,0) with zero mean or a non-zero mean constant, reflecting the near-absence of exploitable autocorrelation in monthly equity returns.

Simple Exponential Smoothing (SES) and Holt's linear trend method are also fitted as benchmarks. Ljung-Box tests confirm white-noise residuals for all three model classes across all five sectors. Residual histograms reveal heavier-than-normal tails (leptokurtosis), which motivates using a Student-t error distribution in the variance model.

The key diagnostic takeaway from this stage is that large-magnitude residual spikes appear around 2008 and 2020 in every sector. ARIMA models the conditional mean and cannot adapt its variance over time. This clustering of large residuals is textbook ARCH effect, and directly motivates moving to GARCH for the variance equation.

---

## Volatility Modeling: GARCH vs EGARCH

GARCH(1,1) and EGARCH(1,1) are fitted to the ARIMA residuals for each sector. Model selection is based on BIC. EGARCH wins for all five sectors.

The standard GARCH(1,1) variance equation is:

```
h_t = omega + alpha * u_{t-1}^2 + beta * h_{t-1}
```

EGARCH models the log of conditional variance and adds an asymmetry term:

```
ln(h_t) = omega + alpha * (|z_{t-1}| - E|z|) + gamma * z_{t-1} + beta * ln(h_{t-1})
```

The gamma coefficient captures the leverage effect: when `gamma < 0`, a negative shock of size `z` increases conditional variance more than a positive shock of the same size. This is a well-documented property of equity markets where price drops tend to cause larger volatility increases than equivalent price rises. All five sectors produce negative gamma estimates, confirming the leverage effect is present across the Indian equity universe studied here.

The persistence of volatility shocks is measured by `alpha1 + beta1`. Values close to 1 indicate that shocks take a long time to decay. Across the five sectors the average persistence is approximately 0.97, meaning roughly 97% of a volatility shock is still present in the next period's conditional variance. This is typical of equity index data and has a direct implication for forecasting: EGARCH will tend to track the current volatility regime closely rather than revert quickly to the long-run mean.

---

## Crisis Period Analysis

Four historical crisis windows are labeled on each sector's conditional volatility series:

| Event | Window |
|---|---|
| GFC 2008 | Jan 2008 to Dec 2009 |
| Taper Tantrum 2013 | May 2013 to Nov 2013 |
| Demonetization 2016 | Nov 2016 to Mar 2017 |
| COVID-19 2020 | Feb 2020 to Dec 2020 |

Several cross-sector patterns emerge from this analysis:

**NIFTY BANK** shows the largest GFC spike and a sharp COVID surge. As a financial sector index, BANK is directly exposed to credit and liquidity stress, making it the most sensitive sector to systemic financial crises.

**NIFTY IT** exhibits an extreme early-period volatility spike (near 1.0 annualized) driven by the dot-com residual and index construction history from the early 2000s. During GFC and COVID, IT volatility is comparatively lower than BANK, reflecting the sector's less leveraged balance sheets and recurring revenue model.

**NIFTY PHARMA** shows a moderate GFC response and an elevated COVID spike, the opposite of IT. The pandemic created demand and supply-chain uncertainty in the pharmaceutical space, increasing volatility even as the broader sector benefited from health-sector attention.

**NIFTY AUTO** is highly cyclical and shows sharp spikes in both GFC and COVID windows. Vehicle demand is sensitive to credit availability and consumer confidence, making AUTO one of the first sectors to transmit macroeconomic stress into price volatility.

**NIFTY FMCG** is the most defensive sector in the study and shows the smallest crisis spikes overall. The exception is Demonetization 2016, where FMCG distribution channels were directly disrupted by the sudden cash withdrawal policy. This differentiates FMCG from IT and Pharma during that event: it was a domestic demand-side shock, not a financial or global shock, and FMCG bore the brunt of it.

The Taper Tantrum of 2013 produces the smallest uniform response across all sectors, consistent with its characterization as a rate expectation shock rather than a realized economic disruption.

---

## Volatility Forecasting

Rolling one-step-ahead EGARCH forecasts are generated for each trading day in the 365-day test window. At each step the model is re-estimated on all available data up to that date, and the one-step-ahead conditional standard deviation is recorded as the forecast. This expanding-window design avoids any look-ahead bias and simulates the information set a practitioner would actually have.

Realized volatility is computed as the annualized rolling 20-day standard deviation of daily log returns and serves as the benchmark against which forecasts are evaluated.

**Forecast accuracy across sectors:**

| Sector | RMSE | MAE | MAPE |
|---|---|---|---|
| NIFTY BANK | 4.32% | 3.07% | 12.6% |
| NIFTY IT | 3.85% | 3.24% | 19.6% |
| NIFTY PHARMA | 3.89% | 3.12% | 14.6% |
| NIFTY AUTO | 3.48% | 2.77% | 12.7% |
| NIFTY FMCG | 2.96% | 2.61% | 22.2% |

FMCG achieves the lowest RMSE and MAE in absolute terms, consistent with its stable, low-volatility return process. NIFTY BANK has the highest RMSE, reflecting the larger and more abrupt volatility swings in the financial sector. NIFTY IT and FMCG show the highest MAPE despite relatively low absolute errors: percentage errors are inflated when realized volatility passes through very low values (near zero denominator), which occurs more frequently in sectors where baseline volatility is low.

The common pattern across all sectors is that EGARCH forecasts track the directional level and broad trend of realized volatility well. Forecast errors are concentrated at sudden regime transitions, where realized volatility jumps faster than the persistence-heavy EGARCH process can update. This is the expected behavior of a high-persistence model: it is accurate in stable regimes and lags at turning points.

---

## Results Gallery

An interactive results gallery is available at the GitHub Pages site for this repository, organized by analysis stage with a filterable card layout and full-image lightbox.
