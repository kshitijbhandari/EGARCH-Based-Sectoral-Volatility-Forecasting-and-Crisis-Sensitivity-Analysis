analyze_index_ts <- function(file_path, index_name = "Index", test_days = 365) {
  

  library(dplyr)
  library(lubridate)
  library(forecast)
  library(tseries)
  library(urca)
  library(ggplot2)
  
  cat("\n==============================\n")
  cat("  ANALYZING:", index_name, "\n")
  cat("==============================\n")
  

  data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
  data$Date <- as.Date(data$Date)
  data <- data %>% arrange(Date)
  
  cutoff_date <- max(data$Date) - test_days
  train_data <- subset(data, Date <= cutoff_date)
  test_data  <- subset(data, Date >  cutoff_date)
  

  train_data <- train_data %>%
    arrange(Date) %>%
    mutate(log_ret = log(Close) - log(lag(Close))) %>%
    filter(!is.na(log_ret))
  
  test_data <- test_data %>%
    arrange(Date) %>%
    mutate(log_ret = log(Close) - log(lag(Close))) %>%
    filter(!is.na(log_ret))
  

  train_monthly <- train_data %>%
    mutate(month = floor_date(Date, "month")) %>%
    group_by(month) %>%
    summarise(month_log_ret = sum(log_ret), .groups = "drop")
  
  test_monthly <- test_data %>%
    mutate(month = floor_date(Date, "month")) %>%
    group_by(month) %>%
    summarise(month_log_ret = sum(log_ret), .groups = "drop")
  

  ts_month_log <- ts(
    train_monthly$month_log_ret,
    start = c(year(min(train_monthly$month)),
              month(min(train_monthly$month))),
    frequency = 12
  )
  

  monthplot(
    ts_month_log,
    main = paste("Month Plot —", index_name),
    ylab = "Monthly Log Returns"
  )
  
  spec.pgram(
    ts_month_log,
    log  = "no",
    main = paste("Sample Spectrum —", index_name)
  )
  
  panel <- train_monthly %>%
    mutate(
      Year  = year(month),
      Month = month(month)
    ) %>%
    select(Year, Month, month_log_ret)
  
  complete_years <- panel %>%
    count(Year) %>%
    filter(n == 12) %>%
    pull(Year)
  
  panel_complete <- panel %>% filter(Year %in% complete_years)
  
  panel_complete$Month <- factor(
    panel_complete$Month,
    levels = 1:12,
    labels = month.abb
  )
  panel_complete$Year <- factor(panel_complete$Year)
  
  friedman_out <- friedman.test(
    month_log_ret ~ Month | Year,
    data = panel_complete
  )
  print(friedman_out)
  

  adf  <- adf.test(ts_month_log)
  pp   <- pp.test(ts_month_log)
  kpss <- kpss.test(ts_month_log, null = "Level")
  
  print(adf); print(pp); print(kpss)
  

  m00 <- Arima(ts_month_log, c(0,0,0), include.mean = TRUE)
  m10 <- Arima(ts_month_log, c(1,0,0), include.mean = TRUE)
  m01 <- Arima(ts_month_log, c(0,0,1), include.mean = TRUE)
  m11 <- Arima(ts_month_log, c(1,0,1), include.mean = TRUE)
  
  info_tab <- data.frame(
    Model = c("ARIMA(0,0,0)", "ARIMA(1,0,0)",
              "ARIMA(0,0,1)", "ARIMA(1,0,1)"),
    BIC   = c(BIC(m00), BIC(m10), BIC(m01), BIC(m11))
  )
  print(info_tab)
  
  auto_fit <- auto.arima(
    ts_month_log,
    seasonal = FALSE,
    ic = "bic",
    stepwise = FALSE,
    approximation = FALSE
  )
  summary(auto_fit)
  

  checkresiduals(auto_fit)
  lb_arima <- Box.test(
    residuals(auto_fit),
    lag = 12,
    type = "Ljung-Box",
    fitdf = 0
  )
  print(lb_arima)
  

  ses_fit  <- ses(ts_month_log)
  holt_fit <- holt(ts_month_log)
  
  checkresiduals(ses_fit)
  lb_ses <- Box.test(residuals(ses_fit), lag = 12, type = "Ljung-Box", fitdf = 0)
  
  checkresiduals(holt_fit)
  lb_holt <- Box.test(residuals(holt_fit), lag = 12, type = "Ljung-Box", fitdf = 0)
  

  return(list(
    index_name         = index_name,
    train_daily        = train_data,
    test_daily         = test_data,
    train_monthly      = train_monthly,
    test_monthly       = test_monthly,
    ts_month_log       = ts_month_log,
    stationarity_tests = list(ADF = adf, PP = pp, KPSS = kpss),
    seasonality_tests  = list(Friedman = friedman_out),
    BIC_table          = info_tab,
    auto_arima         = auto_fit,
    arima_lb_test      = lb_arima,
    ses_fit            = ses_fit,
    holt_fit           = holt_fit,
    ses_lb_test        = lb_ses,
    holt_lb_test       = lb_holt
  ))
}


result_bank <- analyze_index_ts(
  file_path = "C:/lehigh_academics/Homework/time series forecasting/project/index_data/NIFTY BANK.CSV",
  index_name = "NIFTY BANK"
)


result_it <- analyze_index_ts(
  file_path = "C:/lehigh_academics/Homework/time series forecasting/project/index_data/NIFTY IT.CSV",
  index_name = "NIFTY IT"
)

result_pharma <- analyze_index_ts(
  file_path = "C:/lehigh_academics/Homework/time series forecasting/project/index_data/NIFTY PHARMA.CSV",
  index_name = "NIFTY PHARMA"
)

result_fmcg <- analyze_index_ts(
  file_path = "C:/lehigh_academics/Homework/time series forecasting/project/index_data/NIFTY FMCG.CSV",
  index_name = "NIFTY FMCG"
)

result_auto <- analyze_index_ts(
  file_path = "C:/lehigh_academics/Homework/time series forecasting/project/index_data/NIFTY AUTO.CSV",
  index_name = "NIFTY AUTO"
)


results <- list(
  BANK   = result_bank,
  IT     = result_it,
  PHARMA = result_pharma,
  AUTO   = result_auto,
  FMCG   = result_fmcg
)

## all 5 monthplot
dev.new(width = 10, height = 8)

par(mfrow = c(3, 2), mar = c(3, 3, 2, 1))

for (res in results) {
  monthplot(
    res$ts_month_log,
    main = paste("Month Plot —", res$index_name),
    ylab = "Monthly Log Returns",
    xlab = "Month"
  )
}

par(mfrow = c(1, 1))

## sample spectrum for all 5

par(mfrow = c(3, 2), mar = c(3, 3, 2, 1))

for (res in results) {
  spec.pgram(
    res$ts_month_log,
    log = "no",
    main = paste("Spectrum —", res$index_name)
  )
}

par(mfrow = c(1, 1))

## friedman tets

friedman_table <- do.call(
  rbind,
  lapply(results, function(res) {
    data.frame(
      Index = res$index_name,
      Statistic = unname(res$seasonality_tests$Friedman$statistic),
      df = unname(res$seasonality_tests$Friedman$parameter),
      p_value = res$seasonality_tests$Friedman$p.value,
      Seasonality = ifelse(
        res$seasonality_tests$Friedman$p.value < 0.05,
        "Yes",
        "No"
      )
    )
  })
)

friedman_table


#Stationarity test

stationarity_table <- do.call(
  rbind,
  lapply(results, function(res) {
    data.frame(
      Index = res$index_name,
      ADF_p  = res$stationarity_tests$ADF$p.value,
      PP_p   = res$stationarity_tests$PP$p.value,
      KPSS_p = res$stationarity_tests$KPSS$p.value,
      Stationary = ifelse(
        res$stationarity_tests$ADF$p.value < 0.05 &
          res$stationarity_tests$PP$p.value  < 0.05 &
          res$stationarity_tests$KPSS$p.value > 0.05,
        "Yes",
        "No"
      )
    )
  })
)

stationarity_table


# ACF plot
par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))

for (res in results) {
  acf(
    res$ts_month_log,
    main = paste("ACF —", res$index_name)
  )
}

par(mfrow = c(1, 1))

#PACF plot
par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))

for (res in results) {
  pacf(
    res$ts_month_log,
    main = paste("PACF —", res$index_name)
  )
}

par(mfrow = c(1, 1))


#ARIMA residual time series (ALL 5)
par(mfrow = c(3, 2), mar = c(3, 3, 2, 1))

for (res in results) {
  plot(
    residuals(res$auto_arima),
    type = "l",
    main = paste("ARIMA Residuals —", res$index_name),
    ylab = "Residuals"
  )
  abline(h = 0, col = "red", lty = 2)
}

par(mfrow = c(1, 1))


##ACF of ARIMA residuals
par(mfrow = c(3, 2), mar = c(3, 3, 2, 1))

for (res in results) {
  acf(
    residuals(res$auto_arima),
    main = paste("ARIMA Residual ACF —", res$index_name)
  )
}

par(mfrow = c(1, 1))


##Histogram of ARIMA residuals (ALL 5)
par(mfrow = c(3, 2), mar = c(3, 3, 2, 1))

for (res in results) {
  hist(
    residuals(res$auto_arima),
    breaks = 20,
    main = paste("ARIMA Residuals —", res$index_name),
    xlab = "Residuals",
    col = "lightgray"
  )
}

par(mfrow = c(1, 1))

#ARIMA selection + residual whiteness (TABLE)
arima_table <- do.call(
  rbind,
  lapply(results, function(res) {
    data.frame(
      Index = res$index_name,
      Selected_Model = paste0(
        "ARIMA(",
        paste(res$auto_arima$arma[c(1,6,2)], collapse = ","),
        ")"
      ),
      LjungBox_p = res$arima_lb_test$p.value,
      White_Noise = ifelse(res$arima_lb_test$p.value > 0.05, "Yes", "No")
    )
  })
)

arima_table

## SES residual time series
par(mfrow = c(3, 2), mar = c(3, 3, 2, 1))

for (res in results) {
  plot(
    residuals(res$ses_fit),
    type = "l",
    main = paste("SES Residuals —", res$index_name),
    ylab = "Residuals"
  )
  abline(h = 0, col = "red", lty = 2)
}

par(mfrow = c(1, 1))


###ACF of SES residuals (ALL 5)
par(mfrow = c(3, 2), mar = c(3, 3, 2, 1))

for (res in results) {
  acf(
    residuals(res$ses_fit),
    main = paste("SES Residual ACF —", res$index_name)
  )
}

par(mfrow = c(1, 1))

#Holt residual time series (ALL 5)
par(mfrow = c(3, 2), mar = c(3, 3, 2, 1))

for (res in results) {
  plot(
    residuals(res$holt_fit),
    type = "l",
    main = paste("Holt Residuals —", res$index_name),
    ylab = "Residuals"
  )
  abline(h = 0, col = "red", lty = 2)
}

par(mfrow = c(1, 1))

#ACF of Holt residuals (ALL 5)

par(mfrow = c(3, 2), mar = c(3, 3, 2, 1))

for (res in results) {
  acf(
    residuals(res$holt_fit),
    main = paste("Holt Residual ACF —", res$index_name)
  )
}

par(mfrow = c(1, 1))

## ets table

ets_table <- do.call(
  rbind,
  lapply(results, function(res) {
    data.frame(
      Index = res$index_name,
      SES_LB_p  = res$ses_lb_test$p.value,
      Holt_LB_p = res$holt_lb_test$p.value,
      Useful = ifelse(
        res$ses_lb_test$p.value < 0.05 | res$holt_lb_test$p.value < 0.05,
        "Possibly",
        "No"
      )
    )
  })
)

ets_table



## roll vol all in 5

library(dplyr)
library(zoo)

get_roll_vol <- function(result, window = 12) {
  ts_data <- result$ts_month_log
  
  rv <- rollapply(ts_data, width = window, FUN = sd, align = "right", fill = NA)
  
  data.frame(
    Date = time(ts_data),
    RollingVol = as.numeric(rv),
    Sector = result$index_name
  )
}

roll_all <- bind_rows(
  get_roll_vol(result_bank),
  get_roll_vol(result_it),
  get_roll_vol(result_pharma),
  get_roll_vol(result_fmcg),
  get_roll_vol(result_auto)
)



library(ggplot2)

ggplot(roll_all, aes(x = Date, y = RollingVol, color = Sector)) +
  geom_line(size = 1) +
  labs(
    title = "Rolling 12-Month Volatility Across Sectors",
    x = "Year",
    y = "Rolling Volatility (12M)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



library(rugarch)

ret_bank <- result_bank$train_monthly$month_log_ret
r <- as.numeric(ret_bank)

library(forecast)
auto.arima(r, seasonal = FALSE)



## GARCH(1,1)
spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "norm"
)

fit_garch <- ugarchfit(spec_garch, data = r)
show(fit_garch)


### E_GARCH
spec_egarch <- ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
  mean.model     = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "norm"
)

fit_egarch <- ugarchfit(spec_egarch, data = r)
show(fit_egarch)



### bic for GARCH

get_vol_bic <- function(result) {
  r <- as.numeric(result$train_monthly$month_log_ret)
  
  spec_garch <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
    distribution.model = "norm"
  )
  fit_garch <- ugarchfit(spec_garch, data = r)
  
  spec_egarch <- ugarchspec(
    variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
    mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
    distribution.model = "norm"
  )
  fit_egarch <- ugarchfit(spec_egarch, data = r)
  
  bic_garch  <- infocriteria(fit_garch)[3]  
  bic_egarch <- infocriteria(fit_egarch)[3] 
  
  data.frame(
    Index       = result$index_name,
    BIC_GARCH   = round(bic_garch, 4),
    BIC_EGARCH  = round(bic_egarch, 4),
    Better_Model = ifelse(bic_garch < bic_egarch, "GARCH(1,1)", "EGARCH(1,1)")
  )
}


bic_vol_table <- do.call(rbind, lapply(
  list(result_bank, result_it, result_pharma, result_fmcg, result_auto),
  get_vol_bic
))

bic_vol_table


## EGARCH helper func
library(rugarch)

fit_egarch_sector <- function(result) {
  r <- as.numeric(result$train_monthly$month_log_ret)
  
  spec_egarch <- ugarchspec(
    variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
    mean.model     = list(armaOrder = c(0,0), include.mean = TRUE),
    distribution.model = "norm"
  )
  
  fit <- ugarchfit(spec = spec_egarch, data = r)
  
  coefs <- coef(fit)
  
  mu     <- unname(coefs["mu"])
  omega  <- unname(coefs["omega"])
  alpha1 <- unname(coefs["alpha1"])
  beta1  <- unname(coefs["beta1"])
  gamma1 <- unname(coefs["gamma1"])
  
  persistence <- alpha1 + beta1   
  ic <- infocriteria(fit)
  aic_val <- ic[1]
  bic_val <- ic[2]
  
  out_row <- data.frame(
    Index       = result$index_name,
    mu          = mu,
    omega       = omega,
    alpha1      = alpha1,
    beta1       = beta1,
    gamma1      = gamma1,
    Persistence = persistence,
    AIC         = aic_val,
    BIC         = bic_val
  )
  
  return(list(
    fit      = fit,
    summary  = out_row
  ))
}


egarch_bank <- fit_egarch_sector(result_bank)
egarch_bank$summary

egarch_it     <- fit_egarch_sector(result_it)
egarch_pharma <- fit_egarch_sector(result_pharma)
egarch_fmcg   <- fit_egarch_sector(result_fmcg)
egarch_auto   <- fit_egarch_sector(result_auto)

egarch_table <- rbind(
  egarch_bank$summary,
  egarch_it$summary,
  egarch_pharma$summary,
  egarch_fmcg$summary,
  egarch_auto$summary
)

egarch_table


### crisis period vol analysis

label_crisis_periods <- function(df) {
  df$crisis <- "Normal"
  
  df$crisis[df$month >= "2008-01-01" & df$month <= "2009-12-31"] <- "GFC 2008"
  df$crisis[df$month >= "2013-05-01" & df$month <= "2013-11-30"] <- "Taper 2013"
  df$crisis[df$month >= "2016-11-01" & df$month <= "2017-03-31"] <- "Demo 2016"
  df$crisis[df$month >= "2020-02-01" & df$month <= "2020-12-31"] <- "COVID 2020"
  
  return(df)
}

bank_monthly <- result_bank$train_monthly
bank_monthly <- label_crisis_periods(bank_monthly)

plot_crisis_vol <- function(result, window = 6) {
  df <- result$train_monthly
  df <- df %>% arrange(month)
  
  df$vol <- zoo::rollapply(
    df$month_log_ret, 
    width = window, 
    FUN = sd, 
    fill = NA, 
    align = "right"
  )
  
  df <- label_crisis_periods(df)
  
  ggplot(df, aes(month, vol, color = crisis)) +
    geom_line(size = 1) +
    scale_color_manual(values = c(
      "Normal"     = "black",
      "GFC 2008"   = "red",
      "Taper 2013" = "purple",
      "Demo 2016"  = "darkgreen",
      "COVID 2020" = "blue"
    )) +
    labs(
      title = paste("Crisis-Period Volatility –", result$index_name),
      x = "Year", y = paste(window, "Month Rolling Volatility")
    ) +
    theme_minimal()
}

plot_crisis_vol(result_bank, window = 6)

## crisis summary table

crisis_summary <- function(result, window = 6) {
  df <- result$train_monthly
  df <- df %>% arrange(month)
  
  df$vol <- zoo::rollapply(
    df$month_log_ret, 
    width = window, 
    FUN = sd, 
    fill = NA, 
    align = "right"
  )
  
  df <- label_crisis_periods(df)
  
  df %>%
    group_by(crisis) %>%
    summarise(
      AvgVol = mean(vol, na.rm = TRUE),
      MedianVol = median(vol, na.rm = TRUE),
      Obs = n()
    )
}

crisis_summary(result_bank)

plot_crisis_vol(result_it)
plot_crisis_vol(result_pharma)
plot_crisis_vol(result_auto)
plot_crisis_vol(result_fmcg)

crisis_summary(result_it)
crisis_summary(result_pharma)
crisis_summary(result_auto)
crisis_summary(result_fmcg)




## cross sector comparison

final_table <- data.frame(
  Sector = c("BANK", "IT", "PHARMA", "AUTO", "FMCG"),
  Persistence = c(
    egarch_bank$summary$Persistence,
    egarch_it$summary$Persistence,
    egarch_pharma$summary$Persistence,
    egarch_auto$summary$Persistence,
    egarch_fmcg$summary$Persistence
  ),
  Asymmetry = c(
    egarch_bank$summary$gamma1,
    egarch_it$summary$gamma1,
    egarch_pharma$summary$gamma1,
    egarch_auto$summary$gamma1,
    egarch_fmcg$summary$gamma1
  )
)
final_table



####### forecast volatility


forecast_egarch_rolling <- function(result) {
  library(rugarch)
  library(zoo)
  
  r_train <- result$train_daily$log_ret
  r_test  <- result$test_daily$log_ret
  r_all   <- c(r_train, r_test)
  
  n_test <- length(r_test)
  
  spec <- ugarchspec(
    mean.model = list(armaOrder = c(0,0)),
    variance.model = list(model = "eGARCH", garchOrder = c(1,1)),
    distribution.model = "norm"
  )
  
  fit <- ugarchfit(spec, data = r_all, out.sample = n_test)
  
  fc <- ugarchforecast(fit, n.ahead = 1, n.roll = n_test - 1)
  
  sigma_daily <- sigma(fc)[1, ]
  
  sigma_annual <- sigma_daily * sqrt(252)
  
  return(sigma_annual)
}



compute_realized_vol <- function(result, window = 20) {
  library(zoo)
  
  test_r <- result$test_daily$log_ret
  
  realized <- rollapply(
    test_r,
    width = window,
    FUN = function(x) sd(x, na.rm = TRUE) * sqrt(252),
    align = "right",
    fill = NA
  )
  
  return(realized)
}



## forecast volatility engine
volatility_eval <- function(result, sector_name = result$index_name) {
  library(ggplot2)
  library(Metrics)
  library(dplyr)
  
  fc  <- forecast_egarch_rolling(result)       
  rv  <- compute_realized_vol(result)          
  
  df_compare <- data.frame(
    Date        = result$test_daily$Date,
    ForecastVol = fc,
    RealizedVol = rv
  )
  
  plt <- ggplot(df_compare, aes(Date)) +
    geom_line(aes(y = ForecastVol, color = "Forecasted Volatility"), size = 1) +
    geom_line(aes(y = RealizedVol, color = "Realized Volatility"), size = 1, alpha = 0.8) +
    scale_color_manual(values = c(
      "Forecasted Volatility" = "blue",
      "Realized Volatility"   = "red"
    )) +
    labs(
      title = paste("Rolling 1-Step Ahead EGARCH vs Realized Vol —", sector_name),
      y = "Annualized Volatility",
      x = "Date",
      color = "Legend"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  df_clean <- df_compare %>% filter(!is.na(ForecastVol), !is.na(RealizedVol))
  
  vol_rmse <- Metrics::rmse(df_clean$RealizedVol, df_clean$ForecastVol)
  vol_mae  <- Metrics::mae(df_clean$RealizedVol, df_clean$ForecastVol)
  vol_mape <- Metrics::mape(df_clean$RealizedVol, df_clean$ForecastVol)
  
  metrics <- c(RMSE = vol_rmse, MAE = vol_mae, MAPE = vol_mape)
  

  return(list(
    sector  = sector_name,
    compare = df_compare,
    plot    = plt,
    metrics = metrics
  ))
}

bank_vol   <- volatility_eval(result_bank)
it_vol     <- volatility_eval(result_it)
pharma_vol <- volatility_eval(result_pharma)
auto_vol   <- volatility_eval(result_auto)
fmcg_vol   <- volatility_eval(result_fmcg)

bank_vol$plot

### plot with all 5 plots in one

library(gridExtra)

grid.arrange(
  bank_vol$plot,
  it_vol$plot,
  pharma_vol$plot,
  auto_vol$plot,
  fmcg_vol$plot,
  ncol = 2
)

## table with all metrics
accuracy_table <- data.frame(
  Sector = c(
    bank_vol$sector,
    it_vol$sector,
    pharma_vol$sector,
    auto_vol$sector,
    fmcg_vol$sector
  ),
  RMSE = c(
    bank_vol$metrics["RMSE"],
    it_vol$metrics["RMSE"],
    pharma_vol$metrics["RMSE"],
    auto_vol$metrics["RMSE"],
    fmcg_vol$metrics["RMSE"]
  ),
  MAE = c(
    bank_vol$metrics["MAE"],
    it_vol$metrics["MAE"],
    pharma_vol$metrics["MAE"],
    auto_vol$metrics["MAE"],
    fmcg_vol$metrics["MAE"]
  ),
  MAPE = c(
    bank_vol$metrics["MAPE"],
    it_vol$metrics["MAPE"],
    pharma_vol$metrics["MAPE"],
    auto_vol$metrics["MAPE"],
    fmcg_vol$metrics["MAPE"]
  )
)

accuracy_table