# Summarize a `bridge` object

This method is summarizes the bridge model.

## Usage

``` r
# S3 method for class 'bridge'
summary(object, ...)
```

## Arguments

- object:

  A `bridge` object obtained from
  [`bridge()`](https://marcburri.github.io/bridgr/reference/bridge.md).

- ...:

  Additional arguments to be passed to the summary function. Ignored at
  the moment.

## Value

The function `summary` is used to obtain and print a summary of the
results.

## Examples

``` r
library(bridgr)

# Example usage
target_series <- suppressMessages(tsbox::ts_tbl(data.frame(
  time = seq(as.Date("2020-01-01"), as.Date("2022-12-01"), by = "quarter"),
  value = rnorm(12)
)))

indic_series <- suppressMessages(tsbox::ts_tbl(data.frame(
  time = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "month"),
  value = rnorm(37)
)))

bridge_model <- suppressMessages(bridge(
  target = target_series,
  indic = indic_series,
  indic_predict = "mean",
  indic_aggregators = "mean",
  indic_lags = 2,
  target_lags = 1,
  h = 1
))

# Summarize the information in the bridge model
summary(bridge_model)
#> Bridge model summary
#> -----------------------------------
#> Main model:
#> -----------------------------------
#> Series:  target_series 
#> Regression with ARIMA(1,0,0) errors 
#> 
#> Coefficients:
#>           ar1  intercept  indic_series  indic_series_lag1  indic_series_lag2
#>       -0.2348    -0.1368       -0.9642            -0.2769            -0.2289
#> s.e.   0.3009     0.1359        0.2902             0.2916             0.2642
#> 
#> sigma^2 = 0.4244:  log likelihood = -6.47
#> AIC=24.93   AICc=52.93   BIC=26.75
#> -----------------------------------
#> Single indicator models:
#> -----------------------------------
#> Series:  indic_series 
#> Writing forward the mean over values in last low frequency period.
#> Aggregation to low frequency:
#> Using mean over values in corresponding periods.
#> -----------------------------------
```
