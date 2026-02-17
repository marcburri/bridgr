# Forecasting from a `bridge` object

This function is used to forecast the target variable using the fitted
`bridge` model.

## Usage

``` r
# S3 method for class 'bridge'
forecast(object, xreg = NULL, ...)
```

## Arguments

- object:

  A `bridge` object obtained from
  [`bridge()`](https://marcburri.github.io/bridgr/reference/bridge.md).

- xreg:

  A
  [`tsbox::ts_boxable()`](https://docs.ropensci.org/tsbox/reference/ts_boxable.html)
  series of external regressors. If not supplied by the user, the
  forecast set calculated by
  [`bridge()`](https://marcburri.github.io/bridgr/reference/bridge.md)
  will be used. This is useful for made up scenarios.

- ...:

  Additional arguments to be passed to the forecast function. Ignored at
  the moment.

## Value

An object of class "`forecast`". An object of class `"forecast"` is a
list usually containing at least the following elements:

- model:

  A list containing information about the fitted model

- method:

  The name of the forecasting method as a character string

- mean:

  Point forecasts as a time series

- lower:

  Lower limits for prediction intervals

- upper:

  Upper limits for prediction intervals

- level:

  The confidence values associated with the prediction intervals

- x:

  The original time series (either `object` itself or the time series
  used to create the model stored as `object`).

- residuals:

  Residuals from the fitted model. For models with additive errors, the
  residuals will be x minus the fitted values.

- fitted:

  Fitted values (one-step forecasts)

- forecast_set:

  The forecast set with the forecasted indicator variables used to
  calculate the forecast of the target variable.

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

# Forecasting using the bridge model
fcst <- forecast(bridge_model)
```
