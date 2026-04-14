# Forecast a Bridge Model

Forecast the target variable from a fitted
[`bridge()`](https://marcburri.github.io/bridgr/reference/bridge.md)
model.

## Usage

``` r
# S3 method for class 'bridge'
forecast(object, xreg = NULL, ...)
```

## Arguments

- object:

  A `"bridge"` object returned by
  [`bridge()`](https://marcburri.github.io/bridgr/reference/bridge.md).

- xreg:

  Optional future regressors in a
  [`tsbox::ts_boxable()`](https://docs.ropensci.org/tsbox/reference/ts_boxable.html)
  format. When omitted, the forecast set stored inside `object` is used.

- ...:

  Passed to
  [`forecast::forecast()`](https://pkg.robjhyndman.com/forecast/reference/reexports.html).

## Value

An object of class `"forecast"` with an additional `forecast_set`
component containing the target-period regressors used for the forecast.
