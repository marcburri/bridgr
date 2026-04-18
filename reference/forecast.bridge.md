# Forecast a Bridge Model

Forecast the target variable from a fitted
[`bridge()`](https://marcburri.github.io/bridgr/reference/bridge.md)
model.

## Usage

``` r
# S3 method for class 'bridge'
forecast(object, xreg = NULL, level = c(80, 95), ...)

# S3 method for class 'bridge_forecast'
print(x, ...)
```

## Arguments

- object:

  A `"bridge"` object returned by
  [`bridge()`](https://marcburri.github.io/bridgr/reference/bridge.md).

- xreg:

  Optional future regressors in a
  [`tsbox::ts_boxable()`](https://docs.ropensci.org/tsbox/reference/ts_boxable.html)
  format. When omitted, the forecast set stored inside `object` is used.
  When supplied, `xreg` must contain the same regressor names used when
  fitting the bridge equation.

- level:

  Confidence levels used for bootstrap predictive intervals when the
  model was estimated with `se = TRUE`. When uncertainty is unavailable,
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  still returns the `se`, `lower`, and `upper` components, filled with
  `NA`.

- ...:

  Unused.

- x:

  A `"bridge_forecast"` object returned by `forecast.bridge()`.

## Value

An object of class `"bridge_forecast"` and `"forecast"` containing point
forecasts, bootstrap predictive uncertainty summaries, the target-period
regressors used for forecasting, and bootstrap metadata.

`x`, invisibly.
