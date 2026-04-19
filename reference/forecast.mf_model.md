# Forecast a Mixed-Frequency Model

Forecast the target variable from a fitted
[`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md)
object.

## Usage

``` r
# S3 method for class 'mf_model'
forecast(object, xreg = NULL, level = c(80, 95), ...)

# S3 method for class 'mf_model_forecast'
print(x, ...)
```

## Arguments

- object:

  A `"mf_model"` object returned by
  [`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md).

- xreg:

  Optional future regressors in a
  [`tsbox::ts_boxable()`](https://docs.ropensci.org/tsbox/reference/ts_boxable.html)
  format. When omitted, the forecast regressor set stored inside
  `object` is used. When supplied, `xreg` must contain the same
  non-target regressors used when fitting the bridge equation.

- level:

  Prediction interval levels used when the model was estimated with
  `se = TRUE`. When uncertainty is unavailable,
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  still returns the `se`, `lower`, and `upper` components, filled with
  `NA`.

- ...:

  Unused.

- x:

  A `"mf_model_forecast"` object returned by `forecast.mf_model()`.

## Value

An object of class `"mf_model_forecast"` and `"forecast"` containing
point forecasts, predictive uncertainty summaries, the target-period
regressors used for forecasting, and optional full-system bootstrap
metadata.

`x`, invisibly.

## Examples

``` r
gdp_growth <- suppressMessages(tsbox::ts_na_omit(tsbox::ts_pc(gdp)))
model <- mf_model(
  target = gdp_growth,
  indic = baro,
  indic_predict = "auto.arima",
  indic_aggregators = "mean",
  h = 1
)

forecast(model)
#> Mixed-frequency forecast
#> -----------------------------------
#> Target series: gdp_growth
#> Forecast horizon: 1
#> Uncertainty: point forecast only
#> -----------------------------------
#>   time       mean 
#> 1 2023-01-01 0.161
```
