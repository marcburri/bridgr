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

An object of class `"mf_model_forecast"` containing point forecasts,
predictive uncertainty summaries, the observed target history, the
target-period regressors used for forecasting, and optional full-system
bootstrap metadata.

`x`, invisibly.

## Details

In recursive bridge forecasts, uncertainty typically increases with
horizon because later forecast steps depend on forecasted rather than
observed target lags and, when needed, completed indicator paths. Under
the package's residual-resampling and full-system bootstrap workflows,
those simulated disturbances accumulate across steps, so standard errors
and interval widths can widen as the forecast horizon extends. The
`uncertainty-and-scenarios` vignette includes one worked example that
trims forecast rows by an acceptable prediction-interval width.

## Interoperability with the forecast package

`"mf_model_forecast"` deliberately does *not* inherit from the
`forecast` package's `"forecast"` class. Target frequencies supported by
[`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md)
include daily, weekly and sub-daily series, which
[`stats::ts()`](https://rdrr.io/r/stats/ts.html) cannot represent
without silently approximating the calendar, so an object that claimed
`"forecast"` inheritance could not honour it for every model this
package fits. Instead,
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
methods are provided directly for `"mf_model_forecast"`, and
[`as.forecast()`](https://marcburri.github.io/bridgr/reference/as.forecast.md)
converts to a genuine `"forecast"` object whenever the target frequency
is regular enough to allow it, for use with functions such as
[`forecast::accuracy()`](https://pkg.robjhyndman.com/forecast/reference/reexports.html).

## Examples

``` r
gdp_growth <- tsbox::ts_pc(gdp)
#> [value]: 'values' 
#> [value]: 'values' 
gdp_growth <- tsbox::ts_na_omit(gdp_growth)
#> [value]: 'values' 
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
