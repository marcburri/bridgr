# Coerce a Mixed-Frequency Forecast to a forecast Object

Converts a `"mf_model_forecast"` object into a genuine `"forecast"`
object from the forecast package, so that functions such as
[`forecast::accuracy()`](https://pkg.robjhyndman.com/forecast/reference/reexports.html)
can be used on `bridgr` output.

## Usage

``` r
as.forecast(object, ...)

# S3 method for class 'mf_model_forecast'
as.forecast(object, ...)
```

## Arguments

- object:

  A `"mf_model_forecast"` object returned by
  [`forecast.mf_model()`](https://marcburri.github.io/bridgr/reference/forecast.mf_model.md).

- ...:

  Unused.

## Value

An object of class `"forecast"`, with `mean`, `lower`, `upper`, `x`,
`fitted` and `residuals` stored as
[`stats::ts()`](https://rdrr.io/r/stats/ts.html) objects.

## Details

Conversion requires a target frequency that
[`stats::ts()`](https://rdrr.io/r/stats/ts.html) can represent exactly,
that is, a whole number of target periods per calendar year. Annual,
semi-annual, quarterly, bi-monthly and monthly targets qualify; daily,
weekly and sub-daily targets do not, and are rejected with an
informative error rather than silently approximated.

## See also

[`forecast.mf_model()`](https://marcburri.github.io/bridgr/reference/forecast.mf_model.md)
for the native forecast object, which supports every target frequency.

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

converted <- as.forecast(forecast(model))
class(converted)
#> [1] "forecast"
```
