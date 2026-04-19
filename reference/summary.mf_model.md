# Summarize a Mixed-Frequency Model

Summarize a Mixed-Frequency Model

## Usage

``` r
# S3 method for class 'mf_model'
summary(object, ...)
```

## Arguments

- object:

  A `"mf_model"` object returned by
  [`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md).

- ...:

  Unused.

## Value

`object`, invisibly.

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

summary(model)
#> Mixed-frequency model summary
#> -----------------------------------
#> Target series: gdp_growth
#> Target frequency: quarter
#> Forecast horizon: 1
#> Estimation rows: 75
#> Regressors: baro
#> -----------------------------------
#> Target equation coefficients:
#>             Estimate
#> (Intercept)   -9.962
#> baro           0.104
#> -----------------------------------
#> Indicator summary:
#>      Frequency Predict    Aggregation
#> baro month     auto.arima mean       
#> -----------------------------------
```
