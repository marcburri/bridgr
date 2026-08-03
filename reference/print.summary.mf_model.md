# Print a Mixed-Frequency Model Summary

Print a Mixed-Frequency Model Summary

## Usage

``` r
# S3 method for class 'summary.mf_model'
print(x, ...)
```

## Arguments

- x:

  A `"summary.mf_model"` object returned by
  [`summary.mf_model()`](https://marcburri.github.io/bridgr/reference/summary.mf_model.md).

- ...:

  Unused.

## Value

`x`, invisibly.

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

print(summary(model))
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
#> Model fit:
#>  Statistic               Value
#>  R-squared               0.477
#>  Adjusted R-squared      0.469
#>  Residual standard error 0.967
#> -----------------------------------
#> Indicator summary:
#>      Frequency Predict    Aggregation
#> baro month     auto.arima mean       
#> -----------------------------------
```
