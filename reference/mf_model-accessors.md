# Accessor Methods for Mixed-Frequency Models

Access standard model summaries from a fitted
[`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md)
object.

## Usage

``` r
# S3 method for class 'mf_model'
coef(object, ...)

# S3 method for class 'mf_model'
confint(object, parm = NULL, level = 0.95, ...)

# S3 method for class 'mf_model'
formula(x, ...)

# S3 method for class 'mf_model'
nobs(object, ...)

# S3 method for class 'mf_model'
vcov(object, ...)

# S3 method for class 'mf_model'
fitted(object, ...)

# S3 method for class 'mf_model'
residuals(object, ...)

# S3 method for class 'mf_model'
print(x, ...)
```

## Arguments

- object, x:

  A fitted `"mf_model"` object returned by
  [`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md).

- ...:

  Unused.

- parm, level:

  Passed to [`confint()`](https://rdrr.io/r/stats/confint.html).
  Confidence intervals are computed from the coefficient covariance
  matrix returned by
  [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html).

## Value

The requested model summary, usually delegated from the stored target
regression fit.

`x`, invisibly.

## Details

`residuals.mf_model()` returns target-equation residuals on the same
standardized scale as the fitted target series, so they can be passed
directly to downstream residual diagnostics.

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

coef(model)
#> (Intercept)        baro 
#>   -9.961997    0.103915 
```
