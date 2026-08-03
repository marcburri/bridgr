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
model.frame(formula, which = c("estimation", "forecast"), ...)

# S3 method for class 'mf_model'
variable.names(object, which = c("all", "xreg", "target_lags"), ...)

# S3 method for class 'mf_model'
print(x, ...)
```

## Arguments

- object, x, formula:

  A fitted `"mf_model"` object returned by
  [`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md).
  The `formula` spelling is required by the
  [`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html)
  generic and carries the same meaning.

- ...:

  Unused.

- parm, level:

  Passed to [`confint()`](https://rdrr.io/r/stats/confint.html).
  Confidence intervals are computed from the coefficient covariance
  matrix returned by
  [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html), which may be the
  HAC or Delta-HAC covariance when `se = TRUE`. Critical values use a
  t-distribution with residual degrees of freedom from the fitted target
  equation; this is conservative relative to asymptotic normal critical
  values but is common practice in applied econometrics.

- which:

  For [`model.frame()`](https://rdrr.io/r/stats/model.frame.html), which
  modelling frame to return, `"estimation"` (default) or `"forecast"`.
  For [`variable.names()`](https://rdrr.io/r/stats/case.names.html),
  which group of regressor names to return, `"all"` (default), `"xreg"`
  or `"target_lags"`.

## Value

The requested model summary, usually delegated from the stored target
regression fit.

`x`, invisibly.

## Details

`residuals.mf_model()` returns target-equation residuals on the same
standardized scale as the fitted target series, so they can be passed
directly to downstream residual diagnostics.

`model.frame.mf_model()` returns the aligned modelling data. Use
`which = "estimation"` for the in-sample bridge-equation data, and
`which = "forecast"` for the future target-period regressor path the
forecast is produced from. The latter is the frame to modify and pass
back as `xreg` when constructing scenarios.

`variable.names.mf_model()` returns the names of the bridge-equation
regressors. `which = "xreg"` returns the non-target-lag regressors,
which are exactly the series a custom `xreg` must supply when
forecasting a scenario, and pairs with
`model.frame(object, which = "forecast")`.

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
