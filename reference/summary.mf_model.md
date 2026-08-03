# Summarize a Mixed-Frequency Model

Computes the summary quantities for a fitted
[`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md)
object and returns them as a `"summary.mf_model"` object. Following the
convention of [`summary.lm()`](https://rdrr.io/r/stats/summary.lm.html),
the summary is a data object in its own right: the report is rendered by
[`print.summary.mf_model()`](https://marcburri.github.io/bridgr/reference/print.summary.mf_model.md)
rather than by [`summary()`](https://rdrr.io/r/base/summary.html)
itself, so the individual quantities can be extracted programmatically.

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

An object of class `"summary.mf_model"`, a list with components:

- `target_name`, `target_frequency`, `h`, `nobs`:

  Target series name, inferred target frequency unit, forecast horizon,
  and number of estimation rows.

- `regressor_names`:

  Character vector of bridge-equation regressors.

- `coefficients`:

  Numeric matrix with columns `"Estimate"`, `"Std. Error"`, `"t value"`
  and `"Pr(>|t|)"`. Standard errors come from
  [`vcov.mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model-accessors.md),
  so they are the HAC, Delta-HAC or bootstrap standard errors when the
  model was fitted with `se = TRUE`.

- `coefficient_method`:

  Method used for the coefficient standard errors, or `NULL` when the
  model was fitted without uncertainty.

- `r.squared`, `adj.r.squared`, `sigma`, `df.residual`:

  Fit measures for the target equation.

- `indicators`:

  Data frame of per-indicator frequency, completion method and
  aggregation scheme, one row per indicator.

- `custom_weights`:

  Named list of user-supplied numeric aggregation weights, empty when
  none were used.

- `parametric_weights`, `parametric_parameters`:

  Named lists of estimated parametric aggregation weights and their
  underlying parameters, empty when no parametric aggregator was used.

- `uncertainty`, `bootstrap`, `optimization`:

  Uncertainty settings, bootstrap diagnostics, and joint
  parametric-optimization diagnostics.

## See also

[`coef.mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model-accessors.md),
[`confint.mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model-accessors.md)
and
[`vcov.mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model-accessors.md)
for extracting individual quantities directly from the fitted model.

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

model_summary <- summary(model)
model_summary
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

# The coefficient matrix is available programmatically, as for `lm()`.
coef(model_summary)
#>              Estimate Std. Error   t value     Pr(>|t|)
#> (Intercept) -9.961997 1.28968665 -7.724355 4.635357e-11
#> baro         0.103915 0.01274652  8.152423 7.281639e-12
```
