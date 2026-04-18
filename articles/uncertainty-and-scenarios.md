# Bootstrap Uncertainty and Scenario Forecasting

## Overview

`bridgr` can return point forecasts only, or it can also compute
bootstrap uncertainty for the fitted target equation and predictive
intervals for forecasts.

The relevant estimation arguments are:

- `se = FALSE`: point forecasts only.
- `se = TRUE`: compute bootstrap coefficient uncertainty and predictive
  forecast intervals.
- `bootstrap = list(type = "block", N = 100, block_length = NULL)`:
  control the bootstrap.

At the moment, `"block"` is the only supported bootstrap type.

## Fitting a Model with Bootstrap Uncertainty

``` r
gdp_growth <- suppressMessages(tsbox::ts_na_omit(tsbox::ts_pc(gdp)))

boot_model <- bridge(
  target = gdp_growth,
  indic = baro,
  indic_predict = "auto.arima",
  indic_aggregators = "mean",
  indic_lags = 1,
  target_lags = 1,
  h = 2,
  se = TRUE,
  bootstrap = list(type = "block", N = 20, block_length = NULL)
)
```

The current implementation uses a conditional block bootstrap on the
final target-frequency estimation sample. It does not re-estimate
indicator forecast models or parametric aggregation weights inside each
bootstrap draw, but it does add simulated future target shocks to the
stored forecast draws.

## Forecast Output

Once the model has been estimated with `se = TRUE`,
[`forecast()`](https://generics.r-lib.org/reference/forecast.html)
returns a standardized forecast object with:

- `mean`
- `se`
- `lower`
- `upper`
- `forecast_set`
- bootstrap metadata

``` r
fc <- forecast(boot_model)

fc
#> Bridge forecast
#> -----------------------------------
#> Target series: gdp_growth
#> Forecast horizon: 2
#> Uncertainty: predictive intervals from conditional block bootstrap
#> Bootstrap draws: 20 / 20
#> Block length: 5
#> -----------------------------------
#>   time       mean  se    lower_80 upper_80 lower_95 upper_95
#> 1 2023-01-01 0.972 0.947 -0.499   2.440    -0.524   2.775   
#> 2 2023-04-01 0.698 0.571 -0.190   1.378    -0.529   1.578
fc$bootstrap
#> $enabled
#> [1] TRUE
#> 
#> $type
#> [1] "block"
#> 
#> $N
#> [1] 20
#> 
#> $valid_N
#> [1] 20
#> 
#> $block_length
#> [1] 5
#> 
#> $conditional
#> [1] TRUE
```

The intervals are empirical predictive intervals based on the stored
bootstrap forecast draws.

## Summary Output

The same bootstrap run also feeds into
[`summary()`](https://rdrr.io/r/base/summary.html).

``` r
summary(boot_model)
#> Bridge model summary
#> -----------------------------------
#> Target series: gdp_growth
#> Target frequency: quarter
#> Forecast horizon: 2
#> Estimation rows: 74
#> Regressors: baro, baro_lag1
#> -----------------------------------
#> Target equation coefficients:
#>           Estimate Bootstrap SE
#> ar1          0.243        0.162
#> intercept   -6.493        1.730
#> baro         0.161        0.045
#> baro_lag1   -0.092        0.038
#> -----------------------------------
#> Indicator summary:
#>      Frequency Predict    Aggregation
#> baro month     auto.arima mean       
#> -----------------------------------
#> Uncertainty:
#> Method: conditional block bootstrap with predictive forecast draws
#> Bootstrap draws: 20 / 20
#> Block length: 5
#> -----------------------------------
```

The printed summary keeps the same base layout as a point-estimate model
and adds the uncertainty section only when bootstrap output is
available.

## Scenario Forecasting with `xreg`

If you want to forecast the target under a different future regressor
path, pass a custom `xreg` object to
[`forecast()`](https://generics.r-lib.org/reference/forecast.html). The
custom regressor names must match the ones used in the fitted target
equation.

``` r
scenario_xreg <- dplyr::tibble(
  id = rep(boot_model$regressor_names, each = nrow(boot_model$forecast_set)),
  time = rep(
    boot_model$forecast_set$time,
    times = length(boot_model$regressor_names)
  ),
  value = c(
    boot_model$forecast_set$baro + 5,
    boot_model$forecast_set$baro_lag1 + 5
  )
)

scenario_fc <- forecast(boot_model, xreg = scenario_xreg)

dplyr::tibble(
  time = fc$time,
  baseline = as.numeric(fc$mean),
  scenario = as.numeric(scenario_fc$mean)
)
#> # A tibble: 2 × 3
#>   time       baseline scenario
#>   <date>        <dbl>    <dbl>
#> 1 2023-01-01    0.972     1.32
#> 2 2023-04-01    0.698     1.05
```

When a model contains stored bootstrap draws,
`forecast(..., xreg = ...)` reuses the bootstrap refits and evaluates
them on the scenario regressor path. That keeps the predictive
uncertainty handling consistent between the baseline forecast and
scenario forecasts.

## Point Forecasts Only

If you are only interested in point estimates, leave `se = FALSE`. In
that case, `bootstrap` is ignored and
[`forecast()`](https://generics.r-lib.org/reference/forecast.html) still
returns the same object shape, with `NA` uncertainty fields.

``` r
point_model <- bridge(
  target = gdp_growth,
  indic = baro,
  indic_predict = "auto.arima",
  indic_aggregators = "mean",
  indic_lags = 1,
  target_lags = 1,
  h = 1,
  se = FALSE,
  bootstrap = list(type = "block", N = 20)
)

forecast(point_model)
#> Bridge forecast
#> -----------------------------------
#> Target series: gdp_growth
#> Forecast horizon: 1
#> Uncertainty: point forecast only
#> -----------------------------------
#>   time       mean 
#> 1 2023-01-01 0.972
```

## Interpretation

These intervals are conditional on the aligned target-frequency design
matrix. That means they are predictive intervals for the final target
equation given the regressor path, rather than full end-to-end
uncertainty for the whole mixed-frequency system.
