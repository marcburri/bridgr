# bridgr

`bridgr` is designed to simplify the implementation and evaluation of
bridge models, which are useful for nowcasting (predicting the present
or near-term) and forecasting macroeconomic variables like GDP.

Bridge models are statistical tools that link high-frequency indicators
(e.g., monthly industrial production) to low-frequency target variables
(e.g., quarterly GDP) by forecasting and aggregating the indicators to
match the target’s frequency. They enable timely predictions before the
official release of low-frequency data, making them essential for
policymakers who need early insights for decision-making.

`bridgr` supports regular frequency ladders from `second` up to `year`,
several deterministic and model-based indicator forecasting rules, and
multiple aggregation choices including joint `expalmon` and `beta`
weighting. If a target period contains more high-frequency observations
than implied by the current frequency mapping, the package keeps the
most recent observations and reports a summarized warning.

## Installation

From CRAN:

``` r
install.packages("bridgr")
```

You can install the development version of `bridgr` like so:

``` r
# install.packages("devtools")
devtools::install_github("marcburri/bridgr")
```

## Example

This example estimates a bridge model with uncertainty output:

``` r
suppressPackageStartupMessages(library(bridgr))

gdp <- suppressMessages(tsbox::ts_na_omit(tsbox::ts_pc(bridgr::gdp)))

bridge_model <- bridge(
  target = gdp,
  indic = baro,
  indic_predict = "auto.arima",
  indic_aggregators = "mean",
  indic_lags = 2,
  target_lags = 1,
  h = 2,
  se = TRUE
)

forecast(bridge_model)
#> Bridge forecast
#> -----------------------------------
#> Target series: gdp
#> Forecast horizon: 2
#> Uncertainty: prediction intervals from residual resampling
#> Simulation paths: 100
#> -----------------------------------
#>   time       mean  se    lower_80 upper_80 lower_95 upper_95
#> 1 2023-01-01 0.808 0.785 -0.342   1.605    -0.515   2.388   
#> 2 2023-04-01 0.468 0.821 -0.552   1.449    -1.335   2.213

summary(bridge_model)
#> Bridge model summary
#> -----------------------------------
#> Target series: gdp
#> Target frequency: quarter
#> Forecast horizon: 2
#> Estimation rows: 72
#> Regressors: baro, baro_lag1, baro_lag2, gdp_lag1
#> -----------------------------------
#> Target equation coefficients:
#>             Estimate HAC SE
#> (Intercept)   -6.356  1.254
#> baro           0.159  0.031
#> baro_lag1     -0.134  0.049
#> baro_lag2      0.042  0.019
#> gdp_lag1       0.223  0.143
#> -----------------------------------
#> Indicator summary:
#>      Frequency Predict    Aggregation
#> baro month     auto.arima mean       
#> -----------------------------------
#> Uncertainty:
#> Coefficient SEs: hac
#> Prediction intervals: residual resampling
#> Simulation paths: 100
#> -----------------------------------
```

With `se = TRUE`, `bridgr` reports HAC or Delta-HAC coefficient standard
errors and residual-resampling prediction intervals.

If you want data-driven within-period weights instead of a simple
deterministic aggregator, you can switch the indicator aggregator to
`expalmon`. The corresponding weights are estimated jointly within the
bridge model, and `solver_options` let you control the optimization.

``` r
expalmon_model <- bridge(
  target = gdp,
  indic = baro,
  indic_predict = "auto.arima",
  indic_aggregators = "expalmon",
  solver_options = list(seed = 123, n_starts = 3),
  h = 1
)

summary(expalmon_model)
#> Bridge model summary
#> -----------------------------------
#> Target series: gdp
#> Target frequency: quarter
#> Forecast horizon: 1
#> Estimation rows: 75
#> Regressors: baro
#> -----------------------------------
#> Target equation coefficients:
#>             Estimate
#> (Intercept)   -9.371
#> baro           0.098
#> -----------------------------------
#> Indicator summary:
#>      Frequency Predict    Aggregation
#> baro month     auto.arima expalmon   
#> -----------------------------------
#> Estimated parametric aggregation:
#> baro weights: 0.006, 0.994, 0.000
#> baro parameters: -4.914, -10.000
#> -----------------------------------
#> Joint parametric aggregation optimization:
#> Method: L-BFGS-B
#> Objective value: 60.832
#> Convergence code: 0
#> Best start: 1 / 3
#> -----------------------------------
```
