# Estimate a Bridge Model

Estimate a bridge model that links one lower-frequency target series to
one or more higher-frequency indicator series. Indicators are aligned to
the target frequency by forecasting any missing higher-frequency
observations and aggregating them within each target period.

## Usage

``` r
bridge(
  target,
  indic,
  indic_predict = NULL,
  indic_aggregators = NULL,
  indic_lags = 0,
  target_lags = 0,
  h = 1,
  frequency_conversions = NULL,
  se = FALSE,
  bootstrap = NULL,
  full_system_bootstrap = FALSE,
  solver_options = NULL,
  ...
)
```

## Arguments

- target:

  A single target series in a
  [`tsbox::ts_boxable()`](https://docs.ropensci.org/tsbox/reference/ts_boxable.html)
  format.

- indic:

  One or more indicator series in a
  [`tsbox::ts_boxable()`](https://docs.ropensci.org/tsbox/reference/ts_boxable.html)
  format.

- indic_predict:

  A character vector of indicator forecasting methods. Length must be
  `1` or equal to the number of indicator series. Setting
  `indic_predict = "direct"` switches to direct MIDAS-style alignment:
  the indicators are not forecasted, and the most recent complete
  high-frequency blocks are assigned backward to the target periods.
  Direct alignment must be used for all indicators at once. When
  `h > 1`, the latest complete block is assigned to the farthest
  requested forecast horizon and earlier complete blocks are assigned
  backward from there. For `indic_predict = "mean"`, missing
  high-frequency observations are filled with the mean of the latest
  available `obs_per_target` high-frequency observations, and that same
  mean is extended across the forecast horizon.

- indic_aggregators:

  A character vector of aggregation methods or a list of numeric
  weights. Length must be `1` or equal to the number of indicator
  series. Numeric weights must sum to one and have the appropriate
  length for the inferred target-period block size. `"unrestricted"`
  keeps one separate coefficient per high-frequency observation within
  the target period. The parametric aggregators use two coefficients
  each: `"expalmon"` uses `(linear, quadratic)`, and `"beta"` uses
  `(left_shape, right_shape)` as the normalized beta shape parameters.
  When `indic_predict = "direct"`, `indic_aggregators` is ignored and
  direct blocks are averaged within each target period.

- indic_lags:

  A non-negative integer giving the number of target-period lags to add
  for each aggregated indicator.

- target_lags:

  A non-negative integer giving the autoregressive order in the target
  equation.

- h:

  A positive integer forecast horizon measured in target periods.

- frequency_conversions:

  A named numeric vector used to customize the regular frequency ladder.
  Supported names are `spm`, `mph`, `hpd`, `dpw`, `wpm`, `mpq`, and
  `qpy`.

- se:

  Logical flag indicating whether coefficient standard errors and
  prediction intervals should be computed. When `TRUE`, `bridge()`
  reports HAC standard errors for the linear target equation, or
  Delta-HAC standard errors when parametric aggregation weights are
  estimated jointly.

- bootstrap:

  A list of uncertainty controls. Currently only
  `list(N = 100, block_length = NULL)` is supported. `N` is the number
  of predictive simulation paths used when `se = TRUE`. If
  `full_system_bootstrap = TRUE`, the same `N` controls the number of
  full-system target-period block-bootstrap replications used for
  prediction intervals. `block_length` is only used by the full-system
  bootstrap. When `block_length` is `NULL`, `bridge()` uses
  `ceiling(n^(1/3))` based on the final target-period sample size.

- full_system_bootstrap:

  Logical flag indicating whether prediction intervals and coefficient
  standard errors should be based on a full-system target-period block
  bootstrap instead of residual resampling and HAC / Delta-HAC
  uncertainty from the fitted target equation. This option is only used
  when `se = TRUE`. Because it refits the full bridge workflow on every
  draw, `full_system_bootstrap = TRUE` can be substantially slower than
  the default residual-resampling intervals.

- solver_options:

  A list of optional controls for joint parametric-weight optimization.
  Supported entries are: `method` for the optimizer (`"L-BFGS-B"`,
  `"BFGS"`, `"Nelder-Mead"`, or `"nlminb"`), `maxiter` for the iteration
  budget per optimization run, `n_starts` for the number of multi-start
  attempts, `seed` for reproducible random restarts, `trace` for
  optimizer verbosity, and `start_values` for user-supplied initial
  parameter values. `start_values` can be either a numeric vector or a
  named list. For a numeric vector, values are concatenated in indicator
  order across the parametric aggregators. Within each indicator, the
  parameter order is `(linear, quadratic)` for `"expalmon"` and
  `(left_shape, right_shape)` for `"beta"`. Named-list `start_values`
  must provide exactly the required number of values for each parametric
  indicator. These controls are ignored unless at least one indicator
  uses a parametric aggregator.

- ...:

  Reserved for future extensions.

## Value

An object of class `"bridge"` containing the standardized input series,
inferred frequencies, aligned estimation and forecast datasets, the
fitted target model, fitted indicator models, and metadata required by
[`forecast.bridge()`](https://marcburri.github.io/bridgr/reference/forecast.bridge.md)
and
[`summary.bridge()`](https://marcburri.github.io/bridgr/reference/summary.bridge.md).

## Details

Supported indicator forecasting methods are `"mean"`, `"last"`,
`"auto.arima"`, `"ets"`, and `"direct"`. Supported aggregation methods
are `"mean"`, `"last"`, `"sum"`, `"unrestricted"`, `"expalmon"`,
`"beta"`, or a numeric weight vector supplied inside a
[`list()`](https://rdrr.io/r/base/list.html). `"unrestricted"` expands
each high-frequency observation within a target period into its own
bridge regressor, which corresponds to a U-MIDAS style specification
when the frequency gap is small. When one or more indicators use a
parametric aggregator, the corresponding aggregation weights are
estimated jointly against the final bridge-model objective rather than
one indicator at a time.

Unrestricted mixed-frequency regressions can become parameter-heavy
quickly. When `indic_aggregators = "unrestricted"`, `bridge()` warns if
the final estimation sample contains fewer than 10 observations per
predictor in the bridge regression.

The package assumes a regular frequency ladder
`second -> minute -> hour -> day -> week -> month -> quarter -> year`.
The default number of lower-level observations per higher-level unit is:

- `spm = 60`

- `mph = 60`

- `hpd = 24`

- `dpw = 7`

- `wpm = 4`

- `mpq = 3`

- `qpy = 4`

Users can override any subset of these values with
`frequency_conversions`. If a target period contains more high-frequency
observations than implied by the current mapping, `bridge()` keeps the
most recent observations and emits a summarized warning. If a target
period contains fewer observations than required, the call fails.
Month-, quarter-, and year-based input dates are standardized to period
starts when needed for frequency recognition.

## References

Baffigi, A., Golinelli, R., & Parigi, G. (2004). Bridge models to
forecast the euro area GDP. *International Journal of Forecasting*,
20(3), 447-460.
[doi:10.1016/S0169-2070(03)00067-0](https://doi.org/10.1016/S0169-2070%2803%2900067-0)

Ghysels, E., Sinko, A., & Valkanov, R. (2007). MIDAS regressions:
Further results and new directions. *Econometric Reviews*, 26(1), 53-90.
[doi:10.1080/07474930600972467](https://doi.org/10.1080/07474930600972467)

Andreou, E., Ghysels, E., & Kourtellos, A. (2010). Regression models
with mixed sampling frequencies. *Journal of Econometrics*, 158(2),
246-261.
[doi:10.1016/j.jeconom.2010.01.004](https://doi.org/10.1016/j.jeconom.2010.01.004)

Schumacher, C. (2016). A comparison of MIDAS and bridge equations.
*International Journal of Forecasting*, 32(2), 257-270.
[doi:10.1016/j.ijforecast.2015.07.004](https://doi.org/10.1016/j.ijforecast.2015.07.004)

Burri, M. (2026). Nowcasting Swiss GDP Growth From Public Lead Texts:
Simple Methods Are Sufficient. *Oxford Bulletin of Economics and
Statistics*, 1-25.
[doi:10.1111/obes.70073](https://doi.org/10.1111/obes.70073)

## Examples

``` r
gdp_growth <- suppressMessages(tsbox::ts_na_omit(tsbox::ts_pc(gdp)))

model <- bridge(
  target = gdp_growth,
  indic = baro,
  indic_predict = "auto.arima",
  indic_aggregators = "mean",
  indic_lags = 1,
  target_lags = 1,
  h = 2
)

expalmon_model <- bridge(
  target = gdp_growth,
  indic = baro,
  indic_predict = "auto.arima",
  indic_aggregators = "expalmon",
  solver_options = list(seed = 123, n_starts = 3),
  h = 1
)

forecast(model)
#> Bridge forecast
#> -----------------------------------
#> Target series: gdp_growth
#> Forecast horizon: 2
#> Uncertainty: point forecast only
#> -----------------------------------
#>   time       mean 
#> 1 2023-01-01 0.875
#> 2 2023-04-01 0.678
summary(expalmon_model)
#> Bridge model summary
#> -----------------------------------
#> Target series: gdp_growth
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
