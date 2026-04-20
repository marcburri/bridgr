# Estimate a Mixed-Frequency Model

Estimate a bridge model that links one lower-frequency target series to
one or more higher-frequency indicator series. Indicators are aligned to
the target frequency by forecasting any missing higher-frequency
observations and aggregating them within each target period.

## Usage

``` r
mf_model(
  target,
  indic,
  missing = "error",
  indic_predict = NULL,
  indic_aggregators = NULL,
  indic_lags = 0,
  target_lags = 0,
  h = 1,
  frequency_conversions = NULL,
  se = FALSE,
  bootstrap = NULL,
  full_system_bootstrap = FALSE,
  stationarity = "none",
  solver_options = NULL
)

bridge(...)
```

## Arguments

- target:

  A single target series in a
  [`tsbox::ts_boxable()`](https://docs.ropensci.org/tsbox/reference/ts_boxable.html)
  format, such as a data frame or tibble with `time` and
  `value`/`values` columns, or a regular time-series object supported by
  tsbox.

- indic:

  One or more indicator series in a
  [`tsbox::ts_boxable()`](https://docs.ropensci.org/tsbox/reference/ts_boxable.html)
  format, such as data frames / tibbles with `time` and `value`/`values`
  columns, or regular time-series objects supported by tsbox.

- missing:

  Character string controlling how explicit missing values in submitted
  `target` or `indic` series are handled. `missing = "error"` keeps the
  strict default and aborts on explicit missing values.
  `missing = "drop"` removes explicit missing values with a warning
  before downstream analysis, which is most useful when they correspond
  to supported ragged-edge gaps at the ends of series.
  `missing = "impute"` replaces explicit missing values by series-wise
  linear interpolation with endpoint carry before model fitting. Missing
  timestamps are always an error.

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
  prediction intervals should be computed. When `TRUE`, `mf_model()`
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
  bootstrap. When `block_length` is `NULL`, `mf_model()` uses
  `ceiling(n^(1/3))` based on the final target-period sample size.

- full_system_bootstrap:

  Logical flag indicating whether prediction intervals and coefficient
  standard errors should be based on a full-system target-period block
  bootstrap instead of residual resampling and HAC / Delta-HAC
  uncertainty from the fitted target equation. This option is only used
  when `se = TRUE`. Because it refits the full bridge workflow on every
  draw, `full_system_bootstrap = TRUE` can be substantially slower than
  the default residual-resampling intervals.

- stationarity:

  Character string controlling optional heuristic lower-order
  stationarity diagnostics. `stationarity = "none"` skips diagnostics.
  `stationarity = "warn"` checks submitted target and indicator series
  for a KPSS-style differencing signal and large variance shifts, and
  warns when those heuristics suggest that upstream transformations such
  as differences, growth rates, log changes, or demeaning may be
  appropriate.

- solver_options:

  A list of optional controls for joint parametric-weight optimization.
  Supported entries are: `method` for the optimizer (`"L-BFGS-B"`,
  `"BFGS"`, `"Nelder-Mead"`, or `"nlminb"`), `maxiter` for the iteration
  budget per optimization run, `n_starts` for the number of multi-start
  attempts, `seed` for reproducible random restarts, `trace` for
  optimizer verbosity, `warn` to control whether non-convergence
  warnings are emitted, `reltol` for the relative convergence tolerance
  passed through to the selected optimizer backend, and `start_values`
  for user-supplied initial parameter values. Documented defaults are
  `method = "L-BFGS-B"`, `maxiter = 1000`, `n_starts = 5`, `trace = 0`,
  `warn = TRUE`, and `reltol = 1e-8`. `start_values` can be either a
  numeric vector or a named list. For a numeric vector, values are
  concatenated in indicator order across the parametric aggregators.
  Within each indicator, the parameter order is `(linear, quadratic)`
  for `"expalmon"` and `(left_shape, right_shape)` for `"beta"`.
  Named-list `start_values` must provide exactly the required number of
  values for each parametric indicator. Users can override `reltol` or
  suppress convergence warnings through
  `solver_options = list(reltol = ..., warn = FALSE)`. These controls
  are ignored unless at least one indicator uses a parametric
  aggregator.

- ...:

  Arguments forwarded to `mf_model()`.

## Value

An object of class `"mf_model"` containing the standardized input
series, inferred frequencies, aligned estimation and forecast datasets,
the fitted target model, fitted indicator models, and metadata required
by
[`forecast.mf_model()`](https://marcburri.github.io/bridgr/reference/forecast.mf_model.md)
and
[`summary.mf_model()`](https://marcburri.github.io/bridgr/reference/summary.mf_model.md).

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
quickly. When `indic_aggregators = "unrestricted"`, `mf_model()` warns
if the final estimation sample contains fewer than 10 observations per
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
observations than implied by the current mapping, `mf_model()` keeps the
most recent observations and emits a summarized warning. If a target
period contains fewer observations than required, the call fails.
Month-, quarter-, and year-based input dates are standardized to period
starts when needed for frequency recognition.

## Model specification

`bridgr` does not use a formula interface. Mixed-frequency bridge models
are specified through separate `target` and `indic` series plus the
forecasting, aggregation, and lag controls because the package must
standardize, align, extend, and aggregate the time-series inputs before
the final target regression formula can be assembled.

## Terminology

Throughout `bridgr`, a *target* is the lower-frequency response series
to be forecasted. An *indicator* is any higher-frequency predictor
series aligned to the target frequency before the final regression is
fit. *Indicator forecasting* refers to how end-of-sample indicator
values are completed when the target horizon extends beyond the latest
observed indicator block. *Aggregation* refers to how high-frequency
indicator values within a target period are combined into bridge
regressors. *Direct* prediction skips indicator forecasting and aligns
the latest complete high-frequency blocks backward from the forecast
horizon, while *unrestricted* aggregation keeps one separate coefficient
per within-period high-frequency observation.

## Input standardization

Submitted series are converted to a common internal table with `id`,
`time`, and numeric `values` columns before model fitting. This means
that additional input attributes beyond the series identifier,
timestamps, and numeric values are not preserved in the fitted object
unless they are re-encoded in those standardized columns.

## Input assumptions

`bridgr` assumes that submitted target and indicator series are ordered
by time within each series, free of duplicate timestamps and explicit
missing values, and regular enough for the package to infer a supported
target and indicator frequency. It also assumes that indicator series
are at least as high-frequency as the target. Violations of these
assumptions are rejected during preprocessing and validation rather than
being silently repaired.

## Stationarity

`bridgr` assumes that users provide target and indicator series on a
scale that is appropriate for bridge-style forecasting. In practice this
often means working with growth rates, differences, or other transformed
series prepared upstream. This expectation primarily concerns the
lower-order moments that matter most for bridge-style forecasting,
typically the mean and variance of the submitted series. By default the
package does not automatically enforce stationarity, but
`stationarity = "warn"` enables heuristic pre-fit diagnostics for strong
linear trends and variance shifts and points users toward differences,
growth rates, log changes, demeaning, or other variance-stabilizing
transformations when those heuristics are triggered.

## Deprecated `bridge()` wrapper

`bridge()` is retained for compatibility and forwards to `mf_model()`
with a deprecation warning.

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
gdp_growth <- tsbox::ts_pc(gdp)
#> [value]: 'values' 
#> [value]: 'values' 
gdp_growth <- tsbox::ts_na_omit(gdp_growth)
#> [value]: 'values' 
gdp_growth <- dplyr::slice_tail(gdp_growth, n = 12)
baro_small <- dplyr::slice_tail(baro, n = 36)

mf_model(
  target = gdp_growth,
  indic = baro_small,
  indic_predict = "auto.arima",
  indic_aggregators = "mean",
  indic_lags = 1,
  target_lags = 1,
  h = 1,
  frequency_conversions = c(mpq = 3),
  se = TRUE,
  bootstrap = list(N = 2),
  solver_options = list(seed = 123, n_starts = 1)
)
#> Mixed-frequency model summary
#> -----------------------------------
#> Target series: gdp_growth
#> Target frequency: quarter
#> Forecast horizon: 1
#> Estimation rows: 10
#> Regressors: baro_small, baro_small_lag1, gdp_growth_lag1
#> -----------------------------------
#> Target equation coefficients:
#>                 Estimate HAC SE
#> (Intercept)       -4.556  3.020
#> baro_small         0.118  0.025
#> baro_small_lag1   -0.061  0.042
#> gdp_growth_lag1   -0.188  0.136
#> -----------------------------------
#> Model fit:
#>  Statistic               Value
#>  R-squared               0.820
#>  Adjusted R-squared      0.730
#>  Residual standard error 1.093
#> -----------------------------------
#> Indicator summary:
#>            Frequency Predict    Aggregation
#> baro_small month     auto.arima mean       
#> -----------------------------------
#> Uncertainty:
#> Coefficient SEs: hac
#> Prediction intervals: residual resampling
#> Simulation paths: 2
#> -----------------------------------
```
