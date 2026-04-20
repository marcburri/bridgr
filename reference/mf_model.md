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
  indic_predict = NULL,
  indic_aggregators = NULL,
  indic_lags = 0,
  target_lags = 0,
  h = 1,
  frequency_conversions = NULL,
  se = FALSE,
  bootstrap = NULL,
  full_system_bootstrap = FALSE,
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
#> $target
#> # A tibble: 12 × 3
#>    id         time       values
#>    <chr>      <date>      <dbl>
#>  1 gdp_growth 2020-01-01 -1.50 
#>  2 gdp_growth 2020-04-01 -6.50 
#>  3 gdp_growth 2020-07-01  6.88 
#>  4 gdp_growth 2020-10-01  0.369
#>  5 gdp_growth 2021-01-01  0.442
#>  6 gdp_growth 2021-04-01  2.47 
#>  7 gdp_growth 2021-07-01  2.34 
#>  8 gdp_growth 2021-10-01  0.411
#>  9 gdp_growth 2022-01-01  0.105
#> 10 gdp_growth 2022-04-01  1.03 
#> 11 gdp_growth 2022-07-01  0.255
#> 12 gdp_growth 2022-10-01  0.102
#> 
#> $indic
#> # A tibble: 36 × 3
#>    id         time       values
#>    <chr>      <date>      <dbl>
#>  1 baro_small 2020-01-01  101. 
#>  2 baro_small 2020-02-01   99.2
#>  3 baro_small 2020-03-01   80.8
#>  4 baro_small 2020-04-01   53.8
#>  5 baro_small 2020-05-01   54.5
#>  6 baro_small 2020-06-01   89.6
#>  7 baro_small 2020-07-01  111. 
#>  8 baro_small 2020-08-01  117. 
#>  9 baro_small 2020-09-01  110. 
#> 10 baro_small 2020-10-01  112. 
#> # ℹ 26 more rows
#> 
#> $target_name
#> [1] "gdp_growth"
#> 
#> $indic_name
#> [1] "baro_small"
#> 
#> $target_frequency
#> # A tibble: 1 × 3
#>   id         unit     step
#>   <chr>      <chr>   <dbl>
#> 1 gdp_growth quarter     1
#> 
#> $indicator_frequencies
#> # A tibble: 1 × 3
#>   id         unit   step
#>   <chr>      <chr> <dbl>
#> 1 baro_small month     1
#> 
#> $indic_predict
#> [1] "auto.arima"
#> 
#> $indic_aggregators_requested
#> $indic_aggregators_requested[[1]]
#> [1] "mean"
#> 
#> 
#> $indic_aggregators
#> $indic_aggregators[[1]]
#> [1] "mean"
#> 
#> 
#> $indic_lags
#> [1] 1
#> 
#> $target_lags
#> [1] 1
#> 
#> $target_lag_names
#> [1] "gdp_growth_lag1"
#> 
#> $h
#> [1] 1
#> 
#> $frequency_conversions
#> spm mph hpd dpw wpm mpq qpy 
#>  60  60  24   7   4   3   4 
#> 
#> $se
#> [1] TRUE
#> 
#> $full_system_bootstrap
#> [1] FALSE
#> 
#> $bootstrap
#> $bootstrap$enabled
#> [1] FALSE
#> 
#> $bootstrap$N
#> [1] 2
#> 
#> $bootstrap$valid_N
#> [1] 0
#> 
#> $bootstrap$block_length
#> NULL
#> 
#> $bootstrap$coefficient_draws
#> NULL
#> 
#> $bootstrap$coefficient_covariance
#> NULL
#> 
#> $bootstrap$coefficient_se
#> NULL
#> 
#> $bootstrap$prediction_draws
#> NULL
#> 
#> $bootstrap$models
#> NULL
#> 
#> $bootstrap$target_histories
#> NULL
#> 
#> $bootstrap$requested
#> [1] FALSE
#> 
#> 
#> $uncertainty
#> $uncertainty$enabled
#> [1] TRUE
#> 
#> $uncertainty$coefficient_method
#> [1] "hac"
#> 
#> $uncertainty$coefficient_covariance
#>                  (Intercept)    baro_small baro_small_lag1 gdp_growth_lag1
#> (Intercept)      9.120675787  0.0055728578   -0.1008626690     0.355101063
#> baro_small       0.005572858  0.0006209409   -0.0006854336     0.001638296
#> baro_small_lag1 -0.100862669 -0.0006854336    0.0017473648    -0.005400566
#> gdp_growth_lag1  0.355101063  0.0016382956   -0.0054005658     0.018395183
#> 
#> $uncertainty$coefficient_se
#>     (Intercept)      baro_small baro_small_lag1 gdp_growth_lag1 
#>      3.02004566      0.02491869      0.04180149      0.13562884 
#> 
#> $uncertainty$prediction_method
#> [1] "residual_resampling"
#> 
#> $uncertainty$prediction_draws
#>          [,1]
#> [1,] 1.047887
#> [2,] 3.205471
#> 
#> $uncertainty$simulation_paths
#> [1] 2
#> 
#> 
#> $solver_options
#> $solver_options$method
#> [1] "L-BFGS-B"
#> 
#> $solver_options$maxiter
#> [1] 1000
#> 
#> $solver_options$n_starts
#> [1] 1
#> 
#> $solver_options$seed
#> [1] 123
#> 
#> $solver_options$trace
#> [1] 0
#> 
#> $solver_options$start_values
#> NULL
#> 
#> 
#> $formula
#> gdp_growth ~ baro_small + baro_small_lag1 + gdp_growth_lag1
#> <environment: 0x5589dc6e5948>
#> 
#> $estimation_set
#> # A tibble: 10 × 5
#>    time       gdp_growth baro_small baro_small_lag1 gdp_growth_lag1
#>    <date>          <dbl>      <dbl>           <dbl>           <dbl>
#>  1 2020-07-01      6.88       113.             66.0          -6.50 
#>  2 2020-10-01      0.369      107.            113.            6.88 
#>  3 2021-01-01      0.442      109.            107.            0.369
#>  4 2021-04-01      2.47       125.            109.            0.442
#>  5 2021-07-01      2.34       112.            125.            2.47 
#>  6 2021-10-01      0.411      104.            112.            2.34 
#>  7 2022-01-01      0.105       97.4           104.            0.411
#>  8 2022-04-01      1.03        94.3            97.4           0.105
#>  9 2022-07-01      0.255       90.0            94.3           1.03 
#> 10 2022-10-01      0.102       90.7            90.0           0.255
#> 
#> $forecast_base_set
#> # A tibble: 1 × 3
#>   time       baro_small baro_small_lag1
#>   <date>          <dbl>           <dbl>
#> 1 2023-01-01       98.3            90.7
#> 
#> $forecast_set
#> # A tibble: 1 × 4
#>   time       baro_small baro_small_lag1 gdp_growth_lag1
#>   <date>          <dbl>           <dbl> <list>         
#> 1 2023-01-01       98.3            90.7 <dbl [1]>      
#> 
#> $model
#> 
#> Call:
#> stats::lm(formula = formula, data = estimation_set)
#> 
#> Coefficients:
#>     (Intercept)       baro_small  baro_small_lag1  gdp_growth_lag1  
#>        -4.55613          0.11797         -0.06052         -0.18792  
#> 
#> 
#> $indic_models
#> $indic_models$baro_small
#> Series: xts_series 
#> ARIMA(1,0,2) with non-zero mean 
#> 
#> Coefficients:
#>          ar1     ma1     ma2     mean
#>       0.4157  0.8900  0.5322  99.9752
#> s.e.  0.1990  0.1954  0.1624   5.0588
#> 
#> sigma^2 = 65.32:  log likelihood = -125.2
#> AIC=260.41   AICc=262.41   BIC=268.32
#> 
#> 
#> $parametric_weights
#> list()
#> 
#> $parametric_parameters
#> list()
#> 
#> $parametric_specs
#> list()
#> 
#> $fixed_aggregated
#> # A tibble: 13 × 3
#>    id         time       values
#>    <chr>      <date>      <dbl>
#>  1 baro_small 2020-01-01   93.8
#>  2 baro_small 2020-04-01   66.0
#>  3 baro_small 2020-07-01  113. 
#>  4 baro_small 2020-10-01  107. 
#>  5 baro_small 2021-01-01  109. 
#>  6 baro_small 2021-04-01  125. 
#>  7 baro_small 2021-07-01  112. 
#>  8 baro_small 2021-10-01  104. 
#>  9 baro_small 2022-01-01   97.4
#> 10 baro_small 2022-04-01   94.3
#> 11 baro_small 2022-07-01   90.0
#> 12 baro_small 2022-10-01   90.7
#> 13 baro_small 2023-01-01   98.3
#> 
#> $parametric_optimization
#> NULL
#> 
#> $expalmon_weights
#> list()
#> 
#> $expalmon_parameters
#> list()
#> 
#> $expalmon_optimization
#> NULL
#> 
#> $truncation_info
#> $truncation_info$baro_small
#> $truncation_info$baro_small$indicator_id
#> [1] "baro_small"
#> 
#> $truncation_info$baro_small$n_periods
#> [1] 0
#> 
#> 
#> 
#> $regressor_names
#> [1] "baro_small"      "baro_small_lag1" "gdp_growth_lag1"
#> 
#> $xreg_names
#> [1] "baro_small"      "baro_small_lag1"
#> 
#> $target_anchor
#> [1] "2020-01-01"
#> 
#> $future_target_times
#> [1] "2023-01-01"
#> 
#> attr(,"class")
#> [1] "mf_model"
```
