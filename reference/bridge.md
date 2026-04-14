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
  `1` or equal to the number of indicator series.

- indic_aggregators:

  A character vector of aggregation methods or a list of numeric
  weights. Length must be `1` or equal to the number of indicator
  series. Numeric weights must sum to one and have the appropriate
  length for the inferred target-period block size.

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

- solver_options:

  A list of optional controls for joint `expalmon` optimization.
  Supported entries are `method`, `maxiter`, `n_starts`, `seed`, and
  `trace`. These controls are ignored unless at least one indicator uses
  `indic_aggregators = "expalmon"`.

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
`"auto.arima"`, and `"ets"`. Supported aggregation methods are `"mean"`,
`"last"`, `"sum"`, `"expalmon"`, or a numeric weight vector supplied
inside a [`list()`](https://rdrr.io/r/base/list.html). When one or more
indicators use `"expalmon"`, the corresponding aggregation weights are
estimated jointly against the final bridge-model objective rather than
one indicator at a time.

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

## References

Baffigi, A., Golinelli, R., & Parigi, G. (2004). Bridge models to
forecast the euro area GDP. *International Journal of Forecasting*,
20(3), 447-460.
[doi:10.1016/S0169-2070(03)00067-0](https://doi.org/10.1016/S0169-2070%2803%2900067-0)

Burri, M. (2023). Do daily lead texts help nowcasting GDP growth? IRENE
Working Papers 23-02.
<https://www5.unine.ch/RePEc/ftp/irn/pdfs/WP23-02.pdf>

Schumacher, C. (2016). A comparison of MIDAS and bridge equations.
*International Journal of Forecasting*, 32(2), 257-270.
[doi:10.1016/j.ijforecast.2015.07.004](https://doi.org/10.1016/j.ijforecast.2015.07.004)

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
#> [value]: 'values' 
#> [value]: 'values' 

expalmon_model <- bridge(
  target = gdp_growth,
  indic = baro,
  indic_predict = "auto.arima",
  indic_aggregators = "expalmon",
  solver_options = list(seed = 123, n_starts = 3),
  h = 1
)
#> [value]: 'values' 
#> [value]: 'values' 

forecast(model)
#>    Point Forecast      Lo 80    Hi 80      Lo 95    Hi 95
#> 75      0.9724040  0.0112021 1.933606 -0.4976274 2.442435
#> 76      0.6984187 -0.2906595 1.687497 -0.8142460 2.211083
summary(expalmon_model)
#> Bridge model summary
#> -----------------------------------
#> Target series: gdp_growth
#> Target frequency: quarter (step 1)
#> Forecast horizon: 1
#> Formula: gdp_growth ~ baro
#> -----------------------------------
#> Main model:
#> -----------------------------------
#> Series: estimation_xts[, target_name] 
#> Regression with ARIMA(0,0,0) errors 
#> 
#> Coefficients:
#>       intercept    baro
#>         -9.3713  0.0982
#> s.e.     1.0730  0.0106
#> 
#> sigma^2 = 0.8333:  log likelihood = -98.57
#> AIC=203.14   AICc=203.48   BIC=210.09
#> -----------------------------------
#> Indicator models:
#> -----------------------------------
#> Series: baro
#> Frequency: month (step 1)
#> Forecast method: auto.arima
#> Series: xts_series 
#> ARIMA(1,0,2) with non-zero mean 
#> 
#> Coefficients:
#>          ar1     ma1     ma2      mean
#>       0.6688  0.5305  0.3316  100.8580
#> s.e.  0.0653  0.0799  0.0753    1.5774
#> 
#> sigma^2 = 18.46:  log likelihood = -646.14
#> AIC=1302.28   AICc=1302.55   BIC=1319.36
#> Aggregation: expalmon
#> Estimated expalmon weights: 0.006, 0.994, 0
#> Estimated expalmon parameters: -4.914, -10
#> -----------------------------------
#> Joint expalmon optimization:
#> -----------------------------------
#> Method: L-BFGS-B
#> Objective value: 60.8316
#> Convergence code: 0
#> Best start: 1 / 3
#> Message: CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH
#> -----------------------------------
```
