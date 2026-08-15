# bridgr 1.0.0

First stable release. Since 0.1.2 the model-construction entry point and the
fitted-model class have been renamed, the aggregation library and the
uncertainty machinery have been substantially extended, and two methods that
did not honour their documented contract have been corrected. The public
interface centred on `mf_model()`, `forecast()`, `summary()` and `plot()` is
now considered stable, and future breaking changes will go through a
deprecation cycle.

## Breaking changes

* Rename the main model-construction entry point to `mf_model()`, and rename
  the fitted-model class and its S3 methods from `bridge` to `mf_model`.
  `bridge()` remains as a deprecated compatibility wrapper that warns and
  forwards to `mf_model()`, so existing code keeps working.

* `summary()` on an `"mf_model"` object now returns a `"summary.mf_model"`
  object instead of printing and returning the model unchanged, following the
  convention of `summary.lm()`. The printed report is unchanged and is now
  produced by the new `print.summary.mf_model()` method. The returned object
  exposes the summary quantities programmatically, including a standard
  `coefficients` matrix with `Estimate`, `Std. Error`, `t value` and
  `Pr(>|t|)` columns, so `coef(summary(model))` works as it does for `lm()`.
  Standard errors respect the HAC, Delta-HAC or bootstrap covariance when the
  model was fitted with `se = TRUE`.

* Objects returned by `forecast()` no longer inherit from the `forecast`
  package's `"forecast"` class; they are now plain `"mf_model_forecast"`
  objects. The previous inheritance was not honoured -- `plot()` and
  `autoplot()` failed on the result, and `accuracy()` returned misleading
  values -- because target frequencies such as daily and weekly cannot be
  represented by `stats::ts()`, which those methods require. `plot()` and
  `ggplot2::autoplot()` methods are now provided directly for
  `"mf_model_forecast"` and work at every supported target frequency, and the
  new `as.forecast()` converts to a genuine `"forecast"` object for use with
  functions such as `forecast::accuracy()` whenever the target frequency has
  an exact `ts` representation (annual, semi-annual, quarterly, bi-monthly or
  monthly).

* Remove the `legendre` parametric aggregation option.

## New features

* New accessor methods replace reaching into the fitted object's internal
  structure: `weights()` returns aggregation weights (estimated parametric
  weights or user-supplied numeric weights), `aggregation_parameters()`
  returns the estimated parameters of parametric aggregation schemes,
  `indicators()` returns the indicator names, `variable.names()` returns the
  bridge-equation regressor names, and `model.frame()` returns the estimation
  data or the forecast regressor path. `weights()` and
  `aggregation_parameters()` accept an indicator name or position. The
  vignettes now use these accessors throughout.

* `variable.names()` replaces `model$xreg_names` and `model$regressor_names`.
  `variable.names(model, which = "xreg")` returns the non-target-lag
  regressors, which are exactly the series a custom `xreg` must supply when
  forecasting a scenario, and so pairs with
  `model.frame(model, which = "forecast")`.

* `weights()` now also returns the fixed weight vectors implied by the
  deterministic aggregators, rather than `NULL`: `"mean"` gives `1/M`,
  `"last"` gives a one in the final slot, and `"sum"` gives ones. The
  accessor therefore reports the weights actually applied for every
  aggregator except `"unrestricted"`, which estimates one coefficient per
  within-period observation and so implies no weight vector.

* Extend `mf_model()` beyond classic bridge aggregation:
  - add unrestricted mixed-frequency regressors via
    `indic_aggregators = "unrestricted"`
  - add parametric `"beta"` weighting alongside `"expalmon"`
  - add direct high-frequency alignment via `indic_predict = "direct"`
  - support fixed numeric aggregation weights supplied in a `list()`

* Improve mixed-frequency input handling:
  - infer regular frequencies from `second` through `year`
  - allow custom `frequency_conversions`
  - standardize month-, quarter-, and year-end dates to period starts when
    needed for frequency recognition
  - keep the most recent observations in overfilled target periods with a
    summarized warning
  - fail when target periods contain too few high-frequency observations

* Add joint parametric aggregation optimization controls through
  `solver_options`, including optimizer choice, multi-start runs, seeds,
  iteration limits, and user-supplied starting values.

* Add uncertainty support:
  - `se = TRUE` for coefficient uncertainty and prediction intervals
  - HAC standard errors for linear bridge equations
  - Delta-HAC standard errors when parametric aggregation weights are
    estimated jointly
  - residual-resampling prediction intervals by default
  - optional full-system block bootstrap uncertainty via
    `full_system_bootstrap = TRUE`

* Add scenario forecasting support in `forecast.mf_model()` through custom
  future `xreg` paths and standardized forecast objects with uncertainty
  metadata.

* Add plotting methods and helpers:
  - `plot.mf_model()` for fit and forecast plots
  - `theme_bridgr()`, `colors_bridgr()`, `scale_color_bridgr()`, and
    `scale_fill_bridgr()`

* Expand printed output and documentation:
  - standardize `summary.mf_model()` and `forecast.mf_model()` output
  - add vignettes on mixed-frequency modeling, ragged-edge nowcasting, and
    uncertainty / scenario analysis
  - refresh the README examples and package references

## Performance

* The full-system block bootstrap is substantially faster. Calendar shifts
  were applied one step at a time and recomputed for every observation, which
  made timezone normalisation inside `lubridate::%m+%` the dominant cost of a
  bootstrap resample. Shifts are now vectorised and computed once per distinct
  shift amount. On a quarterly target with a monthly indicator, a 50-draw
  full-system bootstrap runs about 3.6 times faster, with bit-identical
  coefficients and forecasts.

* Month, quarter and year shifts of `Date` vectors no longer go through
  `lubridate::%m+%`, which routes through `as.POSIXlt()` and `force_tz()`.
  Profiling showed that timezone coercion alone accounted for roughly 44% of
  the remaining self time in a full-system bootstrap. These shifts now use
  direct integer calendar arithmetic, preserving the end-of-month rollback
  semantics of `%m+%` exactly; `POSIXct` inputs, missing values and
  fractional shifts still use `%m+%`. The isolated shift is about 10 times
  faster and a 50-draw full-system bootstrap about 1.3 times faster, with
  bit-identical coefficients, covariances, forecasts and intervals.

* Use analytic gradients for `expalmon` optimization and improve the
  normalized beta polynomial gradient used in the optimizer.

## Bug fixes

* Fix `mf_model()` failing when the 'xts' package is not installed. Indicator
  forecasting with `indic_predict = "auto.arima"` (the default) or `"ets"`
  routed the series through `tsbox::ts_xts()`, which requires 'xts', so the
  default code path errored for users without it even though 'xts' was only a
  suggested dependency. The fitters are now given the indicator observations
  directly. Results are unchanged: the 'xts' index carried no `tsp` attribute,
  so both fitters already saw a frequency-1 series at every supported
  indicator frequency. 'xts' is no longer a dependency of any kind.

* Fix ragged-edge completion for sub-monthly indicators at multi-step
  horizons (`h > 1`). Completion previously filled future target periods with
  a fixed count of high-frequency grid steps, but calendar periods can hold
  more observations than the regular ladder implies (a quarter has 13 weekly
  or up to 92 daily observations versus the 12 or 84 the ladder expects), so
  early future periods absorbed the surplus and later ones failed block
  validation. Completion is now period-aware: candidate grid times are
  assigned to their calendar periods and each future period receives exactly
  the observations it still needs.

* Ignore indicator observations dated beyond the last forecast period during
  alignment. Such observations cannot enter any regressor and previously made
  block validation fail on a partially observed beyond-horizon period, for
  example when a weekly series extends past the target quarter of an `h = 1`
  nowcast.

* Make direct alignment (`indic_predict = "direct"`) period-aware. Blocks of
  high-frequency observations were strided backward from the end of the
  sample and paired with target periods by position, so on calendar ladders
  (13-Saturday quarters on a 12-slot weekly ladder) historical blocks drifted
  out of their calendar periods -- about one week per quarter, compounding
  over the sample -- and the stride count could overrun the number of target
  periods and fail outright. Each target period that overlaps the observed
  sample is now anchored at the newest observation's position within its own
  period (a MIDAS-with-leads alignment), which reproduces fixed strides
  exactly on regular ladders; periods beyond the observed sample keep the
  documented lead convention.

# bridgr 0.1.2

* Solve dependency issues with xts


# bridgr 0.1.1

* Initial CRAN submission:
  - Added `gdp`,`baro`, `wea` and `fcurve` datasets.

  - Added `bridge()`, `forecast()` and `summary()` functions.
 
  - Supports target variables on monthly, quarterly and yearly frequency, and 
    indicator variables on daily, weekly, monthly, quarterly and yearly frequency. 
    
  - Supports `auto.arima`, `ets` and other methods for indicator variable forecasting.
 
  - Supports aggregation of indicator variables to match the target's frequency using
    custom weighting functions, exponential Almon polynomials and other methods.
