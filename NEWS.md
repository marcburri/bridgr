# bridgr (development version)

* Rename the main model-construction entry point to `mf_model()`, rename the
  fitted-model class and S3 methods to `mf_model`, and keep `bridge()` as a
  deprecated compatibility wrapper.

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

* Remove the `legendre` parametric aggregation option.

* Use analytic gradients for `expalmon` optimization and improve the
  normalized beta polynomial gradient used in the optimizer.

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
