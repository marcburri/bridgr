# Changelog

## bridgr (development version)

- Rename the main model-construction entry point to
  [`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md),
  rename the fitted-model class and S3 methods to `mf_model`, and keep
  [`bridge()`](https://marcburri.github.io/bridgr/reference/mf_model.md)
  as a deprecated compatibility wrapper.

- Extend
  [`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md)
  beyond classic bridge aggregation:

  - add unrestricted mixed-frequency regressors via
    `indic_aggregators = "unrestricted"`
  - add parametric `"beta"` weighting alongside `"expalmon"`
  - add direct high-frequency alignment via `indic_predict = "direct"`
  - support fixed numeric aggregation weights supplied in a
    [`list()`](https://rdrr.io/r/base/list.html)

- Improve mixed-frequency input handling:

  - infer regular frequencies from `second` through `year`
  - allow custom `frequency_conversions`
  - standardize month-, quarter-, and year-end dates to period starts
    when needed for frequency recognition
  - keep the most recent observations in overfilled target periods with
    a summarized warning
  - fail when target periods contain too few high-frequency observations

- Add joint parametric aggregation optimization controls through
  `solver_options`, including optimizer choice, multi-start runs, seeds,
  iteration limits, and user-supplied starting values.

- Add uncertainty support:

  - `se = TRUE` for coefficient uncertainty and prediction intervals
  - HAC standard errors for linear bridge equations
  - Delta-HAC standard errors when parametric aggregation weights are
    estimated jointly
  - residual-resampling prediction intervals by default
  - optional full-system block bootstrap uncertainty via
    `full_system_bootstrap = TRUE`

- Add scenario forecasting support in
  [`forecast.mf_model()`](https://marcburri.github.io/bridgr/reference/forecast.mf_model.md)
  through custom future `xreg` paths and standardized forecast objects
  with uncertainty metadata.

- Add plotting methods and helpers:

  - [`plot.mf_model()`](https://marcburri.github.io/bridgr/reference/plot.mf_model.md)
    for fit and forecast plots
  - [`theme_bridgr()`](https://marcburri.github.io/bridgr/reference/theme_bridgr.md),
    [`colors_bridgr()`](https://marcburri.github.io/bridgr/reference/theme_bridgr.md),
    [`scale_color_bridgr()`](https://marcburri.github.io/bridgr/reference/theme_bridgr.md),
    and
    [`scale_fill_bridgr()`](https://marcburri.github.io/bridgr/reference/theme_bridgr.md)

- Expand printed output and documentation:

  - standardize
    [`summary.mf_model()`](https://marcburri.github.io/bridgr/reference/summary.mf_model.md)
    and
    [`forecast.mf_model()`](https://marcburri.github.io/bridgr/reference/forecast.mf_model.md)
    output
  - add vignettes on mixed-frequency modeling, ragged-edge nowcasting,
    and uncertainty / scenario analysis
  - refresh the README examples and package references

- Remove the `legendre` parametric aggregation option.

- Use analytic gradients for `expalmon` optimization and improve the
  normalized beta polynomial gradient used in the optimizer.

## bridgr 0.1.2

CRAN release: 2026-02-18

- Solve dependency issues with xts

## bridgr 0.1.1

CRAN release: 2024-12-13

- Initial CRAN submission:
  - Added `gdp`,`baro`, `wea` and `fcurve` datasets.

  - Added
    [`bridge()`](https://marcburri.github.io/bridgr/reference/mf_model.md),
    [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
    and [`summary()`](https://rdrr.io/r/base/summary.html) functions.

  - Supports target variables on monthly, quarterly and yearly
    frequency, and indicator variables on daily, weekly, monthly,
    quarterly and yearly frequency.

  - Supports `auto.arima`, `ets` and other methods for indicator
    variable forecasting.

  - Supports aggregation of indicator variables to match the target’s
    frequency using custom weighting functions, exponential Almon
    polynomials and other methods.
