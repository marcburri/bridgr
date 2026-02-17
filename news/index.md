# Changelog

## bridgr (development version)

## bridgr 0.1.2

- Solve dependency issues with xts

## bridgr 0.1.1

CRAN release: 2024-12-13

- Initial CRAN submission:
  - Added `gdp`,`baro`, `wea` and `fcurve` datasets.

  - Added
    [`bridge()`](https://marcburri.github.io/bridgr/reference/bridge.md),
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
