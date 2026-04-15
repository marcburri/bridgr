
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bridgr <a><img src="man/figures/logo.png" align="right" height="138"></a>

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/marcburri/bridgr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/marcburri/bridgr/actions/workflows/R-CMD-check.yaml)
[![Lint](https://github.com/marcburri/bridgr/actions/workflows/lint.yaml/badge.svg)](https://github.com/marcburri/bridgr/actions/workflows/lint.yaml)
[![bridgr status
badge](https://marcburri.r-universe.dev/badges/bridgr)](https://marcburri.r-universe.dev/bridgr)
[![CRAN
status](https://www.r-pkg.org/badges/version/bridgr)](https://CRAN.R-project.org/package=bridgr)
[![](https://cranlogs.r-pkg.org/badges/grand-total/bridgr)](https://CRAN.R-project.org/package=bridgr)
[![Codecov test
coverage](https://codecov.io/gh/marcburri/bridgr/graph/badge.svg)](https://app.codecov.io/gh/marcburri/bridgr)
<!-- badges: end -->

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
multiple aggregation choices including joint `expalmon` weighting. If a
target period contains more high-frequency observations than implied by
the current frequency mapping, the package keeps the most recent
observations and reports a summarized warning.

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

This is a basic example with the default mean aggregation:

``` r
if (requireNamespace("bridgr", quietly = TRUE)) {
  library(bridgr)
} else {
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
}
#> Warning in system("timedatectl", intern = TRUE): running command 'timedatectl'
#> had status 1

gdp <- suppressMessages(tsbox::ts_na_omit(tsbox::ts_pc(bridgr::gdp)))

bridge_model <- bridge(
  target = gdp, 
  indic = baro, 
  indic_predict = "auto.arima", 
  indic_aggregators = "mean",
  indic_lags = 2, 
  target_lags=1, 
  h=2
)
#> [value]: 'values' 
#> [value]: 'values'

forecast(bridge_model)
#>    Point Forecast      Lo 80    Hi 80      Lo 95    Hi 95
#> 74      0.8313868 -0.1302710 1.793045 -0.6393418 2.302115
#> 75      0.5363317 -0.4397745 1.512438 -0.9564939 2.029157

summary(bridge_model)
#> Bridge model summary
#> -----------------------------------
#> Target series: gdp
#> Target frequency: quarter (step 1)
#> Forecast horizon: 2
#> Formula: gdp ~ baro + baro_lag1 + baro_lag2
#> -----------------------------------
#> Main model:
#> -----------------------------------
#> Series: estimation_xts[, target_name] 
#> Regression with ARIMA(1,0,0) errors 
#> 
#> Coefficients:
#>          ar1  intercept    baro  baro_lag1  baro_lag2
#>       0.1740    -7.4164  0.1574    -0.0957     0.0172
#> s.e.  0.1312     1.4152  0.0126     0.0125     0.0127
#> 
#> sigma^2 = 0.5631:  log likelihood = -80.04
#> AIC=172.09   AICc=173.36   BIC=185.83
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
#> Aggregation: mean
#> -----------------------------------
```

If you want data-driven within-period weights instead of a simple mean,
you can switch the indicator aggregator to `expalmon`. The corresponding
weights are estimated jointly within the bridge model, and
`solver_options` let you control the optimization.

``` r
expalmon_model <- bridge(
  target = gdp,
  indic = baro,
  indic_predict = "auto.arima",
  indic_aggregators = "expalmon",
  solver_options = list(seed = 123, n_starts = 3),
  h = 1
)
#> [value]: 'values' 
#> [value]: 'values'

summary(expalmon_model)
#> Bridge model summary
#> -----------------------------------
#> Target series: gdp
#> Target frequency: quarter (step 1)
#> Forecast horizon: 1
#> Formula: gdp ~ baro
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
