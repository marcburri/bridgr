
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bridgr <a><img src="man/figures/logo.png" align="right" height="138"></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/marcburri/bridgr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/marcburri/bridgr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`bridgr` is an R package designed to simplify the implementation and
evaluation of bridge models, which are useful in nowcasting and
forecasting frameworks.

The package is still under development and is not yet available on CRAN.
Therefore, the package is not yet stable and may contain bugs.

## Installation

You can install the development version of `bridgr` like so:

``` r
# install.packages("devtools")
devtools::install_github("marcburri/bridgr")
```

## Example

This is a basic example:

``` r
library(bridgr)

gdp <- suppressMessages(tsbox::ts_pc(bridgr::gdp))

bridge_model <- bridge(
  target = gdp, 
  indic = baro, 
  indic_predict = "auto.arima", 
  indic_lags = 2, 
  target_lags=1, 
  h=2
)
#> Dependent variable: gdp | Frequency: quarter | Estimation sample: 2004-04-01 - 2022-10-01 | Forecast horizon: 2 quarter(s)

forecast(bridge_model)
#>    Point Forecast      Lo 80    Hi 80      Lo 95    Hi 95
#> 75      0.8359963 -0.1209672 1.792960 -0.6275531 2.299546
#> 76      0.5418210 -0.4300031 1.513645 -0.9444557 2.028098

summary(bridge_model)
#> Bridge model summary
#> -----------------------------------
#> Main model:
#> -----------------------------------
#> Series:  gdp 
#> Regression with ARIMA(1,0,0) errors 
#> 
#> Coefficients:
#>          ar1  intercept    baro  baro_lag1  baro_lag2
#>       0.1769     -7.342  0.1575    -0.0962     0.0168
#> s.e.  0.1301      1.407  0.0125     0.0124     0.0126
#> 
#> sigma^2 = 0.5576:  log likelihood = -80.82
#> AIC=173.63   AICc=174.89   BIC=187.46
#> -----------------------------------
#> Single indicator models:
#> -----------------------------------
#> Series:  baro 
#> ARIMA(1,0,2) with non-zero mean 
#> 
#> Coefficients:
#>          ar1     ma1     ma2      mean
#>       0.6719  0.5277  0.3281  100.9338
#> s.e.  0.0645  0.0793  0.0749    1.5678
#> 
#> sigma^2 = 18.25:  log likelihood = -653.45
#> AIC=1316.91   AICc=1317.18   BIC=1334.05
#> Aggregation to low frequency:
#> Using mean over values in corresponding periods.
#> -----------------------------------
```
