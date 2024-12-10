
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bridgr <a><img src="man/figures/logo.png" align="right" height="138"></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/marcburri/bridgr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/marcburri/bridgr/actions/workflows/R-CMD-check.yaml)
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
#> The start dates of the target and indicator variables do not match. Aligning them to 2004-04-01
#> Dependent variable: gdp | Frequency: quarter | Estimation sample: 2004-04-01 - 2022-10-01 | Forecast horizon: 2 quarter(s)

forecast(bridge_model)
#>    Point Forecast      Lo 80    Hi 80      Lo 95    Hi 95
#> 74      0.8313868 -0.1302710 1.793045 -0.6393418 2.302115
#> 75      0.5363317 -0.4397745 1.512438 -0.9564939 2.029157

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
#>       0.1740    -7.4164  0.1574    -0.0957     0.0172
#> s.e.  0.1312     1.4152  0.0126     0.0125     0.0127
#> 
#> sigma^2 = 0.5631:  log likelihood = -80.04
#> AIC=172.09   AICc=173.36   BIC=185.83
#> -----------------------------------
#> Single indicator models:
#> -----------------------------------
#> Series:  baro 
#> ARIMA(1,0,2) with non-zero mean 
#> 
#> Coefficients:
#>          ar1     ma1     ma2      mean
#>       0.6688  0.5305  0.3316  100.8580
#> s.e.  0.0653  0.0799  0.0753    1.5774
#> 
#> sigma^2 = 18.46:  log likelihood = -646.14
#> AIC=1302.28   AICc=1302.55   BIC=1319.36
#> Aggregation to low frequency:
#> Using mean over values in corresponding periods.
#> -----------------------------------
```
