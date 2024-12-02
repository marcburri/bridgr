
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bridgr <a><img src="man/figures/logo.png" align="right" height="138"></a>

<!-- badges: start -->
<!-- badges: end -->

`bridgr` is an R package designed to simplify the implementation and
evaluation of bridge models, which are useful in nowcasting and
forecasting frameworks. The package is still under development and is
not yet available on CRAN.

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
#> 
#> Attaching package: 'bridgr'
#> The following object is masked from 'package:base':
#> 
#>     summary

bridge_model <- bridge(
  target = gdp, 
  indic = baro, 
  indic_predict = "auto.arima", 
  indic_lags = 2, 
  target_lags=1, 
  h=2
)
#> Dependent variable: gdp | Frequency: quarter | Estimation sample: 2004-01-01 - 2022-10-01 | Forecast horizon: 2 quarter(s)

forecast(bridge_model)
#>    Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#> 75       193560.2 191189.5 195930.9 189934.6 197185.8
#> 76       194153.5 190801.0 197506.0 189026.3 199280.8
```
