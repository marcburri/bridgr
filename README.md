
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bridgr

<!-- badges: start -->
<!-- badges: end -->

`bridgr` is an R package designed to simplify the implementation and
evaluation of bridge models, which are useful in nowcasting and
forecasting frameworks.

## Installation

You can install the development version of bridgr like so:

``` r
# install.packages("devtools")
devtools::install_github("marcburri/bridgr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(bridgr)

bridge_instance <- bridge(target = gdp, 
                          indic = fcurve, 
                          indic_predict = "mean", 
                          indic_lags = 2, 
                          target_lags=2, 
                          h=1
                          )
#> Dependent variable: gdp | Frequency: quarter | Estimation sample: 2004-01-01 - 2022-10-01 | Forecast horizon: 1 quarter(s)
```
