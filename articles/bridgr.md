# Getting Started with bridgr

## Overview

`bridgr` is a mixed-frequency forecasting package for bridge models,
MIDAS-style regressions, and intermediate specifications that estimate
within-period weights from the data.

The core workflow is always the same:

1.  Provide one lower-frequency target series and one or more
    higher-frequency indicators.
2.  Decide how missing indicator observations should be handled with
    `indic_predict`.
3.  Decide how indicators should be aligned to the target frequency with
    `indic_aggregators`.
4.  Fit the target equation with
    [`bridge()`](https://marcburri.github.io/bridgr/reference/bridge.md),
    inspect it with [`summary()`](https://rdrr.io/r/base/summary.html),
    and produce target-period forecasts with
    [`forecast()`](https://generics.r-lib.org/reference/forecast.html).

This vignette walks through that workflow with the package’s built-in
Swiss GDP and barometer data.

## Example Data

``` r
gdp_growth <- suppressMessages(tsbox::ts_na_omit(tsbox::ts_pc(gdp)))

head(gdp_growth)
#> # A tibble: 6 × 2
#>   time       values
#>   <date>      <dbl>
#> 1 2004-04-01  0.839
#> 2 2004-07-01 -0.104
#> 3 2004-10-01  0.242
#> 4 2005-01-01  0.860
#> 5 2005-04-01  1.06 
#> 6 2005-07-01  1.15
head(baro)
#> # A tibble: 6 × 2
#>   time       values
#>   <date>      <dbl>
#> 1 2004-01-01   109.
#> 2 2004-02-01   108.
#> 3 2004-03-01   109.
#> 4 2004-04-01   110.
#> 5 2004-05-01   109.
#> 6 2004-06-01   105.
```

`gdp_growth` is quarterly, while `baro` is monthly.
[`bridge()`](https://marcburri.github.io/bridgr/reference/bridge.md)
recognizes the frequency mismatch automatically and aligns the indicator
to the target frequency before fitting the target equation.

## A Basic Bridge Model

``` r
bridge_model <- bridge(
  target = gdp_growth,
  indic = baro,
  indic_predict = "auto.arima",
  indic_aggregators = "mean",
  indic_lags = 1,
  target_lags = 1,
  h = 2
)

forecast(bridge_model)
#> Bridge forecast
#> -----------------------------------
#> Target series: gdp_growth
#> Forecast horizon: 2
#> Target model: fc_model
#> Uncertainty: point forecast only
#> -----------------------------------
#> # A tibble: 2 × 7
#>   time        mean    se lower_80 upper_80 lower_95 upper_95
#>   <date>     <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#> 1 2023-01-01 0.972    NA       NA       NA       NA       NA
#> 2 2023-04-01 0.698    NA       NA       NA       NA       NA
```

The default mean aggregator is the classic bridge-model setup: each
monthly block is completed first, then averaged to the quarterly
frequency before the target equation is estimated.

The fitted object stores the aligned data that went into estimation and
the future target-period regressor path used for forecasting.

``` r
tail(bridge_model$estimation_set)
#> # A tibble: 6 × 4
#>   time       gdp_growth  baro baro_lag1
#>   <date>          <dbl> <dbl>     <dbl>
#> 1 2021-07-01      2.34  112.      125. 
#> 2 2021-10-01      0.411 104.      112. 
#> 3 2022-01-01      0.105  97.4     104. 
#> 4 2022-04-01      1.03   94.3      97.4
#> 5 2022-07-01      0.255  90.0      94.3
#> 6 2022-10-01      0.102  90.7      90.0
bridge_model$forecast_set
#> # A tibble: 2 × 3
#>   time        baro baro_lag1
#>   <date>     <dbl>     <dbl>
#> 1 2023-01-01  97.4      90.7
#> 2 2023-04-01  99.8      97.4
```

## Standardized Output

[`summary()`](https://rdrr.io/r/base/summary.html) and
[`forecast()`](https://generics.r-lib.org/reference/forecast.html) use a
stable package-specific layout. The base output is the same across
bridge, MIDAS-style, and direct-alignment specifications. Additional
details, such as optimization summaries or bootstrap settings, are
appended only when they are relevant.

``` r
summary(bridge_model)
#> Bridge model summary
#> -----------------------------------
#> Target series: gdp_growth
#> Target frequency: quarter (step 1)
#> Forecast horizon: 2
#> Target model: fc_model
#> Estimation rows: 74
#> Regressors: baro, baro_lag1
#> -----------------------------------
#> Target equation coefficients:
#> # A tibble: 4 × 3
#>   term      estimate bootstrap_se
#>   <chr>        <dbl>        <dbl>
#> 1 ar1         0.243            NA
#> 2 intercept  -6.49             NA
#> 3 baro        0.161            NA
#> 4 baro_lag1  -0.0917           NA
#> -----------------------------------
#> Indicator summary:
#> # A tibble: 1 × 5
#>   indicator frequency      predict    aggregation indicator_model
#>   <chr>     <chr>          <chr>      <chr>       <chr>          
#> 1 baro      month (step 1) auto.arima mean        fc_model       
#> -----------------------------------
#> Uncertainty:
#> Method: none
#> -----------------------------------
```

## Direct Alignment

If you set `indic_predict = "direct"`, `bridgr` switches from indicator
forecasting to direct MIDAS-style alignment. In that case, the latest
complete high-frequency blocks are assigned backward to target periods
instead of being forecast forward first.

``` r
direct_model <- bridge(
  target = gdp_growth,
  indic = baro,
  indic_predict = "direct",
  indic_aggregators = "unrestricted",
  h = 1
)

forecast(direct_model)
#> Bridge forecast
#> -----------------------------------
#> Target series: gdp_growth
#> Forecast horizon: 1
#> Target model: lm
#> Indicator handling: direct alignment
#> Uncertainty: point forecast only
#> -----------------------------------
#> # A tibble: 1 × 7
#>   time        mean    se lower_80 upper_80 lower_95 upper_95
#>   <date>     <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#> 1 2023-01-01 0.522    NA       NA       NA       NA       NA
```

This is particularly useful at the ragged edge when you want to work
only with observed high-frequency information and avoid a separate
indicator forecasting step.

## Optional Bootstrap Uncertainty

By default, `bridgr` returns point forecasts only. If you want
uncertainty output, request it at estimation time with `se = TRUE` and a
bootstrap configuration.

``` r
boot_model <- bridge(
  target = gdp_growth,
  indic = baro,
  indic_predict = "auto.arima",
  indic_aggregators = "mean",
  target_lags = 1,
  h = 1,
  se = TRUE,
  bootstrap = list(type = "block", N = 100, block_length = NULL)
)

forecast(boot_model)
summary(boot_model)
```

The current uncertainty implementation is a conditional block bootstrap
on the final target-frequency estimation sample. It does not re-estimate
indicator forecast models or parametric aggregation weights inside each
bootstrap draw.

## Where to Go Next

The vignette
[`vignette("mixed-frequency-modeling", package = "bridgr")`](https://marcburri.github.io/bridgr/articles/mixed-frequency-modeling.md)
compares the main aggregation strategies and shows how `bridgr` moves
from classic bridge models to unrestricted and parametric MIDAS-style
specifications.

The vignette
[`vignette("ragged-edge-nowcasting", package = "bridgr")`](https://marcburri.github.io/bridgr/articles/ragged-edge-nowcasting.md)
focuses on `indic_predict` and the different ways to handle incomplete
high-frequency data at the forecast origin.

The vignette
[`vignette("uncertainty-and-scenarios", package = "bridgr")`](https://marcburri.github.io/bridgr/articles/uncertainty-and-scenarios.md)
shows how to work with conditional block-bootstrap uncertainty and
scenario forecasts based on custom future regressor paths.
