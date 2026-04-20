# Aggregation and Mixed-Frequency Modeling in bridgr

## The Bridge-to-MIDAS Spectrum

`bridgr` supports several ways to map higher-frequency observations into
a lower-frequency target equation. Those choices span a continuum:

- Bridge-style deterministic aggregation: `"mean"`, `"last"`, `"sum"`,
  or fixed numeric weights.
- Unrestricted mixed-frequency regression: `"unrestricted"`.
- Parametric MIDAS-style weighting: `"expalmon"` and `"beta"`.

This vignette uses a simple monthly-to-quarterly simulation so the
differences between these approaches are easy to see.

## A Small Monthly-to-Quarterly Example

``` r
n_quarters <- 40
quarter_index <- rep(seq_len(n_quarters), each = 3)
slot <- rep(1:3, times = n_quarters)
monthly_time <- seq(
  as.Date("2010-01-01"),
  by = "month",
  length.out = n_quarters * 3
)
monthly_indicator <- dplyr::tibble(
  time = monthly_time,
  value = 15 + quarter_index * 0.35 +
    ifelse(slot == 1, 0.8 * sin(quarter_index / 2), 0) +
    ifelse(slot == 2, -0.6 * cos(quarter_index / 3), 0) +
    ifelse(slot == 3, 0.7 * sin(quarter_index / 4 + 0.3), 0)
)

quarter_time <- monthly_time[seq(1, length(monthly_time), by = 3)]
quarter_target <- dplyr::tibble(
  time = quarter_time,
  value = 0.5 +
    vapply(
      seq_along(quarter_time),
      function(i) {
        block <- monthly_indicator$value[((i - 1) * 3 + 1):(i * 3)]
        0.2 * block[[1]] + 0.6 * block[[2]] + 0.2 * block[[3]]
      },
      numeric(1)
    ) +
    rep(c(0.1, -0.05, 0.08, -0.02), length.out = length(quarter_time))
)
```

The target is intentionally driven more by the middle month of each
quarter, so we can see how the different aggregation schemes react. The
monthly indicator also has slot-specific within-quarter movements, so
the unrestricted specification can estimate three distinct monthly
coefficients.

## Deterministic Bridge Aggregation

``` r
mean_model <- mf_model(
  target = quarter_target,
  indic = monthly_indicator,
  indic_predict = "last",
  indic_aggregators = "mean",
  h = 1
)

last_model <- mf_model(
  target = quarter_target,
  indic = monthly_indicator,
  indic_predict = "last",
  indic_aggregators = "last",
  h = 1
)

summary(mean_model)
#> Mixed-frequency model summary
#> -----------------------------------
#> Target series: quarter_target
#> Target frequency: quarter
#> Forecast horizon: 1
#> Estimation rows: 40
#> Regressors: monthly_indicator
#> -----------------------------------
#> Target equation coefficients:
#>                   Estimate
#> (Intercept)          0.516
#> monthly_indicator    0.999
#> -----------------------------------
#> Model fit:
#>  Statistic               Value
#>  R-squared               0.999
#>  Adjusted R-squared      0.999
#>  Residual standard error 0.142
#> -----------------------------------
#> Indicator summary:
#>                   Frequency Predict Aggregation
#> monthly_indicator month     last    mean       
#> -----------------------------------
summary(last_model)
#> Mixed-frequency model summary
#> -----------------------------------
#> Target series: quarter_target
#> Target frequency: quarter
#> Forecast horizon: 1
#> Estimation rows: 40
#> Regressors: monthly_indicator
#> -----------------------------------
#> Target equation coefficients:
#>                   Estimate
#> (Intercept)          0.590
#> monthly_indicator    0.993
#> -----------------------------------
#> Model fit:
#>  Statistic               Value
#>  R-squared               0.993
#>  Adjusted R-squared      0.993
#>  Residual standard error 0.337
#> -----------------------------------
#> Indicator summary:
#>                   Frequency Predict Aggregation
#> monthly_indicator month     last    last       
#> -----------------------------------
```

These are classic bridge-model choices. They are easy to interpret and
often work well when you want a transparent rule for within-period
aggregation.

## Unrestricted Mixed-Frequency Regression

``` r
unrestricted_model <- mf_model(
  target = quarter_target,
  indic = monthly_indicator,
  indic_predict = "last",
  indic_aggregators = "unrestricted",
  h = 1
)

summary(unrestricted_model)
#> Mixed-frequency model summary
#> -----------------------------------
#> Target series: quarter_target
#> Target frequency: quarter
#> Forecast horizon: 1
#> Estimation rows: 40
#> Regressors: monthly_indicator_hf1, monthly_indicator_hf2, monthly_indicator_hf3
#> -----------------------------------
#> Target equation coefficients:
#>                       Estimate
#> (Intercept)              0.541
#> monthly_indicator_hf1    0.198
#> monthly_indicator_hf2    0.596
#> monthly_indicator_hf3    0.205
#> -----------------------------------
#> Model fit:
#>  Statistic               Value
#>  R-squared               1.000
#>  Adjusted R-squared      1.000
#>  Residual standard error 0.067
#> -----------------------------------
#> Indicator summary:
#>                   Frequency Predict Aggregation 
#> monthly_indicator month     last    unrestricted
#> -----------------------------------
```

`"unrestricted"` estimates one coefficient for each within-quarter
monthly observation. In a monthly-on-quarterly example this means three
separate regressors. This example keeps `indic_predict = "last"` so the
comparison isolates the aggregation choice. The ragged-edge vignette
covers `indic_predict = "direct"`, which skips indicator forecasting and
instead works from the latest observed complete high-frequency blocks.

## Parametric MIDAS-Style Weighting

`bridgr` also supports parametric weighting rules that estimate the
within-period shape from the data while keeping the number of free
parameters small.

``` r
expalmon_model <- mf_model(
  target = quarter_target,
  indic = monthly_indicator,
  indic_predict = "last",
  indic_aggregators = "expalmon",
  solver_options = list(seed = 123, n_starts = 1, maxiter = 100),
  h = 1
)

beta_model <- mf_model(
  target = quarter_target,
  indic = monthly_indicator,
  indic_predict = "last",
  indic_aggregators = "beta",
  solver_options = list(
    seed = 123,
    n_starts = 1,
    maxiter = 100,
    start_values = c(2, 2)
  ),
  h = 1
)
```

The fitted object stores both the estimated weight profile and the
underlying parametric coefficients.

``` r
indicator_id <- expalmon_model$indic_name[[1]]

dplyr::bind_rows(
  dplyr::tibble(
    model = "expalmon",
    month = seq_along(expalmon_model$parametric_weights[[indicator_id]]),
    weight = expalmon_model$parametric_weights[[indicator_id]]
  ),
  dplyr::tibble(
    model = "beta",
    month = seq_along(beta_model$parametric_weights[[indicator_id]]),
    weight = beta_model$parametric_weights[[indicator_id]]
  )
)
#> # A tibble: 6 × 3
#>   model    month    weight
#>   <chr>    <int>     <dbl>
#> 1 expalmon     1 1.99 e- 1
#> 2 expalmon     2 5.97 e- 1
#> 3 expalmon     3 2.05 e- 1
#> 4 beta         1 8.88 e-16
#> 5 beta         2 1.000e+ 0
#> 6 beta         3 8.88 e-16
```

## Forecast Comparison

All of these models share the same downstream interface.

``` r
dplyr::bind_rows(
  dplyr::tibble(
    model = "mean",
    forecast = as.numeric(forecast(mean_model)$mean)
  ),
  dplyr::tibble(
    model = "last",
    forecast = as.numeric(forecast(last_model)$mean)
  ),
  dplyr::tibble(
    model = "unrestricted",
    forecast = as.numeric(forecast(unrestricted_model)$mean)
  ),
  dplyr::tibble(
    model = "expalmon",
    forecast = as.numeric(forecast(expalmon_model)$mean)
  ),
  dplyr::tibble(
    model = "beta",
    forecast = as.numeric(forecast(beta_model)$mean)
  )
)
#> # A tibble: 5 × 2
#>   model        forecast
#>   <chr>           <dbl>
#> 1 mean             29.0
#> 2 last             28.9
#> 3 unrestricted     29.0
#> 4 expalmon         29.0
#> 5 beta             29.0
```

## Choosing an Aggregation Strategy

As a rough guide:

- Use deterministic bridge aggregation like “mean”, “last”, “sum” when
  you want a transparent and stable nowcasting rule.
- Use `"unrestricted"` when the frequency gap is small and you want each
  within-period observation to have its own coefficient.
- Use `"expalmon"` or `"beta"` when you want data-driven within-period
  weights but would like a more parsimonious parameterization than
  `"unrestricted"`.
- Use `indic_predict = "direct"` when you want direct alignment based
  only on the latest observed complete high-frequency blocks.

The key design choice in `bridgr` is that all of these specifications
share the same estimation, forecasting, and summary workflow. You can
therefore move between classic bridge models, unrestricted
mixed-frequency regressions, and parametric MIDAS-style models without
switching to a different API.
