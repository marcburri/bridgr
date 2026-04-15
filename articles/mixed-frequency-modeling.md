# Aggregation and Mixed-Frequency Modeling in bridgr

## The Bridge-to-MIDAS Spectrum

`bridgr` supports several ways to map higher-frequency observations into
a lower-frequency target equation. Those choices span a continuum:

- Bridge-style deterministic aggregation: `"mean"`, `"last"`, `"sum"`,
  or fixed numeric weights.
- Unrestricted mixed-frequency regression: `"unrestricted"`.
- Parametric MIDAS-style weighting: `"expalmon"`, `"beta"`, and
  `"legendre"`.

This vignette uses a simple monthly-to-quarterly simulation so the
differences between these approaches are easy to see.

## A Small Monthly-to-Quarterly Example

``` r
monthly_time <- seq(as.Date("2018-01-01"), by = "month", length.out = 36)
monthly_indicator <- dplyr::tibble(
  time = monthly_time,
  value = 15 + seq_len(36) * 0.4 +
    rep(c(0.8, -0.3, 1.2), length.out = 36)
)

quarter_time <- monthly_time[seq(1, 36, by = 3)]
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
quarter, so we can see how the different aggregation schemes react.

## Deterministic Bridge Aggregation

``` r
mean_model <- bridge(
  target = quarter_target,
  indic = monthly_indicator,
  indic_predict = "last",
  indic_aggregators = "mean",
  h = 1
)

last_model <- bridge(
  target = quarter_target,
  indic = monthly_indicator,
  indic_predict = "last",
  indic_aggregators = "last",
  h = 1
)

summary(mean_model)
#> Bridge model summary
#> -----------------------------------
#> Target series: quarter_target
#> Target frequency: quarter (step 1)
#> Forecast horizon: 1
#> Target model: lm
#> Estimation rows: 12
#> Regressors: monthly_indicator
#> -----------------------------------
#> Target equation coefficients:
#> # A tibble: 2 × 3
#>   term              estimate bootstrap_se
#>   <chr>                <dbl>        <dbl>
#> 1 (Intercept)          0.227           NA
#> 2 monthly_indicator    0.998           NA
#> -----------------------------------
#> Indicator summary:
#> # A tibble: 1 × 5
#>   indicator         frequency      predict aggregation indicator_model
#>   <chr>             <chr>          <chr>   <chr>       <chr>          
#> 1 monthly_indicator month (step 1) last    mean        deterministic  
#> -----------------------------------
#> Uncertainty:
#> Method: none
#> -----------------------------------
summary(last_model)
#> Bridge model summary
#> -----------------------------------
#> Target series: quarter_target
#> Target frequency: quarter (step 1)
#> Forecast horizon: 1
#> Target model: lm
#> Estimation rows: 12
#> Regressors: monthly_indicator
#> -----------------------------------
#> Target equation coefficients:
#> # A tibble: 2 × 3
#>   term              estimate bootstrap_se
#>   <chr>                <dbl>        <dbl>
#> 1 (Intercept)         -0.804           NA
#> 2 monthly_indicator    0.998           NA
#> -----------------------------------
#> Indicator summary:
#> # A tibble: 1 × 5
#>   indicator         frequency      predict aggregation indicator_model
#>   <chr>             <chr>          <chr>   <chr>       <chr>          
#> 1 monthly_indicator month (step 1) last    last        deterministic  
#> -----------------------------------
#> Uncertainty:
#> Method: none
#> -----------------------------------
```

These are classic bridge-model choices. They are easy to interpret and
often work well when you want a transparent rule for within-period
aggregation.

## Unrestricted Mixed-Frequency Regression

``` r
unrestricted_model <- bridge(
  target = quarter_target,
  indic = monthly_indicator,
  indic_predict = "direct",
  indic_aggregators = "unrestricted",
  h = 1
)
#> Warning: The unrestricted bridge specification leaves only 3.67 estimation
#> observations per predictor in the final regression. This is below the common
#> 10-observations-per-predictor guideline and may indicate an over-parameterized
#> U-MIDAS specification.

summary(unrestricted_model)
#> Bridge model summary
#> -----------------------------------
#> Target series: quarter_target
#> Target frequency: quarter (step 1)
#> Forecast horizon: 1
#> Target model: lm
#> Estimation rows: 11
#> Regressors: monthly_indicator_hf1, monthly_indicator_hf2, monthly_indicator_hf3
#> Indicator handling: direct alignment
#> -----------------------------------
#> Target equation coefficients:
#> # A tibble: 4 × 3
#>   term                  estimate bootstrap_se
#>   <chr>                    <dbl>        <dbl>
#> 1 (Intercept)               1.53           NA
#> 2 monthly_indicator_hf1     1.00           NA
#> 3 monthly_indicator_hf2    NA              NA
#> 4 monthly_indicator_hf3    NA              NA
#> -----------------------------------
#> Indicator summary:
#> # A tibble: 1 × 5
#>   indicator         frequency      predict aggregation  indicator_model
#>   <chr>             <chr>          <chr>   <chr>        <chr>          
#> 1 monthly_indicator month (step 1) direct  unrestricted deterministic  
#> -----------------------------------
#> Uncertainty:
#> Method: none
#> -----------------------------------
```

`"unrestricted"` estimates one coefficient for each within-quarter
monthly observation. In a monthly-on-quarterly example this means three
separate regressors. This corresponds to a U-MIDAS style specification
when the frequency gap is small enough to estimate those coefficients
reliably.

## Parametric MIDAS-Style Weighting

`bridgr` also supports parametric weighting rules that estimate the
within-period shape from the data while keeping the number of free
parameters small.

``` r
expalmon_model <- bridge(
  target = quarter_target,
  indic = monthly_indicator,
  indic_predict = "last",
  indic_aggregators = "expalmon",
  solver_options = list(seed = 123, n_starts = 1, maxiter = 100),
  h = 1
)

beta_model <- bridge(
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

legendre_model <- bridge(
  target = quarter_target,
  indic = monthly_indicator,
  indic_predict = "last",
  indic_aggregators = "legendre",
  solver_options = list(seed = 123, n_starts = 1, maxiter = 100),
  h = 1
)
#> Warning in bridge(target = quarter_target, indic = monthly_indicator,
#> indic_predict = "last", : Joint parametric aggregation optimization did not
#> fully converge (code 52). Using the best available parameter vector.
```

The fitted object stores both the estimated weight profile and the
underlying parametric coefficients.

``` r
dplyr::bind_rows(
  dplyr::tibble(
    model = "expalmon",
    month = seq_along(expalmon_model$parametric_weights$indic),
    weight = expalmon_model$parametric_weights$indic
  ),
  dplyr::tibble(
    model = "beta",
    month = seq_along(beta_model$parametric_weights$indic),
    weight = beta_model$parametric_weights$indic
  ),
  dplyr::tibble(
    model = "legendre",
    month = seq_along(legendre_model$parametric_weights$indic),
    weight = legendre_model$parametric_weights$indic
  )
)
#> # A tibble: 0 × 2
#> # ℹ 2 variables: model <chr>, month <int>
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
  ),
  dplyr::tibble(
    model = "legendre",
    forecast = as.numeric(forecast(legendre_model)$mean)
  )
)
#> # A tibble: 6 × 2
#>   model        forecast
#>   <chr>           <dbl>
#> 1 mean             30.8
#> 2 last             29.7
#> 3 unrestricted     30.9
#> 4 expalmon         30.8
#> 5 beta             31.6
#> 6 legendre         30.8
```

## Choosing an Aggregation Strategy

As a rough guide:

- Use deterministic bridge aggregation when you want a transparent and
  stable nowcasting rule.
- Use `"unrestricted"` when the frequency gap is small and you want each
  within-period observation to have its own coefficient.
- Use `"expalmon"`, `"beta"`, or `"legendre"` when you want data-driven
  within-period weights but would like a more parsimonious
  parameterization than `"unrestricted"`.
- Use `indic_predict = "direct"` when you want direct MIDAS-style
  alignment based only on the latest observed high-frequency blocks.

The key design choice in `bridgr` is that all of these specifications
share the same estimation, forecasting, and summary workflow. You can
therefore move between bridge and MIDAS-style models without switching
to a different API.
