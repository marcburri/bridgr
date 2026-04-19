# Plot a Mixed-Frequency Model

Visualize a fitted
[`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md)
object either as an in-sample fit or as a forecast with prediction
intervals.

## Usage

``` r
# S3 method for class 'mf_model'
plot(
  x,
  type = c("forecast", "fit"),
  level = 95,
  history_n = 50,
  xlab = NULL,
  ylab = NULL,
  main = NULL,
  ...
)
```

## Arguments

- x:

  A `"mf_model"` object returned by
  [`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md).

- type:

  Plot type. Use `"forecast"` to plot the observed target history
  together with the bridge forecast, or `"fit"` to compare the observed
  target to the in-sample fitted values.

- level:

  Forecast interval level passed to
  [`forecast.mf_model()`](https://marcburri.github.io/bridgr/reference/forecast.mf_model.md)
  when `type = "forecast"`.

- history_n:

  Number of historical target observations to display. Defaults to the
  most recent `50`. Set to `NULL` to show the full history.

- xlab, ylab, main:

  Optional axis and title labels. When omitted, sensible defaults are
  chosen from `type`.

- ...:

  Additional arguments passed to
  [`theme_bridgr()`](https://marcburri.github.io/bridgr/reference/theme_bridgr.md).

## Value

A ggplot2 object.

## Examples

``` r
gdp_growth <- suppressMessages(tsbox::ts_na_omit(tsbox::ts_pc(gdp)))
model <- mf_model(
  target = gdp_growth,
  indic = baro,
  indic_predict = "auto.arima",
  indic_aggregators = "mean",
  h = 1
)

plot(model, type = "fit")
```
