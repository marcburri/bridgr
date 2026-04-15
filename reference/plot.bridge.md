# Plot a Bridge Model

Visualize a fitted
[`bridge()`](https://marcburri.github.io/bridgr/reference/bridge.md)
model either as an in-sample fit or as a forecast with prediction
intervals.

## Usage

``` r
# S3 method for class 'bridge'
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

  A `"bridge"` object returned by
  [`bridge()`](https://marcburri.github.io/bridgr/reference/bridge.md).

- type:

  Plot type. Use `"forecast"` to plot the observed target history
  together with the bridge forecast, or `"fit"` to compare the observed
  target to the in-sample fitted values.

- level:

  Forecast interval level passed to
  [`forecast.bridge()`](https://marcburri.github.io/bridgr/reference/forecast.bridge.md)
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
