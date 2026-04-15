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
  xlab = NULL,
  ylab = NULL,
  main = NULL,
  col_history = "black",
  col_fit = "#1b9e77",
  col_forecast = "#d95f02",
  col_interval = grDevices::adjustcolor("#d95f02", alpha.f = 0.2),
  lwd = 2,
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

- xlab, ylab, main:

  Optional axis and title labels. When omitted, sensible defaults are
  chosen from `type`.

- col_history, col_fit, col_forecast, col_interval:

  Colors used for the historical target, fitted values, forecast mean,
  and forecast interval.

- lwd:

  Line width for the main series.

- ...:

  Additional graphical parameters passed to
  [`graphics::plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

`x`, invisibly.
