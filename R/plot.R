#' Plot a Bridge Model
#'
#' Visualize a fitted [bridge()] model either as an in-sample fit or as a
#' forecast with prediction intervals.
#'
#' @param x A `"bridge"` object returned by [bridge()].
#' @param type Plot type. Use `"forecast"` to plot the observed target history
#'   together with the bridge forecast, or `"fit"` to compare the observed
#'   target to the in-sample fitted values.
#' @param level Forecast interval level passed to [forecast.bridge()] when
#'   `type = "forecast"`.
#' @param xlab,ylab,main Optional axis and title labels. When omitted, sensible
#'   defaults are chosen from `type`.
#' @param col_history,col_fit,col_forecast,col_interval Colors used for the
#'   historical target, fitted values, forecast mean, and forecast interval.
#' @param lwd Line width for the main series.
#' @param ... Additional graphical parameters passed to [graphics::plot()].
#'
#' @return `x`, invisibly.
#' @method plot bridge
#' @export
plot.bridge <- function(
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
) {
  type <- match.arg(type)
  target_time <- x$target$time
  target_values <- x$target$values

  if (type == "fit") {
    fitted_time <- x$estimation_set$time
    fitted_values <- as.numeric(stats::fitted(x$model))

    if (is.null(main)) {
      main <- "Bridge Model Fit"
    }
    if (is.null(xlab)) {
      xlab <- "Time"
    }
    if (is.null(ylab)) {
      ylab <- x$target_name
    }

    y_range <- range(c(target_values, fitted_values), na.rm = TRUE)
    graphics::plot(
      target_time,
      target_values,
      type = "l",
      col = col_history,
      lwd = lwd,
      xlab = xlab,
      ylab = ylab,
      main = main,
      ylim = y_range,
      ...
    )
    graphics::lines(
      fitted_time,
      fitted_values,
      col = col_fit,
      lwd = lwd
    )
    graphics::legend(
      "topleft",
      legend = c("Observed", "Fitted"),
      col = c(col_history, col_fit),
      lty = 1,
      lwd = lwd,
      bty = "n"
    )

    return(invisible(x))
  }

  forecast_object <- forecast::forecast(x, level = level)
  forecast_time <- x$future_target_times
  forecast_mean <- as.numeric(forecast_object$mean)
  show_interval <- !all(is.na(forecast_object$lower[, 1]))

  if (show_interval) {
    lower <- as.numeric(forecast_object$lower[, 1])
    upper <- as.numeric(forecast_object$upper[, 1])
  } else {
    lower <- upper <- forecast_mean
  }

  if (is.null(main)) {
    main <- "Bridge Forecast"
  }
  if (is.null(xlab)) {
    xlab <- "Time"
  }
  if (is.null(ylab)) {
    ylab <- x$target_name
  }

  y_range <- range(c(target_values, lower, upper), na.rm = TRUE)
  graphics::plot(
    target_time,
    target_values,
    type = "l",
    col = col_history,
    lwd = lwd,
    xlab = xlab,
    ylab = ylab,
    main = main,
    ylim = y_range,
    ...
  )

  if (show_interval) {
    polygon_x <- c(forecast_time, rev(forecast_time))
    polygon_y <- c(lower, rev(upper))
    graphics::polygon(
      x = polygon_x,
      y = polygon_y,
      border = NA,
      col = col_interval
    )
  }
  graphics::lines(
    c(utils::tail(target_time, 1), forecast_time),
    c(utils::tail(target_values, 1), forecast_mean),
    col = col_forecast,
    lwd = lwd
  )
  graphics::points(
    forecast_time,
    forecast_mean,
    col = col_forecast,
    pch = 16
  )
  legend_entries <- c("Observed", "Forecast")
  legend_colors <- c(col_history, col_forecast)
  legend_lty <- c(1, 1)
  legend_lwd <- c(lwd, lwd)
  legend_pch <- c(NA, 16)
  legend_pt_cex <- c(1, 1)

  if (show_interval) {
    legend_entries <- c(legend_entries, paste0(level, "% interval"))
    legend_colors <- c(legend_colors, col_interval)
    legend_lty <- c(legend_lty, NA)
    legend_lwd <- c(legend_lwd, NA)
    legend_pch <- c(legend_pch, 15)
    legend_pt_cex <- c(legend_pt_cex, 2)
  }

  graphics::legend(
    "topleft",
    legend = legend_entries,
    col = legend_colors,
    lty = legend_lty,
    lwd = legend_lwd,
    pch = legend_pch,
    pt.cex = legend_pt_cex,
    bty = "n"
  )

  invisible(x)
}
