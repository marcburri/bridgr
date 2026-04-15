#' Plot a Bridge Model
#'
#' Visualize a fitted [bridge()] model either as an in-sample fit or as a
#' forecast with prediction intervals.
#'
#' @param x A `"bridge"` object returned by [bridge()].
#' @param type Plot type. Use `"forecast"` to plot the observed target history
#'   together with the bridge forecast, or `"fit"` to compare the observed
#'   target to the in-sample fitted values.
#' @param level Prediction interval level passed to [forecast::forecast()] when
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
  lower <- as.numeric(forecast_object$lower[, 1])
  upper <- as.numeric(forecast_object$upper[, 1])

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

  polygon_x <- c(forecast_time, rev(forecast_time))
  polygon_y <- c(lower, rev(upper))
  graphics::polygon(
    x = polygon_x,
    y = polygon_y,
    border = NA,
    col = col_interval
  )
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
  graphics::legend(
    "topleft",
    legend = c("Observed", "Forecast", paste0(level, "% interval")),
    col = c(col_history, col_forecast, col_interval),
    lty = c(1, 1, NA),
    lwd = c(lwd, lwd, NA),
    pch = c(NA, 16, 15),
    pt.cex = c(1, 1, 2),
    bty = "n"
  )

  invisible(x)
}
