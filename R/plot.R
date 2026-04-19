#' bridgr Plot Theme and Color Scales
#'
#' Plot styling helpers for `bridgr` graphics, based on the visual defaults
#' used in the `reviser` package.
#'
#' @param base_size Base text size for the plot theme.
#' @param legend_position Legend position passed to [ggplot2::theme()].
#' @param legend_direction Legend direction passed to [ggplot2::theme()].
#' @param ... Additional arguments. In `theme_bridgr()`, `legend.position` and
#'   `legend.direction` are accepted for backward compatibility. In the scale
#'   helpers, `...` is forwarded to the ggplot2 scale constructors.
#'
#' @return A ggplot2 theme, color palette, or scale.
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) +
#'   ggplot2::geom_point() +
#'   theme_bridgr()
#'
#' colors_bridgr()[1:3]
#' @name theme_bridgr
#' @export
theme_bridgr <- function(
  base_size = 12,
  legend_position = "bottom",
  legend_direction = "horizontal",
  ...
) {
  legacy_args <- list(...)
  if (length(legacy_args) > 0) {
    legacy_names <- names(legacy_args)
    if (is.null(legacy_names) || any(!nzchar(legacy_names))) {
      rlang::abort("Arguments passed to `...` must be named.")
    }

    invalid_names <- setdiff(
      legacy_names,
      c("legend.position", "legend.direction")
    )
    if (length(invalid_names) > 0) {
      rlang::abort(
        paste0(
          "Unused argument",
          if (length(invalid_names) == 1) "" else "s",
          ": ",
          paste(invalid_names, collapse = ", ")
        )
      )
    }

    if ("legend.position" %in% legacy_names) {
      legend_position <- legacy_args[["legend.position"]]
    }
    if ("legend.direction" %in% legacy_names) {
      legend_direction <- legacy_args[["legend.direction"]]
    }
  }

  half_line <- base_size / 2
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(
        size = ggplot2::rel(0.9),
        color = "grey10",
        margin = ggplot2::margin(t = 0, r = 7, b = 0, l = 0)
      ),
      plot.title = ggplot2::element_text(
        color = "grey10",
        face = "bold",
        margin = ggplot2::margin(t = half_line * 2, b = half_line * 0.7),
        hjust = 0,
        size = ggplot2::rel(1.2)
      ),
      plot.subtitle = ggplot2::element_text(
        color = "grey10",
        margin = ggplot2::margin(t = 0, b = half_line * 1.2),
        size = ggplot2::rel(0.9),
        hjust = 0
      ),
      plot.caption = ggplot2::element_text(
        color = "grey50",
        margin = ggplot2::margin(t = 0, b = half_line * 1.2),
        size = ggplot2::rel(0.8)
      ),
      panel.grid = ggplot2::element_line(linewidth = 0.2),
      axis.text = ggplot2::element_text(
        color = "grey10",
        size = ggplot2::rel(0.7)
      ),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        color = "grey10",
        size = ggplot2::rel(0.9)
      ),
      legend.position = legend_position,
      legend.direction = legend_direction
    )
}

#' @rdname theme_bridgr
#' @export
colors_bridgr <- function() {
  c(
    "#4D4D4D",
    "#0072B2",
    "#D55E00",
    "#009E73",
    "#E69F00",
    "#56B4E9",
    "#CC79A7",
    "#F0E442",
    "#999999",
    "#8D0808",
    "#461E78",
    "#4AFFF0",
    "#34BDCC",
    "#4F61A1",
    "#440A4F",
    "#C3FBC4",
    "#85F9D6",
    "#79C7AD",
    "#A6CC7A",
    "#DFFF7B",
    "#8D7B88",
    "#4E414F",
    "#BAADB5",
    "#2D2538",
    "#837A80",
    "#FFF68F",
    "#800080",
    "#F8B1CC",
    "#C29BFF",
    "#FFD700",
    "#FF6347"
  )
}

#' @rdname theme_bridgr
#' @export
scale_color_bridgr <- function(...) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    palette = scales::manual_pal(colors_bridgr()),
    ...
  )
}

#' @rdname theme_bridgr
#' @export
scale_fill_bridgr <- function(...) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    palette = scales::manual_pal(colors_bridgr()),
    ...
  )
}

#' Plot a Mixed-Frequency Model
#'
#' Visualize a fitted [mf_model()] object either as an in-sample fit or as a
#' forecast with prediction intervals.
#'
#' @param x A `"mf_model"` object returned by [mf_model()].
#' @param type Plot type. Use `"forecast"` to plot the observed target history
#'   together with the bridge forecast, or `"fit"` to compare the observed
#'   target to the in-sample fitted values.
#' @param level Forecast interval level passed to [forecast.mf_model()] when
#'   `type = "forecast"`.
#' @param history_n Number of historical target observations to display.
#'   Defaults to the most recent `50`. Set to `NULL` to show the full history.
#' @param xlab,ylab,main Optional axis and title labels. When omitted, sensible
#'   defaults are chosen from `type`.
#' @param ... Additional arguments passed to [theme_bridgr()].
#'
#' @return A ggplot2 object.
#'
#' @examples
#' gdp_growth <- suppressMessages(tsbox::ts_na_omit(tsbox::ts_pc(gdp)))
#' model <- mf_model(
#'   target = gdp_growth,
#'   indic = baro,
#'   indic_predict = "auto.arima",
#'   indic_aggregators = "mean",
#'   h = 1
#' )
#'
#' plot(model, type = "fit")
#' @method plot mf_model
#' @export
plot.mf_model <- function(
  x,
  type = c("forecast", "fit"),
  level = 95,
  history_n = 50,
  xlab = NULL,
  ylab = NULL,
  main = NULL,
  ...
) {
  type <- match.arg(type)

  target_data <- dplyr::tibble(
    time = x$target$time,
    value = x$target$values
  )
  if (!is.null(history_n)) {
    history_n <- as.integer(history_n)
    target_data <- utils::tail(target_data, max(1L, history_n))
  }

  palette <- colors_bridgr()
  observed_color <- palette[[1]]
  fit_color <- palette[[2]]
  forecast_color <- palette[[3]]

  if (type == "fit") {
    fitted_data <- dplyr::tibble(
      time = x$estimation_set$time,
      value = as.numeric(stats::fitted(x$model))
    )
    fitted_data <- fitted_data |>
      dplyr::filter(.data$time >= min(target_data$time))

    plot_data <- dplyr::bind_rows(
      dplyr::mutate(target_data, series = "Observed"),
      dplyr::mutate(fitted_data, series = "Fitted")
    )

    if (is.null(main)) {
      main <- "Mixed-Frequency Model Fit"
    }
    if (is.null(xlab)) {
      xlab <- "Time"
    }
    if (is.null(ylab)) {
      ylab <- x$target_name
    }

    return(
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data$time, y = .data$value, color = .data$series)
      ) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::scale_color_manual(
          values = c("Observed" = observed_color, "Fitted" = fit_color)
        ) +
        theme_bridgr(...) +
        ggplot2::labs(
          title = main,
          x = xlab,
          y = ylab,
          color = NULL
        )
    )
  }

  forecast_object <- forecast::forecast(x, level = level)
  forecast_data <- dplyr::tibble(
    time = x$future_target_times,
    value = as.numeric(forecast_object$mean)
  )
  show_interval <- !all(is.na(forecast_object$lower[, 1]))

  if (show_interval) {
    forecast_data <- forecast_data |>
      dplyr::mutate(
        lower = as.numeric(forecast_object$lower[, 1]),
        upper = as.numeric(forecast_object$upper[, 1])
      )
  }

  observed_plot_data <- dplyr::mutate(target_data, series = "Observed")
  forecast_line_data <- dplyr::bind_rows(
    dplyr::tibble(
      time = utils::tail(target_data$time, 1),
      value = utils::tail(target_data$value, 1),
      series = "Forecast"
    ),
    dplyr::mutate(forecast_data, series = "Forecast")
  )

  if (is.null(main)) {
    main <- "Mixed-Frequency Forecast"
  }
  if (is.null(xlab)) {
    xlab <- "Time"
  }
  if (is.null(ylab)) {
    ylab <- x$target_name
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = observed_plot_data,
      ggplot2::aes(x = .data$time, y = .data$value, color = .data$series),
      linewidth = 0.8
    )

  if (show_interval) {
    if (nrow(forecast_data) > 1) {
      p <- p +
        ggplot2::geom_ribbon(
          data = forecast_data,
          ggplot2::aes(
            x = .data$time,
            ymin = .data$lower,
            ymax = .data$upper,
            fill = paste0(level, "% interval")
          ),
          alpha = 0.2
        )
    } else {
      p <- p +
        ggplot2::geom_linerange(
          data = forecast_data,
          ggplot2::aes(
            x = .data$time,
            ymin = .data$lower,
            ymax = .data$upper,
            color = paste0(level, "% interval")
          ),
          linewidth = 1
        )
    }
  }

  p <- p +
    ggplot2::geom_line(
      data = forecast_line_data,
      ggplot2::aes(x = .data$time, y = .data$value, color = .data$series),
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = forecast_data,
      ggplot2::aes(x = .data$time, y = .data$value, color = "Forecast"),
      size = 2
    ) +
    ggplot2::scale_color_manual(
      values = if (show_interval && nrow(forecast_data) == 1) {
        c(
          c("Observed" = observed_color, "Forecast" = forecast_color),
          stats::setNames(forecast_color, paste0(level, "% interval"))
        )
      } else {
        c("Observed" = observed_color, "Forecast" = forecast_color)
      },
      breaks = if (show_interval && nrow(forecast_data) == 1) {
        c("Observed", "Forecast", paste0(level, "% interval"))
      } else {
        c("Observed", "Forecast")
      }
    ) +
    theme_bridgr(...) +
    ggplot2::labs(
      title = main,
      x = xlab,
      y = ylab,
      color = NULL,
      fill = NULL
    )

  if (show_interval && nrow(forecast_data) > 1) {
    p <- p +
      ggplot2::scale_fill_manual(
        values = stats::setNames(
          grDevices::adjustcolor(forecast_color, alpha.f = 0.2),
          paste0(level, "% interval")
        ),
        breaks = paste0(level, "% interval")
      )
  }

  p
}
