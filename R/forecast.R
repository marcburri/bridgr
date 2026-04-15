#' Forecast a Bridge Model
#'
#' Forecast the target variable from a fitted [bridge()] model.
#'
#' @param object A `"bridge"` object returned by [bridge()].
#' @param xreg Optional future regressors in a [tsbox::ts_boxable()] format.
#' When omitted, the forecast set stored inside `object` is used. When
#' supplied, `xreg` must contain the same regressor names used when fitting the
#' bridge equation.
#' @param level Confidence levels used for bootstrap forecast intervals when the
#' model was estimated with `se = TRUE`. When uncertainty is unavailable,
#' `forecast()` still returns the `se`, `lower`, and `upper` components, filled
#' with `NA`.
#' @param ... Reserved for future extensions.
#'
#' @return An object of class `"bridge_forecast"` and `"forecast"` containing
#' point forecasts, conditional bootstrap uncertainty summaries, the
#' target-period regressors used for forecasting, and bootstrap metadata.
#' @method forecast bridge
#' @export
forecast.bridge <- function(
  object,
  xreg = NULL,
  level = c(80, 95),
  ...
) {
  forecast_set <- if (is.null(xreg)) {
    object$forecast_set
  } else {
    as_forecast_xreg(
      xreg = xreg,
      regressor_names = object$regressor_names,
      call = rlang::caller_env()
    )
  }

  point_forecast <- forecast_target_model_mean(
    model = object$model,
    forecast_set = forecast_set,
    target_name = object$target_name,
    regressor_names = object$regressor_names
  )

  bootstrap_draws <- NULL
  if (isTRUE(object$bootstrap$enabled)) {
    if (is.null(xreg)) {
      bootstrap_draws <- object$bootstrap$forecast_draws
    } else {
      bootstrap_draws <- bootstrap_forecast_draws(
        models = object$bootstrap$models,
        forecast_set = forecast_set,
        target_name = object$target_name,
        regressor_names = object$regressor_names
      )
    }
  }

  intervals <- bootstrap_interval_matrices(
    draws = bootstrap_draws,
    level = level,
    horizon = length(point_forecast)
  )

  forecast_set[[object$target_name]] <- as.numeric(point_forecast)
  structure(
    list(
      mean = as.numeric(point_forecast),
      se = intervals$se,
      lower = intervals$lower,
      upper = intervals$upper,
      level = level,
      time = forecast_set$time,
      target_name = object$target_name,
      forecast_set = forecast_set,
      bootstrap = list(
        enabled = isTRUE(object$bootstrap$enabled),
        type = object$bootstrap$type,
        N = object$bootstrap$N,
        valid_N = object$bootstrap$valid_N,
        block_length = object$bootstrap$block_length,
        conditional = isTRUE(object$bootstrap$conditional)
      ),
      direct = identical(unique(object$indic_predict), "direct"),
      model_class = class(object$model)[[1]]
    ),
    class = c("bridge_forecast", "forecast")
  )
}

#' Print a Bridge Forecast
#'
#' @param x A `"bridge_forecast"` object returned by [forecast.bridge()].
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @method print bridge_forecast
#' @rdname forecast.bridge
#' @export
print.bridge_forecast <- function(x, ...) {
  cat("Bridge forecast\n")
  cat("-----------------------------------\n")
  cat("Target series: ", x$target_name, "\n", sep = "")
  cat("Forecast horizon: ", length(x$mean), "\n", sep = "")
  cat("Target model: ", x$model_class, "\n", sep = "")

  if (isTRUE(x$direct)) {
    cat("Indicator handling: direct alignment\n")
  }

  if (isTRUE(x$bootstrap$enabled)) {
    cat("Uncertainty: conditional ", x$bootstrap$type, " bootstrap\n", sep = "")
    cat(
      "Bootstrap draws: ",
      x$bootstrap$valid_N,
      " / ",
      x$bootstrap$N,
      "\n",
      sep = ""
    )
    cat("Block length: ", x$bootstrap$block_length, "\n", sep = "")
  } else {
    cat("Uncertainty: point forecast only\n")
  }
  cat("-----------------------------------\n")

  output <- dplyr::tibble(
    time = x$time,
    mean = as.numeric(x$mean),
    se = as.numeric(x$se)
  )

  for (level_index in seq_along(x$level)) {
    output[[paste0("lower_", x$level[[level_index]])]] <-
      x$lower[, level_index]
    output[[paste0("upper_", x$level[[level_index]])]] <-
      x$upper[, level_index]
  }

  print(output, row.names = FALSE)
  invisible(x)
}
