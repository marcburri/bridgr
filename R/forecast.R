#' Forecast a Bridge Model
#'
#' Forecast the target variable from a fitted [bridge()] model.
#'
#' @param object A `"bridge"` object returned by [bridge()].
#' @param xreg Optional future regressors in a [tsbox::ts_boxable()] format.
#' When omitted, the forecast set stored inside `object` is used. When
#' supplied, `xreg` must contain the same regressor names used when fitting the
#' bridge equation.
#' @param ... Passed to [forecast::forecast()].
#'
#' @return An object of class `"forecast"` with an additional
#' `forecast_set` component containing the target-period regressors used for the
#' forecast.
#' @method forecast bridge
#' @export
forecast.bridge <- function(object, xreg = NULL, ...) {
  forecast_set <- object$forecast_set

  if (inherits(object$model, "lm")) {
    if (is.null(xreg)) {
      newdata <- object$forecast_set
    } else {
      newdata <- as_forecast_xreg(
        xreg = xreg,
        regressor_names = object$regressor_names,
        call = rlang::caller_env()
      )
      newdata[[object$target_name]] <- NA_real_
      forecast_set <- newdata[, c("time", object$regressor_names), drop = FALSE]
    }
  } else {
    if (is.null(xreg)) {
      xreg_values <- if (length(object$regressor_names) == 0) {
        NULL
      } else {
        as.matrix(object$forecast_set[, object$regressor_names, drop = FALSE])
      }
    } else {
      forecast_set <- as_forecast_xreg(
        xreg = xreg,
        regressor_names = object$regressor_names,
        call = rlang::caller_env()
      )
      xreg_values <- as.matrix(
        forecast_set[, object$regressor_names, drop = FALSE]
      )
    }
  }

  forecast_object <- if (inherits(object$model, "lm")) {
    forecast::forecast(object$model, newdata = newdata, ...)
  } else {
    forecast::forecast(object$model, xreg = xreg_values, ...)
  }

  forecast_set[[object$target_name]] <- as.numeric(forecast_object$mean)
  forecast_object$forecast_set <- forecast_set
  forecast_object
}
