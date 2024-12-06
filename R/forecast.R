#' Forecasting from a `bridge_model` class
#'
#' This function is used to forecast the target variable using the fitted `bridge_model`.
#' @param object An object of class `bridge_model` object obtained from [bridgr::bridge()].
#' @param xreg A [tsbox::ts_boxable()] series of external regressors. If not
#' supplied by the user, the forecast set calculated by [bridgr::bridge()]
#' will be used. This is useful for made up scenarios.
#' @param ... Additional arguments to be passed to the forecast function.
#' @export
forecast.bridge <- function(object, xreg = NULL, ...) {

  if (is.null(xreg)) {
    xreg <- object$forecast_set
  }
  # Forecast the target using the fitted model
  fcst <- forecast::forecast(object$model, xreg = xreg)

  return(fcst)
}

# forecast.bridge <- S7::new_external_generic("generics", "forecast", "bridge_model")


#' Forecasting from a `bridge_model` class
#'
#' This function is used to forecast the target variable using the fitted `bridge_model`.
#' @param object An object of class `bridge_model` obtained from [bridgr::bridge()].
#' @param xreg A [tsbox::ts_boxable()] series of external regressors. If not
#' supplied by the user, the forecast set calculated by [bridgr::bridge()]
#' will be used. This is useful for made up scenarios.
#' @param ... Additional arguments to be passed to the forecast function.
#' @usage forecast(object, xreg = NULL, ...)
#' @keywords internal
#' @name forecast.bridge
#' @noRd
# S7::method(forecast.bridge, bridge_model) <- function(object, xreg = NULL, ...) {
#
#   if (is.null(xreg)) {
#     xreg <- object@forecast_set
#   }
#   # Forecast the target using the fitted model
#   fcst <- forecast::forecast(object@model, xreg = xreg)
#
#   return(fcst)
# }
