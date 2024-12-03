#' Implement the `forecast` method for `bridge_model` class
#'
#' This method is used to forecast the target variable using the fitted model.
#' @param bridge_model A `bridge_model` object.
#' @param xreg A matrix of external regressors. If not supplied by the user,
#' the forecast set will be used. This is useful for made up scenarios.
#' @noRd
forecast.bridge_model <- S7::new_external_generic("generics", "forecast", "bridge_model")

#' Define the `forecast` method for `bridge_model` class
#'
#' @keywords internal
#' This method is used to forecast the target variable using the fitted model.
#' @param bridge_model A `bridge_model` object.
#' @param xreg A matrix of external regressors. If not supplied by the user,
#' the forecast set will be used. This is useful for made up scenarios.
#' @name forecast
S7::method(forecast.bridge_model, bridge_model) <- function(bridge_model, xreg = NULL, ...) {

  # Forecast the target using the fitted model
  fcst <- forecast::forecast(bridge_model@model, xreg = bridge_model@forecast_set)

  return(fcst)
}
