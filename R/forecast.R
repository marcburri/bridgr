#' Forecast method for bridge_model
#' Implement the `forecast` method for `bridge`
#' @param bridge_model A `bridge` object.
#' @param xreg A matrix of external regressors. If not supplied by the user,
#' the forecast set will be used.
#'
#' @export
forecast <- S7::new_generic(
  "forecast",
  dispatch_args ="bridge_model",
  function(bridge_model, xreg = NULL, ...) {S7::S7_dispatch()}
)


S7::method(forecast, bridge_model) <- function(bridge_model, xreg = NULL, ...) {

  # Forecast the target using the fitted model
  fcst <- forecast::forecast(bridge_model@model, xreg = bridge_model@forecast_set)

  return(fcst)
}
