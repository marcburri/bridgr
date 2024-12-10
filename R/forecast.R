#' Forecasting from a `bridge` object
#'
#' This function is used to forecast the target variable using the fitted `bridge` model.
#' @param object A `bridge` object obtained from [bridgr::bridge()].
#' @param xreg A [tsbox::ts_boxable()] series of external regressors. If not
#' supplied by the user, the forecast set calculated by [bridgr::bridge()]
#' will be used. This is useful for made up scenarios.
#' @param ... Additional arguments to be passed to the forecast function. Ignored at the moment.
#' @return An object of class "\code{forecast}".
#' An object of class \code{"forecast"} is a list usually containing at least
#' the following elements: \item{model}{A list containing information about the
#' fitted model} \item{method}{The name of the forecasting method as a
#' character string} \item{mean}{Point forecasts as a time series}
#' \item{lower}{Lower limits for prediction intervals} \item{upper}{Upper
#' limits for prediction intervals} \item{level}{The confidence values
#' associated with the prediction intervals} \item{x}{The original time series
#' (either \code{object} itself or the time series used to create the model
#' stored as \code{object}).} \item{residuals}{Residuals from the fitted model.
#' For models with additive errors, the residuals will be x minus the fitted
#' values.} \item{fitted}{Fitted values (one-step forecasts)} \item{forecast_set}{The
#' forecast set with the forecasted indicator variables used to calculate the forecast of
#' the target variable.}
#' @examples
#' library(bridgr)
#'
#' # Example usage
#' target_series <- suppressMessages(tsbox::ts_tbl(data.frame(
#'   time = seq(as.Date("2020-01-01"), as.Date("2022-12-01"), by = "quarter"),
#'   value = rnorm(12)
#' )))
#'
#' indic_series <- suppressMessages(tsbox::ts_tbl(data.frame(
#'   time = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "month"),
#'   value = rnorm(37)
#' )))
#'
#' bridge_model <- suppressMessages(bridge(
#'   target = target_series,
#'   indic = indic_series,
#'   indic_predict = "mean",
#'   indic_aggregators = "mean",
#'   indic_lags = 2,
#'   target_lags = 1,
#'   h = 1
#' ))
#'
#' # Forecasting using the bridge model
#' fcst <- forecast(bridge_model)
#' @export
forecast.bridge <- function(object, xreg = NULL, ...) {

  if (is.null(xreg)) {
    xreg <- tsbox::ts_xts(tsbox::ts_long(object$forecast_set)) %>% suppressMessages()
  } else {
    xreg <- tsbox::ts_xts(tsbox::ts_long(xreg)) %>% suppressMessages()
  }

  # Forecast the target using the fitted model
  fcst <- forecast::forecast(object$model, xreg = xreg)
  object$forecast_set$values <- as.numeric(fcst$mean)
  fcst$forecast_set <- object$forecast_set

  return(fcst)
}
