forecast <- S7::new_generic(
  "forecast",
  dispatch_args ="bridge_model",
  function(bridge_model, h = 1, xreg = NULL, ...) {S7::S7_dispatch()}
)

# Implement the `forecast` method for `Bridge`
S7::method(forecast, bridge_model) <- function(bridge_model, h = 1, xreg = NULL, ...) {
  target <- object@target
  indic <- object@indic
  model <- object@model

  # Prepare future values of the indicator
  newx <- indic %>%
    ts_span(start = end(target)) %>%
    ts_xts()

  # Forecast the target using the fitted model
  fcst <- forecast(model, xreg = newx, h = h)
  forecast_values <- fcst$mean
  forecast_dates <- seq.Date(from = Sys.Date(), by = "month", length.out = h)

  forecast_df <- tibble(
    value = forecast_values,
    time = forecast_dates,
    id = paste0("h", 1:h)
  )

  return(forecast_df)
}

