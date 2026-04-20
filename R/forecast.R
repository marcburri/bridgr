#' Forecast a Mixed-Frequency Model
#'
#' Forecast the target variable from a fitted [mf_model()] object.
#'
#' @param object A `"mf_model"` object returned by [mf_model()].
#' @param xreg Optional future regressors in a [tsbox::ts_boxable()] format.
#' When omitted, the forecast regressor set stored inside `object` is used.
#' When supplied, `xreg` must contain the same non-target regressors used when
#' fitting the bridge equation.
#' @param level Prediction interval levels used when the model was estimated
#' with `se = TRUE`. When uncertainty is unavailable, `forecast()` still
#' returns the `se`, `lower`, and `upper` components, filled with `NA`.
#' @param ... Reserved for future extensions.
#'
#' @return An object of class `"mf_model_forecast"` and `"forecast"` containing
#' point forecasts, predictive uncertainty summaries, the
#' target-period regressors used for forecasting, and optional full-system
#' bootstrap metadata.
#'
#' @details In recursive bridge forecasts, uncertainty typically increases with
#' horizon because later forecast steps depend on forecasted rather than
#' observed target lags and, when needed, completed indicator paths. Under the
#' package's residual-resampling and full-system bootstrap workflows, those
#' simulated disturbances accumulate across steps, so standard errors and
#' interval widths can widen as the forecast horizon extends.
#'
#' @srrstats {TS4.2} The forecast method documents the class and contents of the returned `"mf_model_forecast"` object.
#' @srrstats {TS4.6} The forecast method returns point predictions together with explicit uncertainty summaries when available.
#' @srrstats {TS4.6b} Forecast results return point predictions together with standard errors and interval matrices when uncertainty is available.
#' @srrstats {TS4.6c} When full distribution objects are not returned, the forecast output still carries explicit uncertainty summaries.
#' @srrstats {TS4.7} Forecast output keeps forecast values clearly separated from the observed input history.
#' @srrstats {TS4.7a} `forecast.mf_model()` returns forecast values separately from the observed input series.
#' @srrstats {TS3.2} The forecast documentation explains that horizon-specific uncertainty is driven by recursive dependence on forecasted target lags, completed indicator paths, and accumulated simulated disturbances.
#' @srrstats {RE4.14} Forecast outputs include uncertainty measures (`se`, `lower`, and `upper`) in addition to point predictions when available.
#'
#' @examples
#' gdp_growth <- tsbox::ts_pc(gdp)
#' gdp_growth <- tsbox::ts_na_omit(gdp_growth)
#' model <- mf_model(
#'   target = gdp_growth,
#'   indic = baro,
#'   indic_predict = "auto.arima",
#'   indic_aggregators = "mean",
#'   h = 1
#' )
#'
#' forecast(model)
#' @method forecast mf_model
#' @export
forecast.mf_model <- function(
  object,
  xreg = NULL,
  level = c(80, 95),
  ...
) {
  forecast_base_set <- if (is.null(xreg)) {
    object$forecast_base_set
  } else {
    as_forecast_xreg(
      xreg = xreg,
      regressor_names = object$xreg_names,
      call = rlang::caller_env()
    )
  }
  target_history <- if (object$target_lags > 0) {
    utils::tail(object$estimation_set[[object$target_name]], object$target_lags)
  } else {
    NULL
  }

  point_path <- recursive_lm_forecast(
    model = object$model,
    forecast_set = forecast_base_set,
    target_name = object$target_name,
    target_lags = object$target_lags,
    target_history = target_history
  )
  point_forecast <- point_path$mean

  prediction_draws <- NULL
  prediction_method <- object$uncertainty$prediction_method
  if (is.null(xreg)) {
    prediction_draws <- object$uncertainty$prediction_draws
  } else if (identical(prediction_method, "block_bootstrap") &&
    isTRUE(object$bootstrap$enabled)) {
    prediction_draws <- bootstrap_forecast_draws(
      models = object$bootstrap$models,
      forecast_set = forecast_base_set,
      target_name = object$target_name,
      regressor_names = object$regressor_names,
      target_lags = object$target_lags,
      target_histories = object$bootstrap$target_histories
    )
  } else if (identical(prediction_method, "residual_resampling")) {
    prediction_draws <- simulate_target_model_draws(
      model = object$model,
      forecast_set = forecast_base_set,
      target_name = object$target_name,
      regressor_names = object$regressor_names,
      target_lags = object$target_lags,
      target_history = target_history,
      n_paths = object$bootstrap$N
    )
  } else {
    prediction_draws <- NULL
  }

  intervals <- bootstrap_interval_matrices(
    draws = prediction_draws,
    level = level,
    horizon = length(point_forecast)
  )

  forecast_set <- point_path$forecast_set
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
      uncertainty = list(
        enabled = isTRUE(object$uncertainty$enabled),
        coefficient_method = object$uncertainty$coefficient_method,
        prediction_method = prediction_method,
        simulation_paths = if (is.null(prediction_draws)) {
          0L
        } else {
          nrow(prediction_draws)
        }
      ),
      bootstrap = list(
        requested = isTRUE(object$bootstrap$requested),
        enabled = isTRUE(object$bootstrap$enabled),
        N = object$bootstrap$N,
        valid_N = object$bootstrap$valid_N,
        block_length = object$bootstrap$block_length
      ),
      direct = identical(unique(object$indic_predict), "direct"),
      model_class = class(object$model)[[1]]
    ),
    class = c("mf_model_forecast", "forecast")
  )
}

#' Print a Mixed-Frequency Forecast
#'
#' @param x A `"mf_model_forecast"` object returned by [forecast.mf_model()].
#' @param ... Unused.
#'
#' @return `x`, invisibly.
#' @method print mf_model_forecast
#' @rdname forecast.mf_model
#' @export
print.mf_model_forecast <- function(x, ...) {
  format_number <- function(x) {
    formatC(x, format = "f", digits = 3)
  }

  cat("Mixed-frequency forecast\n")
  cat("-----------------------------------\n")
  cat("Target series: ", x$target_name, "\n", sep = "")
  cat("Forecast horizon: ", length(x$mean), "\n", sep = "")

  if (identical(x$uncertainty$prediction_method, "block_bootstrap")) {
    cat("Uncertainty: prediction intervals from full-system block bootstrap\n")
    cat(
      "Bootstrap draws: ",
      x$bootstrap$valid_N,
      " / ",
      x$bootstrap$N,
      "\n",
      sep = ""
    )
    cat("Block length: ", x$bootstrap$block_length, "\n", sep = "")
  } else if (
    identical(x$uncertainty$prediction_method, "residual_resampling")
  ) {
    cat("Uncertainty: prediction intervals from residual resampling\n")
    cat("Simulation paths: ", x$uncertainty$simulation_paths, "\n", sep = "")
  } else if (isTRUE(x$bootstrap$requested)) {
    cat("Uncertainty: prediction intervals unavailable\n")
  } else {
    cat("Uncertainty: point forecast only\n")
  }
  cat("-----------------------------------\n")

  output <- data.frame(
    time = x$time,
    mean = as.numeric(x$mean),
    check.names = FALSE
  )

  if (!is.null(x$uncertainty$prediction_method)) {
    output$se <- as.numeric(x$se)

    for (level_index in seq_along(x$level)) {
      output[[paste0("lower_", x$level[[level_index]])]] <-
        x$lower[, level_index]
      output[[paste0("upper_", x$level[[level_index]])]] <-
        x$upper[, level_index]
    }
  }

  numeric_columns <- vapply(output, is.numeric, logical(1))
  output[numeric_columns] <- lapply(output[numeric_columns], format_number)

  print(
    noquote(format(output, justify = "left")),
    quote = FALSE,
    right = FALSE
  )
  invisible(x)
}
