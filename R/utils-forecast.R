
#' @keywords internal
#' @noRd
fit_target_model <- function(
  estimation_set,
  formula
) {
  stats::lm(formula = formula, data = estimation_set)
}


#' @keywords internal
#' @noRd
recursive_lm_forecast <- function(
  model,
  forecast_set,
  target_name,
  target_lags = 0,
  target_history = NULL,
  innovations = NULL
) {
  horizon <- nrow(forecast_set)
  if (horizon == 0) {
    return(list(
      mean = numeric(),
      values = numeric(),
      forecast_set = forecast_set
    ))
  }

  lag_names <- target_lag_regressor_names(target_name, target_lags)
  direct_predict <- target_lags == 0 || all(lag_names %in% names(forecast_set))
  augmented <- forecast_set
  mean_values <- numeric(horizon)
  response_values <- numeric(horizon)

  if (direct_predict) {
    mean_values <- suppressWarnings(
      as.numeric(stats::predict(model, newdata = forecast_set))
    )
    response_values <- mean_values
    if (!is.null(innovations)) {
      response_values <- response_values + as.numeric(innovations)
    }
    return(list(
      mean = mean_values,
      values = response_values,
      forecast_set = augmented
    ))
  }

  if (is.null(target_history) || length(target_history) < target_lags) {
    rlang::abort(
      paste0(
        "Need at least ",
        target_lags,
        " lagged target observation",
        if (target_lags == 1) "" else "s",
        " to forecast recursively."
      ),
      call = rlang::caller_env()
    )
  }

  history <- as.numeric(target_history)
  for (step_index in seq_len(horizon)) {
    row_data <- augmented[step_index, , drop = FALSE]
    for (lag_index in seq_len(target_lags)) {
      lag_name <- lag_names[[lag_index]]
      row_data[[lag_name]] <- history[[length(history) - lag_index + 1]]
      augmented[[lag_name]][[step_index]] <- row_data[[lag_name]][[1]]
    }

    mean_values[[step_index]] <- suppressWarnings(
      as.numeric(stats::predict(model, newdata = row_data))
    )
    response_values[[step_index]] <- mean_values[[step_index]]
    if (!is.null(innovations)) {
      response_values[[step_index]] <- response_values[[step_index]] +
        innovations[[step_index]]
    }
    history <- c(history, response_values[[step_index]])
  }

  list(
    mean = mean_values,
    values = response_values,
    forecast_set = augmented
  )
}


#' @keywords internal
#' @noRd
forecast_target_model_mean <- function(
  model,
  forecast_set,
  target_name,
  regressor_names,
  target_lags = 0,
  target_history = NULL
) {
  recursive_lm_forecast(
    model = model,
    forecast_set = forecast_set,
    target_name = target_name,
    target_lags = target_lags,
    target_history = target_history
  )$mean
}


#' @keywords internal
#' @noRd
center_model_residuals <- function(model) {
  residuals <- stats::residuals(model)
  residuals - mean(residuals, na.rm = TRUE)
}


#' @keywords internal
#' @noRd
simulate_target_model_draws <- function(
  model,
  forecast_set,
  target_name,
  regressor_names,
  target_lags = 0,
  target_history = NULL,
  n_paths = 100L,
  innovations = NULL
) {
  horizon <- nrow(forecast_set)
  if (horizon == 0) {
    return(matrix(numeric(), nrow = n_paths, ncol = 0))
  }

  centered_residuals <- center_model_residuals(model)
  if (length(centered_residuals) == 0) {
    centered_residuals <- 0
  }

  if (is.null(innovations)) {
    innovations <- matrix(
      sample(centered_residuals, size = n_paths * horizon, replace = TRUE),
      nrow = n_paths,
      ncol = horizon
    )
  } else {
    innovations <- as.matrix(innovations)
    n_paths <- nrow(innovations)
    if (ncol(innovations) != horizon) {
      rlang::abort(
        paste0(
          "Innovation draws must have ",
          horizon,
          " column",
          if (horizon == 1) "" else "s",
          "."
        ),
        call = rlang::caller_env()
      )
    }
  }

  draws <- matrix(NA_real_, nrow = n_paths, ncol = horizon)
  for (path_index in seq_len(n_paths)) {
    path <- recursive_lm_forecast(
      model = model,
      forecast_set = forecast_set,
      target_name = target_name,
      target_lags = target_lags,
      target_history = target_history,
      innovations = innovations[path_index, ]
    )
    draws[path_index, ] <- path$values
  }

  draws
}
