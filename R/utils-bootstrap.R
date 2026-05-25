
#' @keywords internal
#' @noRd
bridgr_with_seed <- function(seed, expr) {
  expr <- substitute(expr)

  if (is.null(seed)) {
    return(eval(expr, envir = parent.frame()))
  }

  old_seed <- if (
    exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  ) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit(
    {
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      } else {
        # nolint start: object_name_linter.
        assign(x = ".Random.seed", value = old_seed, envir = .GlobalEnv)
        # nolint end
      }
    },
    add = TRUE
  )

  set.seed(seed)
  eval(expr, envir = parent.frame())
}


#' @keywords internal
#' @noRd
default_bootstrap_block_length <- function(n_rows) {
  max(1L, min(as.integer(n_rows), as.integer(ceiling(n_rows^(1 / 3)))))
}


#' @keywords internal
#' @noRd
resolve_bootstrap_block_length <- function(n_rows, block_length) {
  if (is.null(block_length)) {
    return(default_bootstrap_block_length(n_rows))
  }

  max(1L, min(as.integer(n_rows), as.integer(block_length)))
}


#' @keywords internal
#' @noRd
circular_block_bootstrap_indices <- function(n_rows, block_length) {
  n_blocks <- ceiling(n_rows / block_length)
  starts <- sample.int(n_rows, size = n_blocks, replace = TRUE)

  indices <- unlist(
    lapply(
      starts,
      function(start) {
        ((start - 1L + seq_len(block_length) - 1L) %% n_rows) + 1L
      }
    ),
    use.names = FALSE
  )

  indices[seq_len(n_rows)]
}


#' @keywords internal
#' @noRd
extract_model_coefficients <- function(model, coefficient_names) {
  coefficients <- rep(NA_real_, length(coefficient_names))
  names(coefficients) <- coefficient_names

  model_coefficients <- stats::coef(model)
  matched <- match(names(model_coefficients), coefficient_names)
  coefficients[matched[!is.na(matched)]] <- as.numeric(model_coefficients)

  coefficients
}


#' @keywords internal
#' @noRd
predictive_target_model_draw <- function(
  model,
  forecast_set,
  target_name,
  regressor_names,
  target_lags = 0,
  target_history = NULL
) {
  as.numeric(simulate_target_model_draws(
    model,
    forecast_set = forecast_set,
    target_name = target_name,
    regressor_names = regressor_names,
    target_lags = target_lags,
    target_history = target_history,
    n_paths = 1L
  )[1, ])
}


#' @keywords internal
#' @noRd
bootstrap_forecast_draws <- function(
  models,
  forecast_set,
  target_name,
  regressor_names,
  target_lags = 0,
  target_histories = NULL
) {
  if (length(models) == 0) {
    return(NULL)
  }

  if (is.null(target_histories)) {
    target_histories <- rep(list(NULL), length(models))
  }

  draws <- vapply(
    seq_along(models),
    function(index) {
      predictive_target_model_draw(
        model = models[[index]],
        forecast_set = forecast_set,
        target_name = target_name,
        regressor_names = regressor_names,
        target_lags = target_lags,
        target_history = target_histories[[index]]
      )
    },
    FUN.VALUE = numeric(nrow(forecast_set))
  )

  t(draws)
}


#' @keywords internal
#' @noRd
bootstrap_interval_matrices <- function(draws, level, horizon) {
  level_names <- paste0(level, "%")
  lower <- matrix(
    NA_real_,
    nrow = horizon,
    ncol = length(level),
    dimnames = list(NULL, level_names)
  )
  upper <- matrix(
    NA_real_,
    nrow = horizon,
    ncol = length(level),
    dimnames = list(NULL, level_names)
  )
  se <- rep(NA_real_, horizon)

  if (is.null(draws) || nrow(draws) < 2) {
    return(list(se = se, lower = lower, upper = upper))
  }

  se <- apply(draws, 2, stats::sd, na.rm = TRUE)
  alpha <- (100 - level) / 200

  for (level_index in seq_along(level)) {
    lower[, level_index] <- apply(
      draws,
      2,
      stats::quantile,
      probs = alpha[[level_index]],
      na.rm = TRUE,
      type = 8
    )
    upper[, level_index] <- apply(
      draws,
      2,
      stats::quantile,
      probs = 1 - alpha[[level_index]],
      na.rm = TRUE,
      type = 8
    )
  }

  list(se = as.numeric(se), lower = lower, upper = upper)
}


#' @keywords internal
#' @noRd
bootstrap_bridge_system <- function(
  enabled,
  target_tbl,
  indic_tbl,
  target_name,
  target_meta,
  indic_meta,
  target_anchor,
  h,
  frequency_conversions,
  config,
  coefficient_names,
  point_forecast,
  call = rlang::caller_env()
) {
  if (!enabled) {
    return(list(
      enabled = FALSE,
      N = config$bootstrap$N,
      valid_N = 0L,
      block_length = config$bootstrap$block_length,
      coefficient_draws = NULL,
      coefficient_covariance = NULL,
      coefficient_se = NULL,
      prediction_draws = NULL,
      models = NULL,
      target_histories = NULL
    ))
  }

  block_length <- resolve_bootstrap_block_length(
    n_rows = nrow(target_tbl),
    block_length = config$bootstrap$block_length
  )

  prediction_draws <- matrix(
    NA_real_,
    nrow = config$bootstrap$N,
    ncol = length(point_forecast)
  )
  coefficient_draws <- matrix(
    NA_real_,
    nrow = config$bootstrap$N,
    ncol = length(coefficient_names),
    dimnames = list(NULL, coefficient_names)
  )
  models <- vector("list", config$bootstrap$N)
  target_histories <- vector("list", config$bootstrap$N)
  valid <- rep(FALSE, config$bootstrap$N)

  bootstrap_config <- config
  bootstrap_config$se <- FALSE

  for (draw_index in seq_len(config$bootstrap$N)) {
    resampled_inputs <- resample_bridge_inputs(
      target_tbl = target_tbl,
      indic_tbl = indic_tbl,
      target_meta = target_meta,
      indic_meta = indic_meta,
      target_anchor = target_anchor,
      h = h,
      frequency_conversions = frequency_conversions,
      block_length = block_length
    )

    draw_fit <- suppressWarnings(try(
      fit_bridge_model(
        target_tbl = resampled_inputs$target,
        indic_tbl = resampled_inputs$indic,
        target_name = target_name,
        config = bootstrap_config
      ),
      silent = TRUE
    ))
    if (inherits(draw_fit, "try-error")) {
      next
    }

    draw_forecast <- suppressWarnings(try(
      simulate_target_model_draws(
        model = draw_fit$model,
        forecast_set = draw_fit$forecast_base_set,
        target_name = draw_fit$target_name,
        regressor_names = draw_fit$regressor_names,
        target_lags = draw_fit$target_lags,
        target_history = utils::tail(
          draw_fit$estimation_set[[draw_fit$target_name]],
          max(1L, draw_fit$target_lags)
        ),
        n_paths = 1L
      ),
      silent = TRUE
    ))
    if (
      inherits(draw_forecast, "try-error") ||
        any(!is.finite(draw_forecast))
    ) {
      next
    }

    prediction_draws[draw_index, ] <- as.numeric(draw_forecast[1, ])
    coefficient_draws[draw_index, ] <- extract_model_coefficients(
      model = draw_fit$model,
      coefficient_names = coefficient_names
    )
    models[[draw_index]] <- draw_fit$model
    target_histories[[draw_index]] <- if (draw_fit$target_lags > 0) {
      utils::tail(
        draw_fit$estimation_set[[draw_fit$target_name]],
        draw_fit$target_lags
      )
    } else {
      NULL
    }
    valid[[draw_index]] <- TRUE
  }

  if (!any(valid)) {
    rlang::warn(
      paste(
        "Full-system block bootstrap failed for every resample.",
        "Prediction intervals will be unavailable."
      ),
      call = call
    )
    return(list(
      enabled = FALSE,
      N = config$bootstrap$N,
      valid_N = 0L,
      block_length = block_length,
      coefficient_draws = NULL,
      coefficient_covariance = NULL,
      coefficient_se = NULL,
      prediction_draws = NULL,
      models = NULL,
      target_histories = NULL
    ))
  }

  if (sum(valid) < config$bootstrap$N) {
    rlang::warn(
      paste0(
        "Full-system block bootstrap produced ",
        sum(valid),
        " valid draws out of ",
        config$bootstrap$N,
        "."
      ),
      call = call
    )
  }

  coefficient_draws <- coefficient_draws[valid, , drop = FALSE]
  coefficient_covariance <- if (nrow(coefficient_draws) < 2) {
    matrix(
      NA_real_,
      nrow = length(coefficient_names),
      ncol = length(coefficient_names),
      dimnames = list(coefficient_names, coefficient_names)
    )
  } else {
    stats::cov(coefficient_draws, use = "pairwise.complete.obs")
  }
  coefficient_se <- if (nrow(coefficient_draws) < 2) {
    rep(NA_real_, length(coefficient_names))
  } else {
    apply(coefficient_draws, 2, stats::sd, na.rm = TRUE)
  }
  names(coefficient_se) <- coefficient_names

  list(
    enabled = TRUE,
    N = config$bootstrap$N,
    valid_N = sum(valid),
    block_length = block_length,
    coefficient_draws = coefficient_draws,
    coefficient_covariance = coefficient_covariance,
    coefficient_se = coefficient_se,
    prediction_draws = prediction_draws[valid, , drop = FALSE],
    models = models[valid],
    target_histories = target_histories[valid]
  )
}


#' @keywords internal
#' @noRd
build_prediction_uncertainty <- function(
  enabled,
  model = NULL,
  forecast_set = NULL,
  target_name = NULL,
  regressor_names = NULL,
  target_lags = 0,
  target_history = NULL,
  bootstrap = NULL,
  full_system_bootstrap = FALSE,
  full_bootstrap = NULL
) {
  if (!enabled) {
    return(list(
      enabled = FALSE,
      method = NULL,
      draws = NULL,
      N = 0L
    ))
  }

  if (isTRUE(full_system_bootstrap)) {
    if (
      isTRUE(full_bootstrap$enabled) &&
        !is.null(full_bootstrap$prediction_draws)
    ) {
      return(list(
        enabled = TRUE,
        method = "block_bootstrap",
        draws = full_bootstrap$prediction_draws,
        N = full_bootstrap$valid_N
      ))
    }

    return(list(
      enabled = FALSE,
      method = NULL,
      draws = NULL,
      N = 0L
    ))
  }

  prediction_draws <- suppressWarnings(try(
    simulate_target_model_draws(
      model = model,
      forecast_set = forecast_set,
      target_name = target_name,
      regressor_names = regressor_names,
      target_lags = target_lags,
      target_history = target_history,
      n_paths = bootstrap$N
    ),
    silent = TRUE
  ))

  if (inherits(prediction_draws, "try-error")) {
    rlang::warn(
      paste(
        "Residual-resampling prediction intervals could not be simulated.",
        "Prediction intervals will be unavailable."
      ),
      call = rlang::caller_env()
    )
    return(list(
      enabled = FALSE,
      method = NULL,
      draws = NULL,
      N = 0L
    ))
  }

  if (!is.null(prediction_draws) && nrow(prediction_draws) > 0) {
    return(list(
      enabled = TRUE,
      method = "residual_resampling",
      draws = prediction_draws,
      N = nrow(prediction_draws)
    ))
  }

  list(
    enabled = FALSE,
    method = NULL,
    draws = NULL,
    N = 0L
  )
}
