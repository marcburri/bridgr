
# Minimum estimation-observations-per-predictor threshold below which the
# unrestricted (U-MIDAS) specification triggers a warning.
MIN_OBS_PER_PREDICTOR <- 10L

# Symmetric bounds on the parametric-aggregator parameters during joint
# optimization, on the optimizer scale (log scale for beta shapes).
PARAMETRIC_OPT_BOUNDS <- c(-10, 10)

# Standard deviation of the Gaussian jitter added to additional multi-start
# initial values in the parametric-aggregator optimizer.
PARAMETRIC_MULTISTART_JITTER_SD <- 0.5


#' @keywords internal
#' @noRd
is_parametric_aggregator <- function(aggregator) {
  is.character(aggregator) &&
    length(aggregator) == 1 &&
    aggregator %in% c("expalmon", "beta")
}


#' @keywords internal
#' @noRd
parametric_parameter_names <- function(aggregator) {
  switch(
    aggregator,
    "expalmon" = c("linear", "quadratic"),
    "beta" = c("left_shape", "right_shape"),
    rlang::abort(
      paste0("Unsupported parametric aggregator `", aggregator, "`."),
      call = rlang::caller_env()
    )
  )
}


#' @keywords internal
#' @noRd
parametric_parameter_count <- function(aggregator) {
  length(parametric_parameter_names(aggregator))
}


#' @keywords internal
#' @noRd
default_parametric_start <- function(aggregator) {
  if (identical(aggregator, "beta")) {
    return(c(1, 1))
  }

  rep(0, parametric_parameter_count(aggregator))
}


#' @srrstats {RE2.4} Bridge regressions that collapse each submitted indicator
#' to one finalized regressor column are screened for exact collinearity before
#' fitting through one dedicated preprocessing routine that checks both
#' regressor-vs-regressor and target-vs-regressor dependencies.
#' @keywords internal
#' @noRd
check_estimation_set_collinearity <- function(
  estimation_set,
  target_name,
  regressor_names,
  call = rlang::caller_env()
) {
  check_regressor_collinearity(
    estimation_set = estimation_set,
    regressor_names = regressor_names,
    call = call
  )
  check_target_regressor_collinearity(
    estimation_set = estimation_set,
    target_name = target_name,
    regressor_names = regressor_names,
    call = call
  )
}


#' @keywords internal
#' @noRd
has_perfect_affine_dependence <- function(x, y) {
  collinearity_tol <- sqrt(.Machine$double.eps) * max(1, abs(x), abs(y))
  residuals <- qr.resid(
    qr(
      cbind("(Intercept)" = 1, x = x),
      tol = sqrt(.Machine$double.eps)
    ),
    y
  )

  max(abs(residuals)) <= collinearity_tol
}


#' @srrstats {RE2.4a} Finalized single-column bridge regressors are screened
#' pairwise for exact affine dependencies before fitting, so duplicated or
#' perfectly collinear predictor columns are rejected with an explicit
#' preprocessing error.
#' @keywords internal
#' @noRd
check_regressor_collinearity <- function(
  estimation_set,
  regressor_names,
  call = rlang::caller_env()
) {
  if (length(regressor_names) < 2) {
    return(invisible(NULL))
  }

  regressor_matrix <- as.matrix(estimation_set[, regressor_names, drop = FALSE])

  for (left_index in seq_len(length(regressor_names) - 1L)) {
    for (right_index in seq.int(left_index + 1L, length(regressor_names))) {
      left_name <- regressor_names[[left_index]]
      right_name <- regressor_names[[right_index]]

      if (
        has_perfect_affine_dependence(
          x = regressor_matrix[, left_index],
          y = regressor_matrix[, right_index]
        )
      ) {
        rlang::abort(
          paste0(
            "Perfect collinearity detected among regressors in the final ",
            "estimation set: `",
            left_name,
            "` and `",
            right_name,
            "`."
          ),
          call = call
        )
      }
    }
  }

  invisible(NULL)
}


#' @srrstats {RE2.4b} The target column is screened against each finalized
#' single-column regressor before fitting, so response variables that are
#' perfectly collinear with any submitted predictor are rejected explicitly.
#' @keywords internal
#' @noRd
check_target_regressor_collinearity <- function(
  estimation_set,
  target_name,
  regressor_names,
  call = rlang::caller_env()
) {
  regressor_matrix <- as.matrix(estimation_set[, regressor_names, drop = FALSE])
  target_values <- estimation_set[[target_name]]

  for (regressor_index in seq_along(regressor_names)) {
    if (
      has_perfect_affine_dependence(
        x = regressor_matrix[, regressor_index],
        y = target_values
      )
    ) {
      rlang::abort(
        paste0(
          "Perfect collinearity detected between `",
          target_name,
          "` and regressor `",
          regressor_names[[regressor_index]],
          "` in the finalized estimation set."
        ),
        call = call
      )
    }
  }

  invisible(NULL)
}


#' @keywords internal
#' @noRd
extend_indicator_series <- function(
  indicator_tbl,
  indicator_id,
  indicator_meta,
  target_meta,
  target_anchor,
  future_target_times,
  obs_per_target,
  predict_method,
  call = rlang::caller_env()
) {
  indicator_tbl <- indicator_tbl |>
    dplyr::arrange(.data$time)

  target_periods <- compute_target_periods(
    indicator_tbl$time,
    target_anchor = target_anchor,
    target_meta = target_meta
  )

  observed_future <- sum(target_periods %in% future_target_times)
  missing_obs <- max(
    0L,
    obs_per_target * length(future_target_times) - observed_future
  )

  model <- NULL
  if (missing_obs > 0) {
    mean_reference_values <- NULL
    if (identical(predict_method, "mean")) {
      mean_reference_values <- latest_available_block_values(
        indicator_tbl = indicator_tbl,
        obs_per_target = obs_per_target
      )
    }

    # Forecast only the high-frequency points needed to populate future
    # target periods.
    extension <- forecast_indicator_values(
      indicator_tbl = indicator_tbl,
      indicator_meta = indicator_meta,
      n_ahead = missing_obs,
      method = predict_method,
      obs_per_target = obs_per_target,
      mean_reference_values = mean_reference_values,
      call = call
    )
    model <- extension$model

    last_time <- max(indicator_tbl$time)
    new_times <- shift_time_vec(
      time = last_time,
      n = seq_len(missing_obs) * indicator_meta$step[[1]],
      unit = indicator_meta$unit[[1]]
    )

    indicator_tbl <- dplyr::bind_rows(
      indicator_tbl,
      dplyr::tibble(
        id = indicator_id,
        time = new_times,
        values = extension$values
      )
    )
  }

  list(data = indicator_tbl, model = model)
}


#' @keywords internal
#' @noRd
latest_available_block_values <- function(
  indicator_tbl,
  obs_per_target
) {
  utils::tail(
    indicator_tbl$values,
    min(obs_per_target, nrow(indicator_tbl))
  )
}


#' @keywords internal
#' @noRd
forecast_indicator_values <- function(
  indicator_tbl,
  indicator_meta,
  n_ahead,
  method,
  obs_per_target,
  mean_reference_values = NULL,
  call = rlang::caller_env()
) {
  if (method == "last") {
    return(list(
      values = rep(utils::tail(indicator_tbl$values, 1), n_ahead),
      model = NULL
    ))
  }

  if (method == "mean") {
    recent_values <- mean_reference_values %||% utils::tail(
      indicator_tbl$values,
      min(obs_per_target, nrow(indicator_tbl))
    )
    return(list(
      values = rep(mean(recent_values), n_ahead),
      model = NULL
    ))
  }

  xts_series <- suppressMessages(tsbox::ts_xts(indicator_tbl))
  if (method == "auto.arima") {
    model <- forecast::auto.arima(xts_series)
    values <- as.numeric(forecast::forecast(model, h = n_ahead)$mean)
    return(list(values = values, model = model))
  }

  if (method == "ets") {
    model <- forecast::ets(xts_series)
    values <- as.numeric(forecast::forecast(model, h = n_ahead)$mean)
    return(list(values = values, model = model))
  }

  rlang::abort(
    paste0("Unsupported indicator forecasting method `", method, "`."),
    call = call
  )
}


#' @keywords internal
#' @noRd
prepare_indicator_period_blocks <- function(
  indicator_tbl,
  indicator_id,
  target_meta,
  target_anchor,
  obs_per_target,
  call = rlang::caller_env()
) {
  periods <- compute_target_periods(
    indicator_tbl$time,
    target_anchor = target_anchor,
    target_meta = target_meta
  )

  # Build per-period blocks and counts in a single grouped summary.
  grouped <- indicator_tbl |>
    dplyr::mutate(period = periods) |>
    dplyr::group_by(.data$period) |>
    dplyr::arrange(.data$time, .by_group = TRUE) |>
    dplyr::summarise(
      n_obs = dplyr::n(),
      values = list(utils::tail(.data$values, obs_per_target)),
      .groups = "drop"
    )

  if (any(grouped$n_obs < obs_per_target)) {
    rlang::abort(
      paste0(
        "Indicator `", indicator_id,
        "` has fewer observations within at least one target period ",
        "than required by the current frequency mapping (required: ",
        obs_per_target,
        ")."
      ),
      call = call
    )
  }

  n_truncated <- sum(grouped$n_obs > obs_per_target)

  blocks <- do.call(rbind, grouped$values)
  if (is.null(dim(blocks))) {
    blocks <- matrix(blocks, nrow = 1)
  }
  storage.mode(blocks) <- "double"

  list(
    periods = grouped$period,
    blocks = blocks,
    truncation = list(
      indicator_id = indicator_id,
      n_periods = n_truncated
    )
  )
}


#' @keywords internal
#' @noRd
prepare_indicator_direct_blocks <- function(
  indicator_tbl,
  indicator_id,
  target_times,
  obs_per_target,
  call = rlang::caller_env()
) {
  indicator_tbl <- indicator_tbl |>
    dplyr::arrange(.data$time)

  n_available_blocks <- floor(nrow(indicator_tbl) / obs_per_target)
  if (n_available_blocks < 1) {
    rlang::abort(
      paste0(
        "Indicator `", indicator_id,
        "` does not contain enough observations for direct alignment ",
        "(required at least ", obs_per_target,
        ", available: ", nrow(indicator_tbl), ")."
      ),
      call = call
    )
  }

  n_used_obs <- n_available_blocks * obs_per_target
  used_values <- utils::tail(indicator_tbl$values, n_used_obs)
  blocks <- matrix(used_values, ncol = obs_per_target, byrow = TRUE)
  storage.mode(blocks) <- "double"

  list(
    periods = utils::tail(target_times, n_available_blocks),
    blocks = blocks,
    truncation = list(
      indicator_id = indicator_id,
      n_periods = 0
    )
  )
}


#' @keywords internal
#' @noRd
as_unrestricted_indicator_long <- function(indicator_id, periods, blocks) {
  indicator_ids <- paste0(indicator_id, "_hf", seq_len(ncol(blocks)))

  dplyr::tibble(
    id = rep(indicator_ids, each = nrow(blocks)),
    time = rep(periods, times = ncol(blocks)),
    values = as.numeric(as.vector(blocks))
  )
}


#' @keywords internal
#' @noRd
aggregate_indicator_blocks <- function(
  blocks,
  aggregator,
  indicator_id,
  call = rlang::caller_env()
) {
  if (is.character(aggregator)) {
    if (identical(aggregator, "mean")) {
      return(rowMeans(blocks))
    }
    if (identical(aggregator, "last")) {
      return(blocks[, ncol(blocks)])
    }
    if (identical(aggregator, "sum")) {
      return(rowSums(blocks))
    }
    rlang::abort(
      paste0(
        "Unsupported aggregation method for indicator `", indicator_id, "`."
      ),
      call = call
    )
  }

  if (!is.numeric(aggregator)) {
    rlang::abort(
      paste0(
        "Unsupported aggregation method for indicator `", indicator_id, "`."
      ),
      call = call
    )
  }

  if (length(aggregator) != ncol(blocks)) {
    rlang::abort(
      paste0(
        "Numeric weights for indicator `", indicator_id,
        "` must have length ", ncol(blocks), "."
      ),
      call = call
    )
  }

  if (!isTRUE(all.equal(sum(aggregator), 1))) {
    rlang::abort(
      paste0(
        "Numeric weights for indicator `", indicator_id, "` must sum to 1."
      ),
      call = call
    )
  }

  as.numeric(blocks %*% as.numeric(aggregator))
}


#' @keywords internal
#' @noRd
as_indicator_long <- function(indicator_id, periods, values) {
  dplyr::tibble(
    id = indicator_id,
    time = periods,
    values = as.numeric(values)
  )
}


#' @keywords internal
#' @noRd
parametric_positions <- function(aggregator, n_weights) {
  if (n_weights == 1) {
    return(1)
  }

  if (identical(aggregator, "expalmon")) {
    return(seq(-1, 1, length.out = n_weights))
  }

  seq(0, 1, length.out = n_weights)
}


#' @keywords internal
#' @noRd
exp_almon_gradient <- function(parameters, n_weights) {
  basis <- parametric_polynomial_basis(
    aggregator = "expalmon",
    parameters = parameters,
    n_weights = n_weights
  )
  weights <- exp_almon(parameters, n_weights)
  weighted_basis <- colSums(weights * basis)

  basis_centered <- sweep(basis, 2, weighted_basis, FUN = "-")
  basis_centered * as.vector(weights)
}


#' @keywords internal
#' @noRd
parametric_weight_gradient <- function(aggregator, parameters, n_weights) {
  if (n_weights == 1) {
    return(matrix(
      0,
      nrow = 1,
      ncol = parametric_parameter_count(aggregator)
    ))
  }

  if (identical(aggregator, "beta")) {
    eps <- .Machine$double.eps
    positions <- (seq_len(n_weights) - 1) / (n_weights - 1)
    positions[[1]] <- positions[[1]] + eps
    positions[[n_weights]] <- positions[[n_weights]] - eps
    raw_weights <- positions^(parameters[[1]] - 1) *
      (1 - positions)^(parameters[[2]] - 1)
    raw_sum <- sum(raw_weights)
    log_left <- raw_weights * log(positions)
    log_right <- raw_weights * log(1 - positions)

    return(cbind(
      log_left / raw_sum -
        raw_weights * sum(log_left) / raw_sum^2,
      log_right / raw_sum -
        raw_weights * sum(log_right) / raw_sum^2
    ))
  }

  basis <- parametric_polynomial_basis(
    aggregator = aggregator,
    parameters = parameters,
    n_weights = n_weights
  )
  weights <- parametric_weights(
    aggregator = aggregator,
    parameters = parameters,
    n_weights = n_weights
  )
  weighted_basis <- colSums(weights * basis)

  basis_centered <- sweep(basis, 2, weighted_basis, FUN = "-")
  basis_centered * as.vector(weights)
}


#' @keywords internal
#' @noRd
optimizer_scale_derivative <- function(parameters, aggregator) {
  if (identical(aggregator, "beta")) {
    return(as.numeric(parameters))
  }

  rep(1, length(parameters))
}


#' @keywords internal
#' @noRd
build_parametric_derivative_wide <- function(
  indicator_id,
  periods,
  values,
  indic_lags
) {
  derivative_long <- as_indicator_long(
    indicator_id = indicator_id,
    periods = periods,
    values = values
  )

  suppressMessages(tsbox::ts_wide(
    add_indicator_lags(
      derivative_long,
      indic_lags = indic_lags
    )
  ))
}


#' @keywords internal
#' @noRd
compute_parametric_objective_gradient <- function(
  estimation_set,
  target_name,
  regressor_names,
  coefficients,
  residuals,
  parametric_specs,
  parameter_blocks,
  indic_lags
) {
  gradient <- numeric(sum(vapply(
    parametric_specs,
    function(spec) parametric_parameter_count(spec$aggregator),
    FUN.VALUE = integer(1)
  )))
  offset <- 0L
  estimation_times <- estimation_set$time
  regressor_coefficients <- coefficients[regressor_names]
  regressor_coefficients[is.na(regressor_coefficients)] <- 0

  for (indicator_id in names(parametric_specs)) {
    spec <- parametric_specs[[indicator_id]]
    parameters <- parameter_blocks[[indicator_id]]
    weight_gradient <- parametric_weight_gradient(
      aggregator = spec$aggregator,
      parameters = parameters,
      n_weights = ncol(spec$blocks)
    )
    scale_gradient <- optimizer_scale_derivative(
      parameters = parameters,
      aggregator = spec$aggregator
    )

    for (parameter_index in seq_len(ncol(weight_gradient))) {
      derivative_values <- as.numeric(
        spec$blocks %*% weight_gradient[, parameter_index]
      )
      derivative_wide <- build_parametric_derivative_wide(
        indicator_id = indicator_id,
        periods = spec$periods,
        values = derivative_values,
        indic_lags = indic_lags
      )
      matched_rows <- match(estimation_times, derivative_wide$time)
      present <- !is.na(matched_rows)
      prediction_derivative <- numeric(nrow(estimation_set))
      derivative_columns <- intersect(
        setdiff(names(derivative_wide), "time"),
        regressor_names
      )

      for (column_name in derivative_columns) {
        prediction_derivative[present] <- prediction_derivative[present] +
          derivative_wide[[column_name]][matched_rows[present]] *
            regressor_coefficients[[column_name]]
      }

      gradient[[offset + parameter_index]] <- -2 * sum(
        residuals * prediction_derivative,
        na.rm = TRUE
      ) * scale_gradient[[parameter_index]]
    }

    offset <- offset + length(parameters)
  }

  gradient
}


#' @keywords internal
#' @noRd
evaluate_parametric_objective <- function(
  parameters,
  parametric_specs,
  fixed_aggregated,
  target_tbl,
  target_name,
  indic_lags,
  target_lags
) {
  parameter_blocks <- parameter_blocks_from_optimizer(
    parameters = parameters,
    specs = parametric_specs
  )
  parametric_data <- aggregate_parametric_specs(
    parametric_specs = parametric_specs,
    parameter_blocks = parameter_blocks
  )
  estimation_set <- build_bridge_estimation_set(
    target_tbl = target_tbl,
    target_name = target_name,
    feature_long = dplyr::bind_rows(
      fixed_aggregated,
      parametric_data$aggregated
    ),
    indic_lags = indic_lags,
    estimation_times = unique(target_tbl$time),
    target_lags = target_lags
  )

  if (nrow(estimation_set) == 0) {
    return(list(value = Inf, gradient = rep(0, length(parameters))))
  }

  regressor_names <- setdiff(colnames(estimation_set), c("time", target_name))
  if (length(regressor_names) == 0) {
    return(list(value = Inf, gradient = rep(0, length(parameters))))
  }

  fit <- suppressWarnings(try(
    stats::lm.fit(
      x = cbind(
        "(Intercept)" = 1,
        as.matrix(estimation_set[, regressor_names, drop = FALSE])
      ),
      y = estimation_set[[target_name]]
    ),
    silent = TRUE
  ))
  if (inherits(fit, "try-error")) {
    return(list(value = Inf, gradient = rep(0, length(parameters))))
  }

  residuals <- stats::residuals(fit)
  rss <- sum(residuals^2, na.rm = TRUE)
  if (!is.finite(rss)) {
    return(list(value = Inf, gradient = rep(0, length(parameters))))
  }

  coefficient_names <- c("(Intercept)", regressor_names)
  coefficients <- stats::setNames(
    as.numeric(fit$coefficients),
    coefficient_names
  )

  list(
    value = rss,
    gradient = compute_parametric_objective_gradient(
      estimation_set = estimation_set,
      target_name = target_name,
      regressor_names = regressor_names,
      coefficients = coefficients,
      residuals = residuals,
      parametric_specs = parametric_specs,
      parameter_blocks = parameter_blocks,
      indic_lags = indic_lags
    )
  )
}


#' @keywords internal
#' @noRd
parametric_polynomial_basis <- function(aggregator, parameters, n_weights) {
  positions <- parametric_positions(aggregator, n_weights)

  if (identical(aggregator, "expalmon")) {
    return(vapply(
      seq_along(parameters),
      function(i) positions^i,
      FUN.VALUE = numeric(n_weights)
    ))
  }

  rlang::abort(
    paste0("Unsupported polynomial aggregator `", aggregator, "`."),
    call = rlang::caller_env()
  )
}


#' @keywords internal
#' @noRd
parametric_weights <- function(aggregator, parameters, n_weights) {
  expected_length <- parametric_parameter_count(aggregator)
  if (!is.numeric(parameters) || length(parameters) != expected_length) {
    rlang::abort(
      paste0(
        "`parameters` must contain exactly ",
        expected_length,
        " value",
        if (expected_length == 1) "" else "s",
        " for `",
        aggregator,
        "` weights."
      ),
      call = rlang::caller_env()
    )
  }

  if (identical(aggregator, "beta")) {
    if (n_weights == 1) {
      return(1)
    }

    eps <- .Machine$double.eps
    positions <- (seq_len(n_weights) - 1) / (n_weights - 1)
    positions[[1]] <- positions[[1]] + eps
    positions[[n_weights]] <- positions[[n_weights]] - eps
    raw_weights <- positions^(parameters[[1]] - 1) *
      (1 - positions)^(parameters[[2]] - 1)
    return(raw_weights / sum(raw_weights))
  }

  basis <- parametric_polynomial_basis(
    aggregator = aggregator,
    parameters = parameters,
    n_weights = n_weights
  )
  log_weights <- drop(basis %*% parameters)
  log_weights <- log_weights - max(log_weights)
  raw_weights <- exp(log_weights)
  raw_weights / sum(raw_weights)
}


#' @keywords internal
#' @noRd
parametric_bounds <- function(specs) {
  n_params <- vapply(
    specs,
    function(spec) parametric_parameter_count(spec$aggregator),
    FUN.VALUE = integer(1)
  )
  total <- sum(n_params)

  list(
    lower = rep(PARAMETRIC_OPT_BOUNDS[[1]], total),
    upper = rep(PARAMETRIC_OPT_BOUNDS[[2]], total)
  )
}


#' @keywords internal
#' @noRd
split_parameter_vector <- function(parameters, specs) {
  block_sizes <- vapply(
    specs,
    function(spec) parametric_parameter_count(spec$aggregator),
    FUN.VALUE = integer(1)
  )
  split_indices <- rep(seq_along(specs), times = block_sizes)
  split(parameters, split_indices) |>
    stats::setNames(names(specs))
}


#' @keywords internal
#' @noRd
flatten_parameter_blocks <- function(parameter_blocks, specs) {
  unlist(
    lapply(
      names(specs),
      function(indicator_id) {
        as.numeric(parameter_blocks[[indicator_id]])
      }
    ),
    use.names = FALSE
  )
}


#' @keywords internal
#' @noRd
to_optimizer_scale <- function(parameters, aggregator) {
  parameters <- as.numeric(parameters)

  if (identical(aggregator, "beta")) {
    return(log(parameters))
  }

  parameters
}


#' @keywords internal
#' @noRd
from_optimizer_scale <- function(parameters, aggregator) {
  parameters <- as.numeric(parameters)

  if (identical(aggregator, "beta")) {
    return(exp(parameters))
  }

  parameters
}


#' @keywords internal
#' @noRd
parameter_blocks_from_optimizer <- function(parameters, specs) {
  optimizer_blocks <- split_parameter_vector(
    parameters = parameters,
    specs = specs
  )

  blocks <- lapply(
    names(specs),
    function(indicator_id) {
      from_optimizer_scale(
        parameters = optimizer_blocks[[indicator_id]],
        aggregator = specs[[indicator_id]]$aggregator
      )
    }
  )
  names(blocks) <- names(specs)

  blocks
}


#' @keywords internal
#' @noRd
aggregate_parametric_specs <- function(parametric_specs, parameter_blocks) {
  aggregated <- vector("list", length(parametric_specs))
  weights <- vector("list", length(parametric_specs))

  for (i in seq_along(parametric_specs)) {
    spec <- parametric_specs[[i]]
    indicator_id <- spec$indicator_id
    # Rebuild each indicator's target-frequency series from the current
    # weight guess.
    current_weights <- parametric_weights(
      aggregator = spec$aggregator,
      parameters = parameter_blocks[[indicator_id]],
      n_weights = ncol(spec$blocks)
    )
    weights[[indicator_id]] <- current_weights
    aggregated[[i]] <- as_indicator_long(
      indicator_id = indicator_id,
      periods = spec$periods,
      values = drop(spec$blocks %*% current_weights)
    )
  }

  list(
    aggregated = dplyr::bind_rows(aggregated),
    weights = weights
  )
}


#' @keywords internal
#' @noRd
run_parametric_optimizer <- function(
  objective,
  gradient,
  start,
  lower,
  upper,
  solver_options
) {
  method <- solver_options$method
  if (method == "nlminb") {
    fit <- stats::nlminb(
      start = start,
      objective = objective,
      gradient = gradient,
      lower = lower,
      upper = upper,
      control = list(
        trace = solver_options$trace,
        eval.max = solver_options$maxiter * 2L,
        iter.max = solver_options$maxiter,
        rel.tol = solver_options$reltol
      )
    )
    return(list(
      par = fit$par,
      value = fit$objective,
      convergence = fit$convergence,
      message = fit$message,
      method = method
    ))
  }

  args <- list(
    par = start,
    fn = objective,
    method = method,
    control = list(
      trace = solver_options$trace,
      maxit = solver_options$maxiter
    )
  )
  if (identical(method, "L-BFGS-B")) {
    args$control$factr <- max(solver_options$reltol / .Machine$double.eps, 1)
  } else {
    args$control$reltol <- solver_options$reltol
  }
  if (!is.null(gradient) && !identical(method, "Nelder-Mead")) {
    args$gr <- gradient
  }
  if (method == "L-BFGS-B") {
    args$lower <- lower
    args$upper <- upper
  }

  fit <- do.call(stats::optim, args)
  list(
    par = fit$par,
    value = fit$value,
    convergence = fit$convergence,
    message = fit$message %||% "",
    method = method
  )
}


#' @srrstats {RE3.0} Warns before keeping the best non-converged result.
#' @keywords internal
#' @noRd
optimize_parametric_weights <- function(
  parametric_specs,
  fixed_aggregated,
  target_tbl,
  target_name,
  indic_lags,
  target_lags,
  solver_options,
  call = rlang::caller_env()
) {
  # `solver_options` are expected to be pre-normalized by
  # `validate_bridge_inputs()`; this function does not re-validate them.
  indicator_ids <- names(parametric_specs)
  base_blocks <- solver_options$start_values
  if (is.null(base_blocks)) {
    base_blocks <- lapply(
      parametric_specs,
      function(spec) default_parametric_start(spec$aggregator)
    )
    names(base_blocks) <- indicator_ids
  }

  base_start <- flatten_parameter_blocks(
    parameter_blocks = lapply(
      names(parametric_specs),
      function(indicator_id) {
        to_optimizer_scale(
          parameters = base_blocks[[indicator_id]],
          aggregator = parametric_specs[[indicator_id]]$aggregator
        )
      }
    ) |>
      stats::setNames(names(parametric_specs)),
    specs = parametric_specs
  )
  bounds <- parametric_bounds(parametric_specs)
  lower <- bounds$lower
  upper <- bounds$upper
  evaluation_cache <- new.env(parent = emptyenv())

  # Score candidate weights by the final bridge-model fit, not indicator
  # fit alone.
  evaluate <- function(parameters) {
    cache_key <- paste(signif(parameters, 16), collapse = "\r")
    if (exists(cache_key, envir = evaluation_cache, inherits = FALSE)) {
      return(get(cache_key, envir = evaluation_cache, inherits = FALSE))
    }

    result <- evaluate_parametric_objective(
      parameters = parameters,
      parametric_specs = parametric_specs,
      fixed_aggregated = fixed_aggregated,
      target_tbl = target_tbl,
      target_name = target_name,
      indic_lags = indic_lags,
      target_lags = target_lags
    )
    assign(cache_key, result, envir = evaluation_cache)
    result
  }
  objective <- function(parameters) evaluate(parameters)$value
  gradient <- function(parameters) evaluate(parameters)$gradient

  run_starts <- function() {
    lapply(
      seq_len(solver_options$n_starts),
      function(start_index) {
        current_start <- base_start
        if (start_index > 1) {
          # Jitter later starts so the optimizer can escape poor local
          # solutions.
          current_start <- current_start + stats::rnorm(
            length(base_start),
            mean = 0,
            sd = PARAMETRIC_MULTISTART_JITTER_SD
          )
          current_start <- pmax(pmin(current_start, upper), lower)
        }
        run_parametric_optimizer(
          objective = objective,
          gradient = gradient,
          start = current_start,
          lower = lower,
          upper = upper,
          solver_options = solver_options
        )
      }
    )
  }

  results <- if (is.null(solver_options$seed)) {
    run_starts()
  } else {
    withr::with_seed(solver_options$seed, run_starts())
  }

  converged <- vapply(
    results,
    function(result) isTRUE(result$convergence == 0),
    FUN.VALUE = logical(1)
  )
  objective_values <- vapply(
    results,
    function(result) result$value,
    FUN.VALUE = numeric(1)
  )
  objective_values[!is.finite(objective_values)] <- Inf

  if (any(converged)) {
    best_index <- which.min(ifelse(converged, objective_values, Inf))
  } else {
    best_index <- which.min(objective_values)
  }
  best_result <- results[[best_index]]

  if (!is.finite(best_result$value)) {
    rlang::abort(
      paste(
        "Joint parametric aggregation optimization failed to find",
        "a finite objective value."
      ),
      call = call
    )
  }

  if (!isTRUE(best_result$convergence == 0) && isTRUE(solver_options$warn)) {
    rlang::warn(
      paste0(
        "Joint parametric aggregation optimization did not fully ",
        "converge (code ",
        best_result$convergence,
        "). Using the best available parameter vector."
      ),
      call = call
    )
  }

  best_blocks <- parameter_blocks_from_optimizer(
    parameters = best_result$par,
    specs = parametric_specs
  )
  parametric_data <- aggregate_parametric_specs(
    parametric_specs = parametric_specs,
    parameter_blocks = best_blocks
  )

  list(
    aggregated = parametric_data$aggregated,
    weights = parametric_data$weights,
    parameters = best_blocks,
    optimization = list(
      method = best_result$method,
      value = best_result$value,
      convergence = best_result$convergence,
      message = best_result$message,
      n_starts = solver_options$n_starts,
      best_start = best_index
    )
  )
}


#' @keywords internal
#' @noRd
add_indicator_lags <- function(data, indic_lags) {
  out <- data

  if (indic_lags == 0) {
    return(out)
  }

  lagged <- vector("list", indic_lags)
  for (lag_index in seq_len(indic_lags)) {
    # Lag each indicator within series so target-period alignment stays intact.
    lagged[[lag_index]] <- data |>
      dplyr::group_by(.data$id) |>
      dplyr::arrange(.data$time, .by_group = TRUE) |>
      dplyr::mutate(
        values = dplyr::lag(.data$values, n = lag_index),
        id = paste0(.data$id, "_lag", lag_index)
      ) |>
      dplyr::ungroup()
  }

  dplyr::bind_rows(out, dplyr::bind_rows(lagged))
}


#' Exponential Almon polynomial weights
#' @keywords internal
#' @noRd
exp_almon <- function(parameters, n_weights) {
  parametric_weights(
    aggregator = "expalmon",
    parameters = parameters,
    n_weights = n_weights
  )
}


#' @keywords internal
#' @noRd
target_lag_regressor_names <- function(target_name, target_lags) {
  if (target_lags < 1) {
    return(character())
  }

  paste0(target_name, "_lag", seq_len(target_lags))
}


#' @keywords internal
#' @noRd
add_target_lagged_regressors <- function(
  data,
  target_name,
  target_lags
) {
  if (target_lags < 1 || nrow(data) == 0) {
    return(data)
  }

  out <- data
  for (lag_index in seq_len(target_lags)) {
    out[[paste0(target_name, "_lag", lag_index)]] <-
      dplyr::lag(out[[target_name]], n = lag_index)
  }

  stats::na.omit(out)
}


#' @keywords internal
#' @noRd
build_bridge_estimation_set <- function(
  target_tbl,
  target_name,
  feature_long,
  indic_lags,
  estimation_times,
  target_lags = 0
) {
  target_long <- target_tbl |>
    dplyr::select("id", "time", "values")

  features_with_lags <- add_indicator_lags(
    feature_long,
    indic_lags = indic_lags
  )
  full_long <- dplyr::bind_rows(target_long, features_with_lags) |>
    dplyr::filter(.data$time %in% estimation_times)

  suppressMessages(tsbox::ts_wide(full_long)) |>
    stats::na.omit() |>
    add_target_lagged_regressors(
      target_name = target_name,
      target_lags = target_lags
    )
}


#' @keywords internal
#' @noRd
bridge_mean_from_parameters <- function(
  coefficient_values,
  formula,
  estimation_set
) {
  model_matrix <- stats::model.matrix(
    object = stats::terms(formula),
    data = estimation_set
  )
  as.numeric(model_matrix %*% coefficient_values[colnames(model_matrix)])
}


#' @keywords internal
#' @noRd
parametric_parameter_labels <- function(parametric_specs) {
  unlist(
    lapply(
      names(parametric_specs),
      function(indicator_id) {
        paste0(
          indicator_id,
          "::",
          parametric_parameter_names(
            parametric_specs[[indicator_id]]$aggregator
          )
        )
      }
    ),
    use.names = FALSE
  )
}


#' @keywords internal
#' @noRd
rebuild_parametric_estimation_set <- function(
  target_tbl,
  target_name,
  fixed_aggregated,
  parametric_specs,
  parameter_blocks,
  indic_lags,
  target_lags
) {
  parametric_data <- aggregate_parametric_specs(
    parametric_specs = parametric_specs,
    parameter_blocks = parameter_blocks
  )

  build_bridge_estimation_set(
    target_tbl = target_tbl,
    target_name = target_name,
    feature_long = dplyr::bind_rows(
      fixed_aggregated,
      parametric_data$aggregated
    ),
    indic_lags = indic_lags,
    estimation_times = unique(target_tbl$time),
    target_lags = target_lags
  )
}


#' @keywords internal
#' @noRd
compute_bridge_loss <- function(
  estimation_set,
  target_name,
  target_lags,
  call = rlang::caller_env()
) {
  if (nrow(estimation_set) == 0) {
    return(Inf)
  }

  regressor_names <- setdiff(colnames(estimation_set), c("time", target_name))
  if (length(regressor_names) == 0) {
    return(Inf)
  }

  fit <- suppressWarnings(try(
    stats::lm.fit(
      x = cbind(
        "(Intercept)" = 1,
        as.matrix(estimation_set[, regressor_names, drop = FALSE])
      ),
      y = estimation_set[[target_name]]
    ),
    silent = TRUE
  ))

  if (inherits(fit, "try-error")) {
    return(Inf)
  }

  rss <- sum(stats::residuals(fit)^2, na.rm = TRUE)
  if (!is.finite(rss)) {
    return(Inf)
  }

  rss
}


#' @keywords internal
#' @noRd
resample_bridge_inputs <- function(
  target_tbl,
  indic_tbl,
  target_meta,
  indic_meta,
  target_anchor,
  h,
  frequency_conversions,
  block_length
) {
  n_periods <- nrow(target_tbl)
  sampled_indices <- moving_block_bootstrap_indices(
    n_rows = n_periods,
    block_length = block_length
  )

  boot_target <- dplyr::tibble(
    id = target_tbl$id[[1]],
    time = target_tbl$time,
    values = target_tbl$values[sampled_indices]
  )

  future_target_times <- build_forecast_target_times(
    last_target_time = max(target_tbl$time),
    target_meta = target_meta,
    h = h
  )

  boot_indic <- lapply(
    seq_len(nrow(indic_meta)),
    function(index) {
      indicator_id <- indic_meta$id[[index]]
      indicator_tbl <- indic_tbl |>
        dplyr::filter(.data$id == indicator_id)

      obs_per_target <- observations_per_target_period(
        indicator_meta = indic_meta[index, , drop = FALSE],
        target_meta = target_meta,
        frequency_conversions = frequency_conversions
      )
      periods <- compute_target_periods(
        indicator_tbl$time,
        target_anchor = target_anchor,
        target_meta = target_meta
      )
      observed_indicator <- indicator_tbl |>
        dplyr::mutate(period = periods) |>
        dplyr::filter(.data$period %in% target_tbl$time) |>
        dplyr::select(-"period")

      observed_blocks <- prepare_indicator_period_blocks(
        indicator_tbl = observed_indicator,
        indicator_id = indicator_id,
        target_meta = target_meta,
        target_anchor = target_anchor,
        obs_per_target = obs_per_target
      )
      observed_period_index <- match(target_tbl$time, observed_blocks$periods)
      resampled_blocks <- observed_blocks$blocks[
        observed_period_index[sampled_indices],
        ,
        drop = FALSE
      ]
      observed_long <- as_indicator_period_long(
        indicator_id = indicator_id,
        target_times = target_tbl$time,
        blocks = resampled_blocks,
        indicator_meta = indic_meta[index, , drop = FALSE]
      )

      future_blocks <- prepare_future_indicator_blocks(
        indicator_tbl = indicator_tbl,
        target_meta = target_meta,
        target_anchor = target_anchor,
        future_target_times = future_target_times
      )
      if (length(future_blocks$values) == 0) {
        return(observed_long)
      }

      future_long <- dplyr::bind_rows(
        lapply(
          seq_along(future_blocks$values),
          function(future_index) {
            dplyr::tibble(
              id = indicator_id,
              time = within_target_period_times(
                period_start = future_blocks$periods[[future_index]],
                indicator_meta = indic_meta[index, , drop = FALSE],
                n_obs = length(future_blocks$values[[future_index]])
              ),
              values = as.numeric(future_blocks$values[[future_index]])
            )
          }
        )
      )

      dplyr::bind_rows(observed_long, future_long)
    }
  )

  list(
    target = boot_target,
    indic = dplyr::bind_rows(boot_indic)
  )
}
