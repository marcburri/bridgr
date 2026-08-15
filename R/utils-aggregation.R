
# Minimum estimation-observations-per-predictor threshold below which the
# unrestricted (U-MIDAS) specification triggers a warning.
min_obs_per_predictor <- 10L

# Symmetric bounds on the parametric-aggregator parameters during joint
# optimization, on the optimizer scale (log scale for beta shapes).
parametric_opt_bounds <- c(-10, 10)

# Standard deviation of the Gaussian jitter added to additional multi-start
# initial values in the parametric-aggregator optimizer.
parametric_multistart_jitter_sd <- 0.5


#' Is this a parametric (weight-estimated) aggregator?
#'
#' Called from `validate_mf_inputs()` and `build_indicator_features()` in
#' `mf_model.R` to route an indicator to the joint parametric-weight
#' optimization path instead of a fixed aggregator (`"mean"`, `"last"`,
#' `"sum"`, `"unrestricted"`, or numeric weights).
#'
#' @keywords internal
#' @noRd
is_parametric_aggregator <- function(aggregator) {
  is.character(aggregator) &&
    length(aggregator) == 1 &&
    aggregator %in% c("expalmon", "beta")
}


#' Parameter names for a parametric aggregator
#'
#' Called by `parametric_parameter_count()` immediately below and by
#' `parametric_parameter_labels()` further down in this file, which builds
#' `theta`-vector labels for Delta-HAC standard errors.
#'
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


#' Number of parameters for a parametric aggregator
#'
#' Thin wrapper around `parametric_parameter_names()` above, used throughout
#' this file and in `utils-input.R` wherever code needs to size a parameter
#' vector for `"expalmon"` (2 parameters) or `"beta"` (2 parameters)
#' aggregation without hardcoding the count.
#'
#' @keywords internal
#' @noRd
parametric_parameter_count <- function(aggregator) {
  length(parametric_parameter_names(aggregator))
}


#' Default starting parameters for the parametric-aggregator optimizer
#'
#' Only called from `optimize_parametric_weights()` further down in this
#' file, to fill in a starting value whenever the user does not supply
#' `solver_options$start_values` for a given indicator.
#'
#' @keywords internal
#' @noRd
default_parametric_start <- function(aggregator) {
  if (identical(aggregator, "beta")) {
    return(c(1, 1))
  }

  rep(0, parametric_parameter_count(aggregator))
}


#' Screen the finalized estimation set for exact collinearity
#'
#' Only called from `check_estimation_set()` in `mf_model.R`, right after the
#' estimation set is assembled and before `fit_target_model()` runs. Just
#' dispatches to the two checks below in this file:
#' `check_regressor_collinearity()` (regressor-vs-regressor) and
#' `check_target_regressor_collinearity()` (target-vs-regressor); both abort
#' on the first perfect affine dependence they find.
#'
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


#' Test whether `y` is an exact affine function of `x`
#'
#' Only called by `check_regressor_collinearity()` and
#' `check_target_regressor_collinearity()` immediately below, to check the
#' abort condition in each pairwise comparison. Not called anywhere else.
#'
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


#' Screen finalized regressors pairwise for exact collinearity
#'
#' Called from `check_estimation_set_collinearity()` immediately above. Only
#' calls `has_perfect_affine_dependence()` (also just above) to check the
#' abort condition for each regressor pair; this function exists solely to
#' loop over pairs and raise one explicit error on the first match.
#'
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


#' Screen the target column against each finalized regressor
#'
#' Called from `check_estimation_set_collinearity()` above, as the second of
#' its two checks. Only calls `has_perfect_affine_dependence()` to check the
#' abort condition for each regressor against the target.
#'
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


#' Extend one indicator series with forecasted future values
#'
#' Called from `build_indicator_features()` in `mf_model.R`, once per
#' indicator, whenever `indic_predict` is not `"direct"`. Calls
#' `compute_target_periods()` (`utils-frequency.R`) to place existing
#' observations into target periods, `shift_time_vec()`
#' (`utils-frequency.R`) to build a candidate future time grid, and
#' `forecast_indicator_values()` immediately below to fill in the values for
#' whichever candidate times are needed to complete future target periods.
#'
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

  # Observations in periods beyond the last forecast period cannot enter any
  # regressor; keep them out of the alignment instead of failing validation
  # on a partially observed beyond-horizon period.
  if (length(future_target_times) > 0) {
    horizon_end <- max(future_target_times)
    within_horizon <- target_periods <= horizon_end
    indicator_tbl <- indicator_tbl[within_horizon, , drop = FALSE]
    target_periods <- target_periods[within_horizon]
  }

  # Calendar periods can hold more observations than the regular ladder
  # implies (a quarter has 13 weekly or up to 92 daily observations, while the
  # ladder expects 12 or 84). Filling future periods with a fixed count of
  # grid steps therefore drifts: early future periods absorb the surplus and
  # later ones come up short. Instead, generate a candidate grid, assign each
  # candidate time to its calendar period, and keep exactly the observations
  # each future period still needs.
  last_time <- max(indicator_tbl$time)
  n_candidate <- as.integer(
    ceiling(1.35 * obs_per_target) * length(future_target_times) + 8L
  )
  candidate_times <- shift_time_vec(
    time = last_time,
    n = seq_len(n_candidate) * indicator_meta$step[[1]],
    unit = indicator_meta$unit[[1]]
  )
  candidate_periods <- compute_target_periods(
    candidate_times,
    target_anchor = target_anchor,
    target_meta = target_meta
  )

  keep <- logical(n_candidate)
  for (period in future_target_times) {
    observed_in_period <- sum(target_periods == period)
    needed <- max(0L, obs_per_target - observed_in_period)
    idx <- which(candidate_periods == period)
    keep[utils::head(idx, needed)] <- TRUE
  }
  missing_obs <- if (any(keep)) max(which(keep)) else 0L

  model <- NULL
  if (missing_obs > 0) {
    mean_reference_values <- NULL
    if (identical(predict_method, "mean")) {
      mean_reference_values <- latest_available_block_values(
        indicator_tbl = indicator_tbl,
        obs_per_target = obs_per_target
      )
    }

    # Forecast along the full candidate grid up to the last needed point,
    # then keep only the times that populate future target periods.
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

    keep_head <- keep[seq_len(missing_obs)]
    indicator_tbl <- dplyr::bind_rows(
      indicator_tbl,
      dplyr::tibble(
        id = indicator_id,
        time = candidate_times[seq_len(missing_obs)][keep_head],
        values = extension$values[keep_head]
      )
    )
  }

  list(data = indicator_tbl, model = model)
}


#' Most recent observed block of indicator values
#'
#' Only called from `extend_indicator_series()` above, to seed the
#' `mean_reference_values` used when `indic_predict = "mean"`, so the mean
#' used to fill future observations matches the mean used to fill missing
#' history within the last observed block.
#'
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


#' Forecast an indicator series forward by one of the supported methods
#'
#' Only called from `extend_indicator_series()` above, to fill the
#' higher-frequency observations an indicator is missing in future target
#' periods. Implements `indic_predict = "last"`/`"mean"`/`"auto.arima"`/
#' `"ets"` directly (`"direct"` alignment bypasses this function entirely and
#' is handled by `prepare_indicator_direct_blocks()` below instead).
#'
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

  # The observations are passed as a plain vector: an xts index carries no
  # `tsp`, so both fitters already saw a frequency-1 series whatever the
  # indicator frequency, and going through xts would make it a hard dependency.
  series <- indicator_tbl$values
  if (method == "auto.arima") {
    model <- forecast::auto.arima(series)
    values <- as.numeric(forecast::forecast(model, h = n_ahead)$mean)
    return(list(values = values, model = model))
  }

  if (method == "ets") {
    model <- forecast::ets(series)
    values <- as.numeric(forecast::forecast(model, h = n_ahead)$mean)
    return(list(values = values, model = model))
  }

  rlang::abort(
    paste0("Unsupported indicator forecasting method `", method, "`."),
    call = call
  )
}


#' Group one indicator's observations into per-target-period blocks
#'
#' Called from `build_indicator_features()` in `mf_model.R` for the
#' non-direct alignment path (after `extend_indicator_series()` has filled in
#' any needed forecasts), and again from `resample_mf_inputs()` below in this
#' file when block-bootstrapping. Calls `compute_target_periods()`
#' (`utils-frequency.R`) to assign each observation to a target period, then
#' returns one row of `obs_per_target` values per period as a matrix, ready
#' for `aggregate_indicator_blocks()` or the parametric-weight machinery.
#'
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


#' Build per-target-period blocks under direct MIDAS-style alignment
#'
#' Only called from `build_indicator_features()` in `mf_model.R`, on the
#' `indic_predict = "direct"` path, as the direct-alignment counterpart to
#' `prepare_indicator_period_blocks()` above (no forecasting; blocks are
#' assigned period-relative-anchor lead/lag positions instead).
#'
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
  n <- nrow(indicator_tbl)

  if (n < obs_per_target) {
    rlang::abort(
      paste0(
        "Indicator `", indicator_id,
        "` does not contain enough observations for direct alignment ",
        "(required at least ", obs_per_target,
        ", available: ", n, ")."
      ),
      call = call
    )
  }

  times <- indicator_tbl$time
  values <- indicator_tbl$values
  target_times <- sort(target_times)
  last_obs <- times[n]

  covered <- target_times[target_times <= last_obs]
  uncovered <- target_times[target_times > last_obs]

  block_ends <- integer(0)
  block_periods <- target_times[0]

  # Covered periods are anchored period-aware: the newest observation sits
  # `delta` time units into its own target period, and every earlier period
  # receives the block ending at the same relative position. On a regular
  # ladder this reproduces fixed backward strides exactly; on calendar
  # ladders, where target periods hold varying numbers of observations
  # (e.g. 13-Saturday quarters on a 12-slot weekly ladder), fixed strides
  # drift out of their periods while the period-relative anchor does not.
  if (length(covered) > 0) {
    delta <- as.numeric(last_obs) - as.numeric(max(covered))
    anchors <- vapply(
      seq_along(covered),
      function(k) {
        idx <- which(as.numeric(times) <= as.numeric(covered[k]) + delta)
        if (length(idx) == 0) NA_integer_ else max(idx)
      },
      integer(1)
    )
    keep <- !is.na(anchors) & anchors >= obs_per_target
    block_ends <- anchors[keep]
    block_periods <- covered[keep]
  }

  # Periods beyond the newest observation keep the documented lead
  # convention: the latest complete block is assigned to the farthest
  # requested horizon and earlier complete blocks stride backward from there.
  if (length(uncovered) > 0) {
    strides <- n - (length(uncovered) - seq_along(uncovered)) * obs_per_target
    keep_u <- strides >= obs_per_target
    block_ends <- c(block_ends, strides[keep_u])
    block_periods <- c(block_periods, uncovered[keep_u])
  }

  if (length(block_ends) == 0) {
    rlang::abort(
      paste0(
        "Indicator `", indicator_id,
        "` does not contain enough observations for direct alignment ",
        "(required at least ", obs_per_target,
        " before the first target period, available: ", n, ")."
      ),
      call = call
    )
  }

  blocks <- t(vapply(
    block_ends,
    function(e) values[(e - obs_per_target + 1):e],
    numeric(obs_per_target)
  ))
  storage.mode(blocks) <- "double"

  list(
    periods = block_periods,
    blocks = blocks,
    truncation = list(
      indicator_id = indicator_id,
      n_periods = 0
    )
  )
}


#' Expand an indicator's per-period blocks into one column per HF position
#'
#' Only called from `build_indicator_features()` in `mf_model.R`, when
#' `indic_aggregators = "unrestricted"` for an indicator. Turns the block
#' matrix from `prepare_indicator_period_blocks()` /
#' `prepare_indicator_direct_blocks()` above into long-format rows tagged
#' `"<indicator_id>_hf1"`, `"<indicator_id>_hf2"`, ..., one bridge regressor
#' per within-period high-frequency observation (U-MIDAS style), bypassing
#' `aggregate_indicator_blocks()` below entirely.
#'
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


#' Collapse one indicator's per-period blocks to a single value per period
#'
#' Only called from `build_indicator_features()` in `mf_model.R`, for the
#' fixed (non-parametric, non-unrestricted) aggregators `"mean"`, `"last"`,
#' `"sum"`, and user-supplied numeric weight vectors. Parametric aggregators
#' (`"expalmon"`, `"beta"`) instead go through
#' `optimize_parametric_weights()` / `aggregate_parametric_specs()` further
#' down in this file.
#'
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


#' Wrap one aggregated indicator series into long format
#'
#' Small tibble-building helper called from three places: fixed aggregation
#' in `build_indicator_features()` (`mf_model.R`), the parametric-derivative
#' pivot in `build_parametric_derivative_wide()` below, and the rebuilt
#' aggregated series in `aggregate_parametric_specs()` below. Counterpart to
#' `as_unrestricted_indicator_long()` above, which expands to multiple
#' columns instead of collapsing to one.
#'
#' @keywords internal
#' @noRd
as_indicator_long <- function(indicator_id, periods, values) {
  dplyr::tibble(
    id = indicator_id,
    time = periods,
    values = as.numeric(values)
  )
}


#' Within-period positions used to build the parametric weight curve
#'
#' Only called from `parametric_polynomial_basis()` below, which needs the
#' `x`-axis positions (in `[-1, 1]` for `"expalmon"`, `[0, 1]` for `"beta"`)
#' at which the polynomial/beta-shape basis is evaluated.
#'
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


#' Jacobian of the aggregation weights with respect to the shape parameters
#'
#' Only called from `compute_parametric_objective_gradient()` below, as one
#' step of the analytic gradient used by the joint parametric-weight
#' optimizer in `optimize_parametric_weights()`. Calls
#' `parametric_polynomial_basis()` and `parametric_weights()` below for the
#' `"expalmon"` case; the `"beta"` case is worked out inline because its
#' weight function is not expressed through the shared polynomial basis.
#'
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


#' Chain-rule factor for the optimizer-scale reparameterization
#'
#' Only called from `compute_parametric_objective_gradient()` below. `"beta"`
#' parameters are optimized on a log scale (see `to_optimizer_scale()` /
#' `from_optimizer_scale()` further down), so gradients computed on the
#' natural parameter scale need this `d(natural)/d(optimizer)` factor before
#' they are valid on the scale the optimizer actually searches.
#'
#' @keywords internal
#' @noRd
optimizer_scale_derivative <- function(parameters, aggregator) {
  if (identical(aggregator, "beta")) {
    return(as.numeric(parameters))
  }

  rep(1, length(parameters))
}


#' Pivot one parameter's weight-derivative series to wide (regressor) form
#'
#' Only called from `compute_parametric_objective_gradient()` below, once per
#' parametric parameter, so its per-period derivative values can be matched
#' up against `estimation_set` by time and lag column name. Calls
#' `as_indicator_long()` and `add_indicator_lags()` above to reuse the same
#' long-to-wide pipeline used for the actual aggregated indicator data.
#'
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


#' Analytic gradient of the bridge-model RSS w.r.t. parametric shape params
#'
#' Only called from `evaluate_parametric_objective()` below, as the
#' `gradient` component of its return value, which
#' `optimize_parametric_weights()` passes straight through to
#' `stats::optim()`/`stats::nlminb()`. Combines
#' `parametric_weight_gradient()` and `optimizer_scale_derivative()` above
#' with `build_parametric_derivative_wide()` above via the chain rule to
#' avoid numerical differentiation during the joint optimization.
#'
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


#' Fit a bridge estimation set by OLS and report the RSS, or `NULL` on failure
#'
#' Shared by `evaluate_parametric_objective()` below (which additionally
#' needs the fitted coefficients and residuals for its analytic gradient)
#' and `compute_mf_loss()` further down (which only needs the RSS, and is
#' also exercised directly from `tests/testthat/test-mf_model.R` as
#' `bridgr:::compute_mf_loss()`). Returns `NULL` whenever `estimation_set` is
#' empty, has no regressor columns, `stats::lm.fit()` errors, or the
#' resulting RSS is non-finite — callers turn that into their own failure
#' value (`Inf`, in both current callers).
#'
#' @keywords internal
#' @noRd
fit_estimation_set_rss <- function(estimation_set, target_name) {
  if (nrow(estimation_set) == 0) {
    return(NULL)
  }

  regressor_names <- setdiff(colnames(estimation_set), c("time", target_name))
  if (length(regressor_names) == 0) {
    return(NULL)
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
    return(NULL)
  }

  residuals <- stats::residuals(fit)
  rss <- sum(residuals^2, na.rm = TRUE)
  if (!is.finite(rss)) {
    return(NULL)
  }

  list(
    fit = fit,
    regressor_names = regressor_names,
    residuals = residuals,
    rss = rss
  )
}


#' Objective value and gradient for one trial parametric parameter vector
#'
#' Only called from the `evaluate()` closure inside
#' `optimize_parametric_weights()` below, which caches results per parameter
#' vector before passing `objective`/`gradient` wrapper closures to
#' `run_parametric_optimizer()`. Rebuilds the full bridge estimation set
#' (via `build_mf_estimation_set()` below) for the candidate weights, fits it
#' by OLS via `fit_estimation_set_rss()` above, and reports the residual sum
#' of squares plus its analytic gradient from
#' `compute_parametric_objective_gradient()` above.
#'
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
  estimation_set <- build_mf_estimation_set(
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

  fit_result <- fit_estimation_set_rss(estimation_set, target_name)
  if (is.null(fit_result)) {
    return(list(value = Inf, gradient = rep(0, length(parameters))))
  }

  coefficient_names <- c("(Intercept)", fit_result$regressor_names)
  coefficients <- stats::setNames(
    as.numeric(fit_result$fit$coefficients),
    coefficient_names
  )

  list(
    value = fit_result$rss,
    gradient = compute_parametric_objective_gradient(
      estimation_set = estimation_set,
      target_name = target_name,
      regressor_names = fit_result$regressor_names,
      coefficients = coefficients,
      residuals = fit_result$residuals,
      parametric_specs = parametric_specs,
      parameter_blocks = parameter_blocks,
      indic_lags = indic_lags
    )
  )
}


#' Polynomial basis matrix for exponential-Almon-style weight curves
#'
#' Called from `parametric_weight_gradient()` and `parametric_weights()` in
#' this file, both of which need the same `positions^1, positions^2, ...`
#' basis matrix: the former to build the weight Jacobian, the latter to turn
#' basis coefficients into normalized weights via a softmax. Only defined for
#' `"expalmon"`; `"beta"` weights are computed directly in the two callers
#' instead, since the beta shape is not a linear combination of this basis.
#'
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


#' Turn parametric shape parameters into a normalized weight vector
#'
#' The user-facing computation behind `"expalmon"`/`"beta"` aggregation:
#' called from `aggregate_parametric_specs()` below (to build the aggregated
#' series shown in `summary.mf_model()`'s parametric-weights block) and
#' directly from `tests/testthat/` as `bridgr:::parametric_weights()` to
#' compute exponential-Almon weights independently of the model-fitting
#' path. Calls `parametric_polynomial_basis()` above for the `"expalmon"`
#' case.
#'
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


#' Symmetric optimizer-scale bounds for a set of parametric specs
#'
#' Only called from `optimize_parametric_weights()` below, to size and fill
#' the `lower`/`upper` vectors passed to `run_parametric_optimizer()` (and
#' `L-BFGS-B`/`nlminb` in particular). Uses the module-level
#' `parametric_opt_bounds` constant defined at the top of this file.
#'
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
    lower = rep(parametric_opt_bounds[[1]], total),
    upper = rep(parametric_opt_bounds[[2]], total)
  )
}


#' Split a flat optimizer parameter vector back into per-indicator blocks
#'
#' Inverse of `flatten_parameter_blocks()` below. Called from
#' `parameter_blocks_from_optimizer()` below (after each optimizer
#' iteration), from `validate_parametric_solver_start()` in `utils-input.R`
#' (to check user-supplied `solver_options$start_values`), and from
#' `coefficient_vcov_delta_hac()` in `utils-uncertainty.R` (to perturb one
#' parameter at a time for the Delta-HAC finite-difference Jacobian).
#'
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


#' Flatten per-indicator parameter blocks into one optimizer vector
#'
#' Inverse of `split_parameter_vector()` above. Called from
#' `optimize_parametric_weights()` below to build the optimizer's starting
#' vector, and from `coefficient_vcov_delta_hac()` in `utils-uncertainty.R`
#' to build the `theta` vector perturbed for the Delta-HAC Jacobian.
#'
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


#' Map natural-scale parameters to the scale the optimizer searches
#'
#' Only called from `optimize_parametric_weights()` below, to convert the
#' starting values into optimizer-scale units before the search (`"beta"`
#' shape parameters must stay positive, so they are searched on a log
#' scale). Inverse of `from_optimizer_scale()` immediately below.
#'
#' @keywords internal
#' @noRd
to_optimizer_scale <- function(parameters, aggregator) {
  parameters <- as.numeric(parameters)

  if (identical(aggregator, "beta")) {
    return(log(parameters))
  }

  parameters
}


#' Map optimizer-scale parameters back to the natural parameter scale
#'
#' Only called from `parameter_blocks_from_optimizer()` below, once per
#' optimizer iteration, to turn the raw search vector back into the natural
#' `"beta"`/`"expalmon"` parameters before computing weights. Inverse of
#' `to_optimizer_scale()` immediately above.
#'
#' @keywords internal
#' @noRd
from_optimizer_scale <- function(parameters, aggregator) {
  parameters <- as.numeric(parameters)

  if (identical(aggregator, "beta")) {
    return(exp(parameters))
  }

  parameters
}


#' Turn one flat optimizer vector into named, natural-scale parameter blocks
#'
#' Only called from `evaluate_parametric_objective()` above (once per
#' objective/gradient evaluation) and once more from
#' `optimize_parametric_weights()` below (to decode the final result).
#' Combines `split_parameter_vector()` and `from_optimizer_scale()` above.
#'
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


#' Aggregate every parametric indicator's blocks at a given parameter guess
#'
#' Called from three places that all need "the aggregated series implied by
#' this parameter vector": `evaluate_parametric_objective()` above (during
#' optimization), `optimize_parametric_weights()` below (for the final
#' result), and `rebuild_parametric_estimation_set()` further down (for
#' Delta-HAC standard errors). Calls `parametric_weights()` and
#' `as_indicator_long()` above.
#'
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


#' Run one solver (nlminb/optim) call for the joint parametric optimization
#'
#' Only called from the `run_starts()` closure inside
#' `optimize_parametric_weights()` below, once per multi-start restart.
#' Translates `solver_options$method` into the corresponding `stats::nlminb()`
#' or `stats::optim()` call and normalizes both return shapes into one
#' `list(par, value, convergence, message, method)` result.
#'
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


#' Jointly estimate parametric aggregation weights against the bridge fit
#'
#' The entry point for this file's parametric-aggregation machinery: called
#' once from `build_indicator_features()` in `mf_model.R` whenever at least
#' one indicator uses `indic_aggregators = "expalmon"` or `"beta"`. Runs a
#' multi-start `stats::optim()`/`stats::nlminb()` search (via
#' `run_parametric_optimizer()` above) over `evaluate_parametric_objective()`
#' above, picks the best converged (or least-bad non-converged) result, and
#' returns the final aggregated series, weights, and parameters via
#' `aggregate_parametric_specs()`. Everything else in this file from
#' `is_parametric_aggregator()` down to `run_parametric_optimizer()` exists
#' to support this one function.
#'
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
  # `validate_mf_inputs()`; this function does not re-validate them.
  indicator_ids <- names(parametric_specs)
  base_blocks <- solver_options$start_values
  if (is.null(base_blocks)) {
    base_blocks <- lapply(
      parametric_specs,
      function(spec) default_parametric_start(spec$aggregator)
    )
    names(base_blocks) <- indicator_ids
  }

  base_start_blocks <- lapply(
    names(parametric_specs),
    function(indicator_id) {
      to_optimizer_scale(
        parameters = base_blocks[[indicator_id]],
        aggregator = parametric_specs[[indicator_id]]$aggregator
      )
    }
  ) |>
    stats::setNames(names(parametric_specs))
  base_start <- flatten_parameter_blocks(
    parameter_blocks = base_start_blocks,
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
            sd = parametric_multistart_jitter_sd
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


#' Add `indic_lags` lagged copies of each aggregated indicator series
#'
#' Called from `prepare_estimation_inputs()` in `mf_model.R` (the main
#' non-parametric path), and twice within this file:
#' `build_parametric_derivative_wide()` above and `build_mf_estimation_set()`
#' below, so parametric-weight derivatives and rebuilt estimation sets stay
#' consistent with the same `indic_lags` regressor structure as the main fit.
#'
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


#' Column names for the target's own autoregressive lag regressors
#'
#' Called from `add_target_lagged_regressors()` immediately below,
#' `prepare_estimation_inputs()` in `mf_model.R`, and
#' `recursive_lm_forecast()` in `utils-forecast.R`. All three need the exact
#' same `"<target_name>_lag1"`, `"<target_name>_lag2"`, ... naming
#' convention to stay in sync when reading and writing target-lag columns.
#'
#' @keywords internal
#' @noRd
target_lag_regressor_names <- function(target_name, target_lags) {
  if (target_lags < 1) {
    return(character())
  }

  paste0(target_name, "_lag", seq_len(target_lags))
}


#' Add target autoregressive lag columns and drop the resulting NA rows
#'
#' Called from `prepare_estimation_inputs()` in `mf_model.R` (building the
#' main estimation set) and from `build_mf_estimation_set()` immediately
#' below (rebuilding the estimation set for a candidate parametric weight
#' vector), so both paths compute target lags identically. Uses
#' `target_lag_regressor_names()` above for the column names.
#'
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


#' Build a wide bridge estimation set from long target and feature data
#'
#' Called from `evaluate_parametric_objective()` above (rebuilding the set
#' for each trial parameter vector during optimization) and from
#' `rebuild_parametric_estimation_set()` below (rebuilding it for Delta-HAC
#' standard errors). Not used for the main model fit, which builds its
#' estimation set directly in `prepare_estimation_inputs()` in `mf_model.R`
#' rather than through this helper. Calls `add_indicator_lags()` and
#' `add_target_lagged_regressors()` above.
#'
#' @keywords internal
#' @noRd
build_mf_estimation_set <- function(
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


#' Fitted mean at a rebuilt estimation set, given fixed coefficients
#'
#' Only called from `coefficient_vcov_delta_hac()` in `utils-uncertainty.R`,
#' twice per parametric parameter (once for a `+epsilon` perturbation, once
#' for `-epsilon`), to build the central-difference Jacobian column for that
#' parameter's effect on the fitted target mean.
#'
#' @keywords internal
#' @noRd
mf_mean_from_parameters <- function(
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


#' Column labels for the flattened parametric-parameter (theta) vector
#'
#' Only called from `coefficient_vcov_delta_hac()` in `utils-uncertainty.R`,
#' to name the extra Jacobian columns (beyond the linear-model coefficients)
#' used for Delta-HAC standard errors. Calls `parametric_parameter_names()`
#' near the top of this file.
#'
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


#' Rebuild the bridge estimation set at a given parametric parameter guess
#'
#' Only called from `coefficient_vcov_delta_hac()` in `utils-uncertainty.R`,
#' three times per parameter (baseline, `+epsilon`, `-epsilon`) while
#' building the Delta-HAC Jacobian. Combines `aggregate_parametric_specs()`
#' and `build_mf_estimation_set()` above.
#'
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

  build_mf_estimation_set(
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


#' Residual sum of squares for a bridge estimation set
#'
#' Not called from elsewhere in the package's model-fitting code (the actual
#' fit path uses `stats::lm()` via `fit_target_model()` in
#' `utils-forecast.R`). Thin `Inf`-on-failure wrapper around
#' `fit_estimation_set_rss()` above, kept as a small, directly testable
#' `bridgr:::compute_mf_loss()` unit exercised from
#' `tests/testthat/test-mf_model.R` to check joint-vs-separate parametric
#' aggregation losses.
#'
#' @keywords internal
#' @noRd
compute_mf_loss <- function(estimation_set, target_name) {
  fit_result <- fit_estimation_set_rss(estimation_set, target_name)
  if (is.null(fit_result)) {
    return(Inf)
  }

  fit_result$rss
}


#' Draw one moving-block-bootstrap resample of target and indicator inputs
#'
#' Only called from `bootstrap_mf_system()` in `utils-bootstrap.R`, once per
#' full-system bootstrap replication (`full_system_bootstrap = TRUE`). Calls
#' `moving_block_bootstrap_indices()` (`utils-bootstrap.R`) to draw the
#' resampled target-period order, then rebuilds matching indicator blocks
#' with `prepare_indicator_period_blocks()` and `as_indicator_period_long()`
#' above/`utils-frequency.R`, keeping future (forecast-horizon) indicator
#' observations unresampled.
#'
#' @keywords internal
#' @noRd
resample_mf_inputs <- function(
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
