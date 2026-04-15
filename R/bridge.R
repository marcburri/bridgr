#' Estimate a Bridge Model
#'
#' Estimate a bridge model that links one lower-frequency target series to one
#' or more higher-frequency indicator series. Indicators are aligned to the
#' target frequency by forecasting any missing higher-frequency observations and
#' aggregating them within each target period.
#'
#' Supported indicator forecasting methods are `"mean"`, `"last"`,
#' `"auto.arima"`, `"ets"`, and `"direct"`. Supported aggregation methods are
#' `"mean"`, `"last"`, `"sum"`, `"unrestricted"`, `"expalmon"`, `"beta"`,
#' `"legendre"`, or a numeric weight vector supplied inside a `list()`.
#' `"unrestricted"` expands each high-frequency observation within a target
#' period into its own bridge regressor, which corresponds to a U-MIDAS style
#' specification when the frequency gap is small. When one or more indicators
#' use a parametric aggregator, the corresponding aggregation weights are
#' estimated jointly against the final bridge-model objective rather than one
#' indicator at a time.
#'
#' Unrestricted mixed-frequency regressions can become parameter-heavy quickly.
#' When `indic_aggregators = "unrestricted"`, `bridge()` warns if the final
#' estimation sample contains fewer than 10 observations per predictor in the
#' bridge regression.
#'
#' The package assumes a regular frequency ladder
#' `second -> minute -> hour -> day -> week -> month -> quarter -> year`.
#' The default number of lower-level observations per higher-level unit is:
#'
#' - `spm = 60`
#' - `mph = 60`
#' - `hpd = 24`
#' - `dpw = 7`
#' - `wpm = 4`
#' - `mpq = 3`
#' - `qpy = 4`
#'
#' Users can override any subset of these values with
#' `frequency_conversions`. If a target period contains more high-frequency
#' observations than implied by the current mapping, `bridge()` keeps the most
#' recent observations and emits a summarized warning. If a target period
#' contains fewer observations than required, the call fails. Month-, quarter-,
#' and year-based input dates are standardized to period starts when needed
#' for frequency recognition.
#'
#' @param target A single target series in a [tsbox::ts_boxable()] format.
#' @param indic One or more indicator series in a [tsbox::ts_boxable()] format.
#' @param indic_predict A character vector of indicator forecasting methods.
#' Length must be `1` or equal to the number of indicator series. Setting
#' `indic_predict = "direct"` switches to direct MIDAS-style alignment: the
#' indicators are not forecasted, and the most recent complete
#' high-frequency blocks are assigned backward to the target periods. Direct
#' alignment must be used for all indicators at once. When `h > 1`, the latest
#' complete block is assigned to the farthest requested forecast horizon and
#' earlier complete blocks are assigned backward from there. For
#' `indic_predict = "mean"`, missing high-frequency observations are filled
#' with the mean of the latest complete target-period block, and that same
#' mean is extended across the forecast horizon.
#' @param indic_aggregators A character vector of aggregation methods or a list
#' of numeric weights. Length must be `1` or equal to the number of indicator
#' series. Numeric weights must sum to one and have the appropriate length for
#' the inferred target-period block size. `"unrestricted"` keeps one separate
#' coefficient per high-frequency observation within the target period. The
#' parametric aggregators use two coefficients each: `"expalmon"` uses
#' `(linear, quadratic)`, `"beta"` uses `(left_shape, right_shape)` as the
#' normalized beta shape parameters, and `"legendre"` uses
#' `(first_order, second_order)` as coefficients on the first two shifted
#' orthonormal Legendre basis functions.
#' @param indic_lags A non-negative integer giving the number of target-period
#' lags to add for each aggregated indicator.
#' @param target_lags A non-negative integer giving the autoregressive order in
#' the target equation.
#' @param h A positive integer forecast horizon measured in target periods.
#' @param frequency_conversions A named numeric vector used to customize the
#' regular frequency ladder. Supported names are `spm`, `mph`, `hpd`, `dpw`,
#' `wpm`, `mpq`, and `qpy`.
#' @param solver_options A list of optional controls for joint parametric-weight
#' optimization. Supported entries are:
#' `method` for the optimizer (`"L-BFGS-B"`, `"BFGS"`, `"Nelder-Mead"`, or
#' `"nlminb"`), `maxiter` for the iteration budget per optimization run,
#' `n_starts` for the number of multi-start attempts, `seed` for reproducible
#' random restarts, `trace` for optimizer verbosity, and `start_values` for
#' user-supplied initial parameter values. `start_values` can be either a
#' numeric vector or a named list. For a numeric vector, values are concatenated
#' in indicator order across the parametric aggregators. Within each indicator,
#' the parameter order is `(linear, quadratic)` for `"expalmon"`,
#' `(left_shape, right_shape)` for `"beta"`, and `(first_order, second_order)`
#' for `"legendre"`. Named-list `start_values` must provide exactly the
#' required number of values for each parametric indicator. These controls are
#' ignored unless at least one indicator uses a parametric aggregator.
#' @param ... Reserved for future extensions.
#'
#' @return An object of class `"bridge"` containing the standardized input
#' series, inferred frequencies, aligned estimation and forecast datasets, the
#' fitted target model, fitted indicator models, and metadata required by
#' [forecast.bridge()] and [summary.bridge()].
#'
#' @examples
#' gdp_growth <- suppressMessages(tsbox::ts_na_omit(tsbox::ts_pc(gdp)))
#'
#' model <- bridge(
#'   target = gdp_growth,
#'   indic = baro,
#'   indic_predict = "auto.arima",
#'   indic_aggregators = "mean",
#'   indic_lags = 1,
#'   target_lags = 1,
#'   h = 2
#' )
#'
#' expalmon_model <- bridge(
#'   target = gdp_growth,
#'   indic = baro,
#'   indic_predict = "auto.arima",
#'   indic_aggregators = "expalmon",
#'   solver_options = list(seed = 123, n_starts = 3),
#'   h = 1
#' )
#'
#' forecast(model)
#' summary(expalmon_model)
#'
#' @references
#' Baffigi, A., Golinelli, R., & Parigi, G. (2004). Bridge models to forecast
#' the euro area GDP. *International Journal of Forecasting*, 20(3), 447-460.
#' \doi{10.1016/S0169-2070(03)00067-0}
#'
#' Ghysels, E., Sinko, A., & Valkanov, R. (2007). MIDAS regressions: Further
#' results and new directions. *Econometric Reviews*, 26(1), 53-90.
#' \doi{10.1080/07474930600972467}
#'
#' Andreou, E., Ghysels, E., & Kourtellos, A. (2010). Regression models with
#' mixed sampling frequencies. *Journal of Econometrics*, 158(2), 246-261.
#' \doi{10.1016/j.jeconom.2010.01.004}
#'
#' Schumacher, C. (2016). A comparison of MIDAS and bridge equations.
#' *International Journal of Forecasting*, 32(2), 257-270.
#' \doi{10.1016/j.ijforecast.2015.07.004}
#'
#' Burri, M. *Oxford Bulletin of Economics and Statistics*.
#' \url{https://onlinelibrary.wiley.com/doi/10.1111/obes.70073}
#' @export
bridge <- function(
  target,
  indic,
  indic_predict = NULL,
  indic_aggregators = NULL,
  indic_lags = 0,
  target_lags = 0,
  h = 1,
  frequency_conversions = NULL,
  solver_options = NULL,
  ...
) {
  target_name <- deparse(substitute(target))
  indic_name <- deparse(substitute(indic))

  # Standardize inputs before any frequency or aggregation logic.
  target_tbl <- as_bridge_tbl(
    x = target,
    arg = "target",
    default_id = target_name
  )
  indic_tbl <- as_bridge_tbl(
    x = indic,
    arg = "indic",
    default_id = indic_name
  )
  target_tbl <- normalize_period_start_data(target_tbl)
  indic_tbl <- normalize_period_start_data(indic_tbl)

  config <- validate_bridge_inputs(
    target_tbl = target_tbl,
    indic_tbl = indic_tbl,
    indic_predict = indic_predict,
    indic_aggregators = indic_aggregators,
    indic_lags = indic_lags,
    target_lags = target_lags,
    h = h,
    frequency_conversions = frequency_conversions,
    solver_options = solver_options
  )

  # Carry the target symbol into the final model formula and output.
  target_tbl <- target_tbl |>
    dplyr::mutate(id = target_name)

  target_meta <- infer_frequency_table(target_tbl)$target
  indic_meta <- infer_frequency_table(indic_tbl)$indicators |>
    dplyr::arrange(match(.data$id, unique(indic_tbl$id)))

  # Keep only the shared sample and record the target-period anchor.
  aligned <- align_bridge_inputs(
    target_tbl = target_tbl,
    indic_tbl = indic_tbl,
    target_meta = target_meta,
    indic_meta = indic_meta
  )
  target_tbl <- aligned$target
  indic_tbl <- aligned$indic
  target_anchor <- aligned$target_anchor

  # Build the future target index once so indicator extension can fill it.
  last_target_time <- max(target_tbl$time)
  future_target_times <- target_future_times(
    last_time = last_target_time,
    target_meta = target_meta,
    h = h
  )
  all_target_times <- c(unique(target_tbl$time), future_target_times)

  # Aggregate each indicator to target frequency, with optional parametric weights.
  indicator_results <- build_indicator_features(
    indic_tbl = indic_tbl,
    indic_meta = indic_meta,
    target_tbl = target_tbl,
    target_meta = target_meta,
    target_anchor = target_anchor,
    all_target_times = all_target_times,
    future_target_times = future_target_times,
    indic_predict = config$indic_predict,
    indic_aggregators = config$indic_aggregators,
    frequency_conversions = config$frequency_conversions,
    indic_lags = indic_lags,
    target_lags = target_lags,
    solver_options = config$solver_options
  )

  target_long <- target_tbl |>
    dplyr::select("id", "time", "values")

  feature_long <- add_indicator_lags(
    indicator_results$aggregated,
    indic_lags = indic_lags
  )

  # Split the combined design matrix into estimation and forecast windows.
  full_long <- dplyr::bind_rows(target_long, feature_long) |>
    dplyr::filter(.data$time %in% all_target_times)

  estimation_long <- full_long |>
    dplyr::filter(.data$time %in% target_tbl$time)
  forecast_long <- full_long |>
    dplyr::filter(.data$time %in% future_target_times)

  # Drop incomplete rows introduced by lagging before fitting the model.
  estimation_set <- suppressMessages(tsbox::ts_wide(estimation_long)) |>
    stats::na.omit()
  forecast_set <- suppressMessages(tsbox::ts_wide(forecast_long)) |>
    stats::na.omit()

  if (nrow(estimation_set) == 0) {
    rlang::abort(
      paste(
        "No complete estimation rows remain after aligning and",
        "aggregating the data."
      ),
      call = rlang::caller_env()
    )
  }

  regressor_names <- setdiff(colnames(estimation_set), c("time", target_name))
  if (length(regressor_names) == 0) {
    rlang::abort(
      paste(
        "At least one aggregated indicator is required to estimate",
        "a bridge model."
      ),
      call = rlang::caller_env()
    )
  }

  if (any(vapply(config$indic_aggregators, identical, logical(1), "unrestricted"))) {
    unrestricted_ratio <- nrow(estimation_set) / length(regressor_names)
    if (is.finite(unrestricted_ratio) && unrestricted_ratio < 10) {
      rlang::warn(
        paste0(
          "The unrestricted bridge specification leaves only ",
          round(unrestricted_ratio, 2),
          " estimation observations per predictor in the final regression. ",
          "This is below the common 10-observations-per-predictor guideline ",
          "and may indicate an over-parameterized U-MIDAS specification."
        ),
        call = rlang::caller_env()
      )
    }
  }

  quoted_regressors <- paste0("`", regressor_names, "`")
  formula <- stats::as.formula(
    paste0(
      "`",
      target_name,
      "` ~ ",
      paste(quoted_regressors, collapse = " + ")
    )
  )

  estimation_xts <- suppressMessages(
    tsbox::ts_xts(tsbox::ts_long(estimation_set))
  )
  xreg_estimation <- estimation_xts[, regressor_names, drop = FALSE]

  # Fit a linear bridge equation unless target AR dynamics are requested.
  if (target_lags == 0) {
    model_fit <- stats::lm(formula = formula, data = estimation_set)
  } else {
    model_fit <- forecast::Arima(
      y = estimation_xts[, target_name],
      order = c(target_lags, 0, 0),
      xreg = xreg_estimation
    )
  }

  structure(
    list(
      target = target_tbl,
      indic = indic_tbl,
      target_name = target_name,
      indic_name = unique(indic_tbl$id),
      target_frequency = target_meta,
      indicator_frequencies = indic_meta,
      indic_predict = config$indic_predict,
      indic_aggregators = config$indic_aggregators,
      indic_lags = indic_lags,
      target_lags = target_lags,
      h = h,
      frequency_conversions = config$frequency_conversions,
      solver_options = config$solver_options,
      formula = formula,
      estimation_set = estimation_set,
      forecast_set = forecast_set,
      model = model_fit,
      indic_models = indicator_results$models,
      parametric_weights = indicator_results$parametric_weights,
      parametric_parameters = indicator_results$parametric_parameters,
      parametric_optimization = indicator_results$parametric_optimization,
      expalmon_weights = indicator_results$expalmon_weights,
      expalmon_parameters = indicator_results$expalmon_parameters,
      expalmon_optimization = indicator_results$expalmon_optimization,
      truncation_info = indicator_results$truncation_info,
      regressor_names = regressor_names,
      target_anchor = target_anchor,
      future_target_times = future_target_times
    ),
    class = "bridge"
  )
}

#' @keywords internal
#' @noRd
validate_bridge_inputs <- function(
  target_tbl,
  indic_tbl,
  indic_predict,
  indic_aggregators,
  indic_lags,
  target_lags,
  h,
  frequency_conversions,
  solver_options = NULL,
  call = rlang::caller_env()
) {
  if (length(unique(target_tbl$id)) != 1) {
    rlang::abort(
      "`target` must contain exactly one time series.",
      call = call
    )
  }

  if (nrow(indic_tbl) == 0 || length(unique(indic_tbl$id)) == 0) {
    rlang::abort(
      "`indic` must contain at least one time series.",
      call = call
    )
  }

  check_bridge_series(target_tbl, "target", call = call)
  check_bridge_series(indic_tbl, "indic", call = call)

  if (!is.numeric(indic_lags) ||
    length(indic_lags) != 1 ||
    indic_lags < 0 ||
    indic_lags != as.integer(indic_lags)) {
    rlang::abort(
      "`indic_lags` must be a single non-negative integer.",
      call = call
    )
  }
  if (!is.numeric(target_lags) ||
    length(target_lags) != 1 ||
    target_lags < 0 ||
    target_lags != as.integer(target_lags)) {
    rlang::abort(
      "`target_lags` must be a single non-negative integer.",
      call = call
    )
  }
  if (!is.numeric(h) || length(h) != 1 || h < 1 || h != as.integer(h)) {
    rlang::abort("`h` must be a single positive integer.", call = call)
  }

  frequency_conversions <- normalize_frequency_conversions(
    frequency_conversions,
    call = call
  )
  solver_options <- normalize_parametric_solver_options(
    solver_options,
    call = call
  )

  target_meta <- infer_frequency_table(target_tbl)$target
  indic_meta <- infer_frequency_table(indic_tbl)$indicators

  for (i in seq_len(nrow(indic_meta))) {
    obs_per_target <- observations_per_target_period(
      indicator_meta = indic_meta[i, , drop = FALSE],
      target_meta = target_meta,
      frequency_conversions = frequency_conversions,
      call = call
    )

    if (obs_per_target < 1) {
      rlang::abort(
        paste0(
          "Indicator `", indic_meta$id[[i]],
          "` is lower-frequency than the target and cannot be used in a ",
          "bridge model."
        ),
        call = call
      )
    }
  }

  n_indicators <- nrow(indic_meta)
  indic_predict <- normalize_indicator_methods(
    methods = indic_predict,
    n_series = n_indicators,
    default = "auto.arima",
    arg = "indic_predict",
    valid = c("mean", "last", "auto.arima", "ets", "direct"),
    call = call
  )
  indic_aggregators <- normalize_indicator_aggregators(
    aggregators = indic_aggregators,
    n_series = n_indicators,
    call = call
  )

  direct_used <- identical(unique(indic_predict), "direct")
  if (any(indic_predict == "direct") && !direct_used) {
    rlang::abort(
      paste(
        "`indic_predict = \"direct\"` must be used for all indicators or for none."
      ),
      call = call
    )
  }
  parametric_specs <- lapply(
    seq_len(n_indicators),
    function(i) {
      aggregator <- indic_aggregators[[i]]
      if (!is_parametric_aggregator(aggregator)) {
        return(NULL)
      }

      list(
        indicator_id = indic_meta$id[[i]],
        aggregator = aggregator
      )
    }
  )
  parametric_specs <- Filter(Negate(is.null), parametric_specs)
  names(parametric_specs) <- vapply(
    parametric_specs,
    function(spec) spec$indicator_id,
    FUN.VALUE = character(1)
  )
  solver_options <- validate_parametric_solver_start(
    solver_options = solver_options,
    parametric_specs = parametric_specs,
    call = call
  )

  list(
    indic_predict = indic_predict,
    indic_aggregators = indic_aggregators,
    frequency_conversions = frequency_conversions,
    solver_options = solver_options
  )
}

#' @keywords internal
#' @noRd
build_indicator_features <- function(
  indic_tbl,
  indic_meta,
  target_tbl,
  target_meta,
  target_anchor,
  all_target_times,
  future_target_times,
  indic_predict,
  indic_aggregators,
  frequency_conversions,
  indic_lags = 0,
  target_lags = 0,
  solver_options = normalize_parametric_solver_options(NULL),
  call = rlang::caller_env()
) {
  aggregated_fixed <- vector("list", length = nrow(indic_meta))
  models <- vector("list", length = nrow(indic_meta))
  parametric_weights <- list()
  parametric_parameters <- list()
  parametric_optimization <- NULL
  expalmon_weights <- list()
  expalmon_parameters <- list()
  truncation_info <- vector("list", length = nrow(indic_meta))
  parametric_specs <- list()

  target_name <- unique(target_tbl$id)

  for (i in seq_len(nrow(indic_meta))) {
    indicator_id <- indic_meta$id[[i]]
    indicator_tbl <- indic_tbl |>
      dplyr::filter(.data$id == indicator_id)

    obs_per_target <- observations_per_target_period(
      indicator_meta = indic_meta[i, , drop = FALSE],
      target_meta = target_meta,
      frequency_conversions = frequency_conversions,
      call = call
    )

    direct_mode <- identical(indic_predict[[i]], "direct")
    if (direct_mode) {
      indicator_extension <- list(data = indicator_tbl, model = NULL)
      aggregation <- prepare_indicator_direct_blocks(
        indicator_tbl = indicator_tbl,
        indicator_id = indicator_id,
        target_times = all_target_times,
        obs_per_target = obs_per_target,
        call = call
      )
    } else {
      indicator_extension <- extend_indicator_series(
        indicator_tbl = indicator_tbl,
        indicator_id = indicator_id,
        indicator_meta = indic_meta[i, , drop = FALSE],
        target_meta = target_meta,
        target_anchor = target_anchor,
        future_target_times = future_target_times,
        obs_per_target = obs_per_target,
        predict_method = indic_predict[[i]],
        call = call
      )
      indicator_tbl <- indicator_extension$data

      aggregation <- prepare_indicator_period_blocks(
        indicator_tbl = indicator_tbl,
        indicator_id = indicator_id,
        target_meta = target_meta,
        target_anchor = target_anchor,
        obs_per_target = obs_per_target,
        call = call
      )
    }

    models[i] <- list(indicator_extension$model)
    truncation_info[[i]] <- aggregation$truncation

    aggregator <- indic_aggregators[[i]]
    if (is_parametric_aggregator(aggregator)) {
      parametric_specs[[indicator_id]] <- list(
        indicator_id = indicator_id,
        aggregator = aggregator,
        periods = aggregation$periods,
        blocks = aggregation$blocks,
        obs_per_target = obs_per_target
      )
    } else if (is.character(aggregator) && identical(aggregator, "unrestricted")) {
      aggregated_fixed[[i]] <- as_unrestricted_indicator_long(
        indicator_id = indicator_id,
        periods = aggregation$periods,
        blocks = aggregation$blocks
      )
    } else {
      aggregated_fixed[[i]] <- as_indicator_long(
        indicator_id = indicator_id,
        periods = aggregation$periods,
        values = aggregate_indicator_blocks(
          blocks = aggregation$blocks,
          aggregator = aggregator,
          indicator_id = indicator_id,
          call = call
        )
      )
    }
  }

  fixed_aggregated <- dplyr::bind_rows(aggregated_fixed)
  if (is.null(fixed_aggregated) || nrow(fixed_aggregated) == 0) {
    fixed_aggregated <- dplyr::tibble(
      id = character(),
      time = target_tbl$time[0],
      values = numeric()
    )
  }
  if (length(parametric_specs) > 0) {
    parametric_fit <- optimize_parametric_weights(
      parametric_specs = parametric_specs,
      fixed_aggregated = fixed_aggregated,
      target_tbl = target_tbl,
      target_name = target_name,
      indic_lags = indic_lags,
      target_lags = target_lags,
      solver_options = solver_options,
      call = call
    )
    fixed_aggregated <- dplyr::bind_rows(fixed_aggregated, parametric_fit$aggregated)
    parametric_weights <- parametric_fit$weights
    parametric_parameters <- parametric_fit$parameters
    parametric_optimization <- parametric_fit$optimization
    expalmon_ids <- names(parametric_specs)[vapply(
      parametric_specs,
      function(spec) identical(spec$aggregator, "expalmon"),
      FUN.VALUE = logical(1)
    )]
    expalmon_weights <- parametric_weights[expalmon_ids]
    expalmon_parameters <- parametric_parameters[expalmon_ids]
    if (length(expalmon_ids) > 0) {
      expalmon_optimization <- parametric_optimization
    } else {
      expalmon_optimization <- NULL
    }
  } else {
    expalmon_optimization <- NULL
  }

  truncation_info <- stats::setNames(truncation_info, indic_meta$id)
  affected <- vapply(
    truncation_info,
    function(info) !is.null(info) && info$n_periods > 0,
    FUN.VALUE = logical(1)
  )
  if (any(affected)) {
    details <- vapply(
      truncation_info[affected],
      function(info) {
        paste0(
          info$indicator_id,
          " (",
          info$n_periods,
          " period",
          if (info$n_periods == 1) "" else "s",
          ")"
        )
      },
      FUN.VALUE = character(1)
    )
    rlang::warn(
      paste0(
        paste(
          "Some indicators had more observations within a target period",
          "than implied by the current frequency mapping."
        ),
        "Using the most recent observations for: ",
        paste(details, collapse = ", "),
        "."
      ),
      call = call
    )
  }

  list(
    aggregated = fixed_aggregated,
    models = stats::setNames(models, indic_meta$id),
    parametric_weights = parametric_weights,
    parametric_parameters = parametric_parameters,
    parametric_optimization = parametric_optimization,
    expalmon_weights = expalmon_weights,
    expalmon_parameters = expalmon_parameters,
    expalmon_optimization = expalmon_optimization,
    truncation_info = truncation_info
  )
}
