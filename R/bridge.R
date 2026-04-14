#' Estimate a Bridge Model
#'
#' Estimate a bridge model that links one lower-frequency target series to one
#' or more higher-frequency indicator series. Indicators are aligned to the
#' target frequency by forecasting any missing higher-frequency observations and
#' aggregating them within each target period.
#'
#' Supported indicator forecasting methods are `"mean"`, `"last"`,
#' `"auto.arima"`, and `"ets"`. Supported aggregation methods are `"mean"`,
#' `"last"`, `"sum"`, `"expalmon"`, or a numeric weight vector supplied inside
#' a `list()`. When one or more indicators use `"expalmon"`, the corresponding
#' aggregation weights are estimated jointly against the final bridge-model
#' objective rather than one indicator at a time.
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
#' contains fewer observations than required, the call fails.
#'
#' @param target A single target series in a [tsbox::ts_boxable()] format.
#' @param indic One or more indicator series in a [tsbox::ts_boxable()] format.
#' @param indic_predict A character vector of indicator forecasting methods.
#' Length must be `1` or equal to the number of indicator series.
#' @param indic_aggregators A character vector of aggregation methods or a list
#' of numeric weights. Length must be `1` or equal to the number of indicator
#' series. Numeric weights must sum to one and have the appropriate length for
#' the inferred target-period block size.
#' @param indic_lags A non-negative integer giving the number of target-period
#' lags to add for each aggregated indicator.
#' @param target_lags A non-negative integer giving the autoregressive order in
#' the target equation.
#' @param h A positive integer forecast horizon measured in target periods.
#' @param frequency_conversions A named numeric vector used to customize the
#' regular frequency ladder. Supported names are `spm`, `mph`, `hpd`, `dpw`,
#' `wpm`, `mpq`, and `qpy`.
#' @param solver_options A list of optional controls for joint `expalmon`
#' optimization. Supported entries are `method`, `maxiter`, `n_starts`,
#' `seed`, and `trace`. These controls are ignored unless at least one
#' indicator uses `indic_aggregators = "expalmon"`.
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
#' Burri, M. (2023). Do daily lead texts help nowcasting GDP growth? IRENE
#' Working Papers 23-02.
#' \url{https://www5.unine.ch/RePEc/ftp/irn/pdfs/WP23-02.pdf}
#'
#' Schumacher, C. (2016). A comparison of MIDAS and bridge equations.
#' *International Journal of Forecasting*, 32(2), 257-270.
#' \doi{10.1016/j.ijforecast.2015.07.004}
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
    ...) {

  target_name <- deparse(substitute(target))
  indic_name <- deparse(substitute(indic))

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

  target_tbl <- target_tbl |>
    dplyr::mutate(id = target_name)

  target_meta <- infer_frequency_table(target_tbl)$target
  indic_meta <- infer_frequency_table(indic_tbl)$indicators |>
    dplyr::arrange(match(.data$id, unique(indic_tbl$id)))

  aligned <- align_bridge_inputs(
    target_tbl = target_tbl,
    indic_tbl = indic_tbl,
    target_meta = target_meta,
    indic_meta = indic_meta
  )
  target_tbl <- aligned$target
  indic_tbl <- aligned$indic
  target_anchor <- aligned$target_anchor

  last_target_time <- max(target_tbl$time)
  future_target_times <- target_future_times(
    last_time = last_target_time,
    target_meta = target_meta,
    h = h
  )
  all_target_times <- c(unique(target_tbl$time), future_target_times)

  indicator_results <- build_indicator_features(
    indic_tbl = indic_tbl,
    indic_meta = indic_meta,
    target_tbl = target_tbl,
    target_meta = target_meta,
    target_anchor = target_anchor,
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

  full_long <- dplyr::bind_rows(target_long, feature_long) |>
    dplyr::filter(.data$time %in% all_target_times)

  estimation_long <- full_long |>
    dplyr::filter(.data$time %in% target_tbl$time)
  forecast_long <- full_long |>
    dplyr::filter(.data$time %in% future_target_times)

  estimation_set <- suppressMessages(tsbox::ts_wide(estimation_long)) |>
    stats::na.omit()
  forecast_set <- suppressMessages(tsbox::ts_wide(forecast_long)) |>
    stats::na.omit()

  if (nrow(estimation_set) == 0) {
    rlang::abort(
      "No complete estimation rows remain after aligning and aggregating the data.",
      call = rlang::caller_env()
    )
  }

  regressor_names <- setdiff(colnames(estimation_set), c("time", target_name))
  if (length(regressor_names) == 0) {
    rlang::abort(
      "At least one aggregated indicator is required to estimate a bridge model.",
      call = rlang::caller_env()
    )
  }

  formula <- stats::as.formula(
    paste(target_name, "~", paste(regressor_names, collapse = " + "))
  )

  estimation_xts <- suppressMessages(tsbox::ts_xts(tsbox::ts_long(estimation_set)))
  xreg_estimation <- estimation_xts[, regressor_names, drop = FALSE]

  model_fit <- forecast::Arima(
    y = estimation_xts[, target_name],
    order = c(target_lags, 0, 0),
    xreg = xreg_estimation
  )

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
    call = rlang::caller_env()) {

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

  if (!is.numeric(indic_lags) || length(indic_lags) != 1 || indic_lags < 0 ||
      indic_lags != as.integer(indic_lags)) {
    rlang::abort("`indic_lags` must be a single non-negative integer.", call = call)
  }
  if (!is.numeric(target_lags) || length(target_lags) != 1 || target_lags < 0 ||
      target_lags != as.integer(target_lags)) {
    rlang::abort("`target_lags` must be a single non-negative integer.", call = call)
  }
  if (!is.numeric(h) || length(h) != 1 || h < 1 || h != as.integer(h)) {
    rlang::abort("`h` must be a single positive integer.", call = call)
  }

  frequency_conversions <- normalize_frequency_conversions(
    frequency_conversions,
    call = call
  )
  solver_options <- normalize_expalmon_solver_options(
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
          "` is lower-frequency than the target and cannot be used in a bridge model."
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
    valid = c("mean", "last", "auto.arima", "ets"),
    call = call
  )
  indic_aggregators <- normalize_indicator_aggregators(
    aggregators = indic_aggregators,
    n_series = n_indicators,
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
    future_target_times,
    indic_predict,
    indic_aggregators,
    frequency_conversions,
    indic_lags = 0,
    target_lags = 0,
    solver_options = normalize_expalmon_solver_options(NULL),
    call = rlang::caller_env()) {

  aggregated_fixed <- vector("list", length = nrow(indic_meta))
  models <- vector("list", length = nrow(indic_meta))
  expalmon_weights <- list()
  expalmon_parameters <- list()
  truncation_info <- vector("list", length = nrow(indic_meta))
  expalmon_specs <- list()

  target_name <- unique(target_tbl$id)
  last_target_time <- max(target_tbl$time)

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

    indicator_extension <- extend_indicator_series(
      indicator_tbl = indicator_tbl,
      indicator_id = indicator_id,
      indicator_meta = indic_meta[i, , drop = FALSE],
      target_meta = target_meta,
      target_anchor = target_anchor,
      future_target_times = future_target_times,
      obs_per_target = obs_per_target,
      predict_method = indic_predict[[i]],
      reference_target_time = last_target_time
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

    models[i] <- list(indicator_extension$model)
    truncation_info[[i]] <- aggregation$truncation

    aggregator <- indic_aggregators[[i]]
    if (is.character(aggregator) && identical(aggregator, "expalmon")) {
      expalmon_specs[[indicator_id]] <- list(
        indicator_id = indicator_id,
        periods = aggregation$periods,
        blocks = aggregation$blocks,
        obs_per_target = obs_per_target
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
  expalmon_optimization <- NULL
  if (length(expalmon_specs) > 0) {
    expalmon_fit <- optimize_expalmon_weights(
      expalmon_specs = expalmon_specs,
      fixed_aggregated = fixed_aggregated,
      target_tbl = target_tbl,
      target_name = target_name,
      indic_lags = indic_lags,
      target_lags = target_lags,
      solver_options = solver_options,
      call = call
    )
    fixed_aggregated <- dplyr::bind_rows(
      fixed_aggregated,
      expalmon_fit$aggregated
    )
    expalmon_weights <- expalmon_fit$weights
    expalmon_parameters <- expalmon_fit$parameters
    expalmon_optimization <- expalmon_fit$optimization
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
      function(info) paste0(info$indicator_id, " (", info$n_periods, " period", if (info$n_periods == 1) "" else "s", ")"),
      FUN.VALUE = character(1)
    )
    rlang::warn(
      paste0(
        "Some indicators had more observations within a target period than implied by the current frequency mapping. ",
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
    expalmon_weights = expalmon_weights,
    expalmon_parameters = expalmon_parameters,
    expalmon_optimization = expalmon_optimization,
    truncation_info = truncation_info
  )
}
