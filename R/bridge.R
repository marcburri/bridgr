#' Estimate a Mixed-Frequency Model
#'
#' Estimate a bridge model that links one lower-frequency target series to one
#' or more higher-frequency indicator series. Indicators are aligned to the
#' target frequency by forecasting any missing higher-frequency observations and
#' aggregating them within each target period.
#'
#' Supported indicator forecasting methods are `"mean"`, `"last"`,
#' `"auto.arima"`, `"ets"`, and `"direct"`. Supported aggregation methods are
#' `"mean"`, `"last"`, `"sum"`, `"unrestricted"`, `"expalmon"`, `"beta"`, or
#' a numeric weight vector supplied inside a `list()`.
#' `"unrestricted"` expands each high-frequency observation within a target
#' period into its own bridge regressor, which corresponds to a U-MIDAS style
#' specification when the frequency gap is small. When one or more indicators
#' use a parametric aggregator, the corresponding aggregation weights are
#' estimated jointly against the final bridge-model objective rather than one
#' indicator at a time.
#'
#' Unrestricted mixed-frequency regressions can become parameter-heavy quickly.
#' When `indic_aggregators = "unrestricted"`, `mf_model()` warns if the final
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
#' observations than implied by the current mapping, `mf_model()` keeps the most
#' recent observations and emits a summarized warning. If a target period
#' contains fewer observations than required, the call fails. Month-, quarter-,
#' and year-based input dates are standardized to period starts when needed
#' for frequency recognition.
#'
#' @param target A single target series in a [tsbox::ts_boxable()] format,
#' such as a data frame or tibble with `time` and `value`/`values` columns,
#' or a regular time-series object supported by tsbox.
#' @param indic One or more indicator series in a [tsbox::ts_boxable()]
#' format, such as data frames / tibbles with `time` and `value`/`values`
#' columns, or regular time-series objects supported by tsbox.
#' @param indic_predict A character vector of indicator forecasting methods.
#' Length must be `1` or equal to the number of indicator series. Setting
#' `indic_predict = "direct"` switches to direct MIDAS-style alignment: the
#' indicators are not forecasted, and the most recent complete
#' high-frequency blocks are assigned backward to the target periods. Direct
#' alignment must be used for all indicators at once. When `h > 1`, the latest
#' complete block is assigned to the farthest requested forecast horizon and
#' earlier complete blocks are assigned backward from there. For
#' `indic_predict = "mean"`, missing high-frequency observations are filled
#' with the mean of the latest available `obs_per_target` high-frequency
#' observations, and that same mean is extended across the forecast horizon.
#' @param indic_aggregators A character vector of aggregation methods or a list
#' of numeric weights. Length must be `1` or equal to the number of indicator
#' series. Numeric weights must sum to one and have the appropriate length for
#' the inferred target-period block size. `"unrestricted"` keeps one separate
#' coefficient per high-frequency observation within the target period. The
#' parametric aggregators use two coefficients each: `"expalmon"` uses
#' `(linear, quadratic)`, and `"beta"` uses `(left_shape, right_shape)` as the
#' normalized beta shape parameters. When `indic_predict = "direct"`,
#' `indic_aggregators` is ignored and direct blocks are averaged within each
#' target period.
#' @param indic_lags A non-negative integer giving the number of target-period
#' lags to add for each aggregated indicator.
#' @param target_lags A non-negative integer giving the autoregressive order in
#' the target equation.
#' @param h A positive integer forecast horizon measured in target periods.
#' @param frequency_conversions A named numeric vector used to customize the
#' regular frequency ladder. Supported names are `spm`, `mph`, `hpd`, `dpw`,
#' `wpm`, `mpq`, and `qpy`.
#' @param se Logical flag indicating whether coefficient standard errors and
#' prediction intervals should be computed. When `TRUE`, `mf_model()`
#' reports HAC
#' standard errors for the linear target equation, or Delta-HAC standard errors
#' when parametric aggregation weights are estimated jointly.
#' @param bootstrap A list of uncertainty controls. Currently only
#' `list(N = 100, block_length = NULL)` is supported. `N` is the number of
#' predictive simulation paths used when `se = TRUE`. If
#' `full_system_bootstrap = TRUE`, the same `N` controls the number of
#' full-system target-period block-bootstrap replications used for prediction
#' intervals. `block_length` is only used by the full-system bootstrap. When
#' `block_length` is `NULL`, `mf_model()` uses `ceiling(n^(1/3))` based on the
#' final target-period sample size.
#' @param full_system_bootstrap Logical flag indicating whether prediction
#' intervals and coefficient standard errors should be based on a full-system
#' target-period block bootstrap instead of residual resampling and HAC /
#' Delta-HAC uncertainty from the fitted target equation. This option is only
#' used when `se = TRUE`. Because it refits the full bridge workflow on every
#' draw, `full_system_bootstrap = TRUE` can be substantially slower than the
#' default residual-resampling intervals.
#' @param solver_options A list of optional controls for joint parametric-weight
#' optimization. Supported entries are:
#' `method` for the optimizer (`"L-BFGS-B"`, `"BFGS"`, `"Nelder-Mead"`, or
#' `"nlminb"`), `maxiter` for the iteration budget per optimization run,
#' `n_starts` for the number of multi-start attempts, `seed` for reproducible
#' random restarts, `trace` for optimizer verbosity, `reltol` for the relative
#' convergence tolerance passed through to the selected optimizer backend, and
#' `start_values` for user-supplied initial parameter values. Documented
#' defaults are `method = "L-BFGS-B"`, `maxiter = 1000`, `n_starts = 5`,
#' `trace = 0`, and `reltol = 1e-8`. `start_values` can be either a numeric
#' vector or a named list. For a numeric vector, values are concatenated in
#' indicator order across the parametric aggregators. Within each indicator,
#' the parameter order is `(linear, quadratic)` for `"expalmon"` and
#' `(left_shape, right_shape)` for `"beta"`. Named-list `start_values` must
#' provide exactly the required number of values for each parametric indicator.
#' Users can override `reltol` in any call through
#' `solver_options = list(reltol = ...)`. These controls are ignored unless at
#' least one indicator uses a parametric aggregator.
#'
#' @return An object of class `"mf_model"` containing the standardized input
#' series, inferred frequencies, aligned estimation and forecast datasets, the
#' fitted target model, fitted indicator models, and metadata required by
#' [forecast.mf_model()] and [summary.mf_model()].
#'
#' @section Model specification:
#' `bridgr` does not use a formula interface. Mixed-frequency bridge models are
#' specified through separate `target` and `indic` series plus the forecasting,
#' aggregation, and lag controls because the package must standardize,
#' align, extend, and aggregate the time-series inputs before the final target
#' regression formula can be assembled.
#'
#' @section Input standardization:
#' Submitted series are converted to a common internal table with `id`, `time`,
#' and numeric `values` columns before model fitting. This means that
#' additional input attributes beyond the series identifier, timestamps, and
#' numeric values are not preserved in the fitted object unless they are
#' re-encoded in those standardized columns.
#'
#' @section Input assumptions:
#' `bridgr` assumes that submitted target and indicator series are ordered by
#' time within each series, free of duplicate timestamps and explicit missing
#' values, and regular enough for the package to infer a supported target and
#' indicator frequency. It also assumes that indicator series are at least as
#' high-frequency as the target. Violations of these assumptions are rejected
#' during preprocessing and validation rather than being silently repaired.
#'
#' @section Stationarity:
#' `bridgr` assumes that users provide target and indicator series on a scale
#' that is appropriate for bridge-style forecasting. In practice this often
#' means working with growth rates, differences, or other transformed series
#' prepared upstream. This expectation primarily concerns the lower-order
#' moments that matter most for bridge-style forecasting, typically the mean
#' and variance of the submitted series. The package does not automatically
#' test for or enforce stationarity before fitting the bridge regression.
#'
#' @section Deprecated `bridge()` wrapper:
#' [bridge()] is retained for compatibility and forwards to `mf_model()` with a
#' deprecation warning.
#'
#' @examples
#' gdp_growth <- tsbox::ts_pc(gdp)
#' gdp_growth <- tsbox::ts_na_omit(gdp_growth)
#' gdp_growth <- dplyr::slice_tail(gdp_growth, n = 12)
#' baro_small <- dplyr::slice_tail(baro, n = 36)
#'
#' mf_model(
#'   target = gdp_growth,
#'   indic = baro_small,
#'   indic_predict = "auto.arima",
#'   indic_aggregators = "mean",
#'   indic_lags = 1,
#'   target_lags = 1,
#'   h = 1,
#'   frequency_conversions = c(mpq = 3),
#'   se = TRUE,
#'   bootstrap = list(N = 2),
#'   solver_options = list(seed = 123, n_starts = 1)
#' )
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
#' Burri, M. (2026). Nowcasting Swiss GDP Growth From Public Lead Texts:
#' Simple Methods Are Sufficient. *Oxford Bulletin of Economics and
#' Statistics*, 1-25. \doi{10.1111/obes.70073}
#'
#' @srrstats {G1.0} `mf_model()` documents primary academic references for bridge and MIDAS models in its `@references` section.
#' @srrstats {G2.0a} The parameter documentation states explicit length rules for vector inputs such as `indic_predict` and `indic_aggregators`.
#' @srrstats {G2.1a} The parameter documentation states explicit expected types for vector inputs such as character method vectors, numeric weight vectors, and named numeric conversion vectors.
#' @srrstats {TS1.1} The `target` and `indic` parameters document accepted ts-boxable time-series and tabular input forms.
#' @srrstats {TS1.8} The documentation explicitly states the regular frequency ladder and default conversion counts used to relate days, weeks, months, quarters, and years.
#' @srrstats {RE1.2} The `indic` parameter documents accepted predictor formats as ts-boxable time-series inputs.
#' @srrstats {RE2.0} The documentation explains default input transformations, including period-start normalization, indicator extension, and aggregation.
#' @srrstats {G3.1} The uncertainty arguments document more than one covariance-estimation route, including HAC / Delta-HAC uncertainty and optional full-system block-bootstrap uncertainty, rather than relying only on `stats::cov()`.
#' @srrstats {TS4.0} The fitted object satisfies the return-value standard through a documented custom `"mf_model"` class.
#' @srrstats {TS4.0b} `mf_model()` returns a class-defined `"mf_model"` object.
#' @srrstats {TS4.2} The `@return` section documents the class and main contents of the `"mf_model"` object.
#' @srrstats {TS4.3} The returned model stores explicit time-scale information through target and indicator frequency metadata plus aligned time-indexed estimation and forecast datasets.
#' @srrstats {TS4.7b} The returned model keeps aligned estimation and forecast values in separate list items, `estimation_set` and `forecast_set`.
#' @srrstats {RE4.0} `mf_model()` returns a model object of class `"mf_model"`.
#' @srrstats {RE1.0} The documentation explicitly explains why `bridgr` uses separate mixed-frequency series inputs instead of a formula interface.
#' @srrstats {RE1.3a} The documentation explicitly states that preprocessing drops arbitrary extra input attributes when data are standardized to the package's common internal table format.
#' @srrstats {RE1.4} The public documentation states the package assumptions about ordered, regular, non-missing mixed-frequency inputs, and validation tests exercise violations such as duplicate timestamps, explicit missing values, and lower-frequency indicators.
#' @srrstats {RE3.2} The `solver_options` documentation states the default convergence-control values used for joint parametric optimization, including the default relative tolerance and iteration budget.
#' @srrstats {RE3.3} The public `solver_options` argument explicitly exposes `reltol`, allowing users to set the convergence tolerance for joint parametric optimization.
#' @srrstats {TS2.2} The stationarity documentation explicitly states that users are expected to prepare bridge inputs so the relevant lower-order moments, typically mean and variance, are on an appropriate scale before fitting.
#' @srrstats {TS2.3} The documentation explicitly states that stationarity-relevant transformations are expected to happen upstream rather than being imposed automatically by the package.
#' @export
mf_model <- function(
  target,
  indic,
  indic_predict = NULL,
  indic_aggregators = NULL,
  indic_lags = 0,
  target_lags = 0,
  h = 1,
  frequency_conversions = NULL,
  se = FALSE,
  bootstrap = NULL,
  full_system_bootstrap = FALSE,
  solver_options = NULL
) {
  target_name <- bridge_argument_label(substitute(target), "target")
  indic_name <- bridge_argument_label(substitute(indic), "indic")

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
    se = se,
    bootstrap = bootstrap,
    full_system_bootstrap = full_system_bootstrap,
    solver_options = solver_options
  )

  # Carry the target symbol into the final model formula and output.
  target_tbl <- target_tbl |>
    dplyr::mutate(id = target_name)
  fit_bridge_model(
    target_tbl = target_tbl,
    indic_tbl = indic_tbl,
    target_name = target_name,
    config = config
  )
}

#' @param ... Arguments forwarded to [mf_model()].
#' @rdname mf_model
#' @export
bridge <- function(...) {
  .Deprecated(new = "mf_model", package = "bridgr")
  mf_model(...)
}

#' @srrstats {RE4.7} When parametric aggregation is used, convergence information is stored in `parametric_optimization` on the returned model object.
#' @srrstats {RE4.8} The returned model stores the standardized target series and target-frequency metadata.
#' @srrstats {RE4.13} The returned model stores indicator series, indicator-frequency metadata, and regressor names.
#' @keywords internal
#' @noRd
fit_bridge_model <- function(
  target_tbl,
  indic_tbl,
  target_name,
  config
) {
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
    h = config$h
  )
  all_target_times <- c(unique(target_tbl$time), future_target_times)

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
    indic_lags = config$indic_lags,
    target_lags = config$target_lags,
    solver_options = config$solver_options
  )

  target_long <- target_tbl |>
    dplyr::select("id", "time", "values")
  feature_long <- add_indicator_lags(
    indicator_results$aggregated,
    indic_lags = config$indic_lags
  )
  full_long <- dplyr::bind_rows(target_long, feature_long) |>
    dplyr::filter(.data$time %in% all_target_times)

  estimation_long <- full_long |>
    dplyr::filter(.data$time %in% target_tbl$time)
  forecast_long <- full_long |>
    dplyr::filter(.data$time %in% future_target_times)

  estimation_set <- suppressMessages(tsbox::ts_wide(estimation_long)) |>
    stats::na.omit() |>
    add_target_lagged_regressors(
      target_name = target_name,
      target_lags = config$target_lags
    )
  forecast_base_set <- suppressMessages(tsbox::ts_wide(forecast_long)) |>
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

  target_lag_names <- target_lag_regressor_names(
    target_name = target_name,
    target_lags = config$target_lags
  )
  regressor_names <- setdiff(colnames(estimation_set), c("time", target_name))
  xreg_names <- setdiff(regressor_names, target_lag_names)
  if (length(xreg_names) == 0) {
    rlang::abort(
      paste(
        "At least one aggregated indicator is required to estimate",
        "a bridge model."
      ),
      call = rlang::caller_env()
    )
  }

  check_estimation_set_collinearity(
    estimation_set = estimation_set,
    target_name = target_name,
    regressor_names = regressor_names,
    call = rlang::caller_env()
  )

  if (
    any(vapply(
      config$indic_aggregators,
      identical,
      logical(1),
      "unrestricted"
    ))
  ) {
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

  model_fit <- fit_target_model(
    estimation_set = estimation_set,
    target_name = target_name,
    regressor_names = regressor_names,
    formula = formula,
    target_lags = config$target_lags
  )

  target_history <- if (config$target_lags > 0) {
    utils::tail(estimation_set[[target_name]], config$target_lags)
  } else {
    NULL
  }
  point_path <- recursive_lm_forecast(
    model = model_fit,
    forecast_set = forecast_base_set,
    target_name = target_name,
    target_lags = config$target_lags,
    target_history = target_history
  )

  coefficient_uncertainty <- if (isTRUE(config$se)) {
    compute_bridge_coefficient_uncertainty(
      model = model_fit,
      formula = formula,
      target_tbl = target_tbl,
      target_name = target_name,
      fixed_aggregated = indicator_results$fixed_aggregated,
      parametric_specs = indicator_results$parametric_specs,
      parameter_blocks = indicator_results$parametric_parameters,
      indic_lags = config$indic_lags,
      target_lags = config$target_lags
    )
  } else {
    list(method = NULL, covariance = NULL, se = NULL)
  }

  full_bootstrap <- if (
    isTRUE(config$se) && isTRUE(config$full_system_bootstrap)
  ) {
    bootstrap_bridge_system(
      enabled = TRUE,
      target_tbl = target_tbl,
      indic_tbl = indic_tbl,
      target_name = target_name,
      target_meta = target_meta,
      indic_meta = indic_meta,
      target_anchor = target_anchor,
      h = config$h,
      frequency_conversions = config$frequency_conversions,
      config = config,
      coefficient_names = names(stats::coef(model_fit)),
      point_forecast = point_path$mean,
      call = rlang::caller_env()
    )
  } else {
    list(
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
    )
  }
  full_bootstrap$requested <- isTRUE(config$se) &&
    isTRUE(config$full_system_bootstrap)
  prediction_uncertainty <- build_prediction_uncertainty(
    enabled = isTRUE(config$se),
    model = model_fit,
    forecast_set = forecast_base_set,
    target_name = target_name,
    regressor_names = regressor_names,
    target_lags = config$target_lags,
    target_history = target_history,
    bootstrap = config$bootstrap,
    full_system_bootstrap = isTRUE(config$full_system_bootstrap),
    full_bootstrap = full_bootstrap
  )
  if (isTRUE(config$full_system_bootstrap)) {
    coefficient_uncertainty <- if (isTRUE(full_bootstrap$enabled)) {
      list(
        method = "block_bootstrap",
        covariance = full_bootstrap$coefficient_covariance,
        se = full_bootstrap$coefficient_se
      )
    } else {
      list(method = NULL, covariance = NULL, se = NULL)
    }
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
      indic_aggregators_requested = config$indic_aggregators_requested,
      indic_aggregators = config$indic_aggregators,
      indic_lags = config$indic_lags,
      target_lags = config$target_lags,
      target_lag_names = target_lag_names,
      h = config$h,
      frequency_conversions = config$frequency_conversions,
      se = config$se,
      full_system_bootstrap = config$full_system_bootstrap,
      bootstrap = full_bootstrap,
      uncertainty = list(
        enabled = isTRUE(config$se),
        coefficient_method = coefficient_uncertainty$method,
        coefficient_covariance = coefficient_uncertainty$covariance,
        coefficient_se = coefficient_uncertainty$se,
        prediction_method = prediction_uncertainty$method,
        prediction_draws = prediction_uncertainty$draws,
        simulation_paths = prediction_uncertainty$N
      ),
      solver_options = config$solver_options,
      formula = formula,
      estimation_set = estimation_set,
      forecast_base_set = forecast_base_set,
      forecast_set = point_path$forecast_set,
      model = model_fit,
      indic_models = indicator_results$models,
      parametric_weights = indicator_results$parametric_weights,
      parametric_parameters = indicator_results$parametric_parameters,
      parametric_specs = indicator_results$parametric_specs,
      fixed_aggregated = indicator_results$fixed_aggregated,
      parametric_optimization = indicator_results$parametric_optimization,
      expalmon_weights = indicator_results$expalmon_weights,
      expalmon_parameters = indicator_results$expalmon_parameters,
      expalmon_optimization = indicator_results$expalmon_optimization,
      truncation_info = indicator_results$truncation_info,
      regressor_names = regressor_names,
      xreg_names = xreg_names,
      target_anchor = target_anchor,
      future_target_times = future_target_times
    ),
    class = "mf_model"
  )
}

#' @srrstats {G2.0} Public scalar and per-indicator controls are checked for required lengths before model fitting.
#' @srrstats {G2.1} Public control arguments are type-checked here, while `as_bridge_tbl()` validates the time-series inputs.
#' @srrstats {G2.2} `target` is restricted to exactly one submitted series, while indicator multiplicity is handled explicitly.
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
  se = FALSE,
  bootstrap = NULL,
  full_system_bootstrap = FALSE,
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
  if (!is.logical(se) || length(se) != 1 || is.na(se)) {
    rlang::abort("`se` must be either `TRUE` or `FALSE`.", call = call)
  }
  if (!is.logical(full_system_bootstrap) ||
    length(full_system_bootstrap) != 1 ||
    is.na(full_system_bootstrap)) {
    rlang::abort(
      "`full_system_bootstrap` must be either `TRUE` or `FALSE`.",
      call = call
    )
  }

  frequency_conversions <- normalize_frequency_conversions(
    frequency_conversions,
    call = call
  )
  bootstrap <- normalize_bridge_bootstrap(
    bootstrap = bootstrap,
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
  indic_aggregators_requested <- indic_aggregators

  direct_used <- identical(unique(indic_predict), "direct")
  if (any(indic_predict == "direct") && !direct_used) {
    rlang::abort(
      paste(
        paste0(
          "`indic_predict = \"direct\"` must be used for all indicators ",
          "or for none."
        )
      ),
      call = call
    )
  }
  if (direct_used) {
    indic_aggregators <- rep(list("mean"), n_indicators)
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
    indic_aggregators_requested = indic_aggregators_requested,
    indic_aggregators = indic_aggregators,
    indic_lags = as.integer(indic_lags),
    target_lags = as.integer(target_lags),
    h = as.integer(h),
    frequency_conversions = frequency_conversions,
    se = isTRUE(se),
    full_system_bootstrap = isTRUE(full_system_bootstrap),
    bootstrap = bootstrap,
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
    } else if (
      is.character(aggregator) &&
        identical(aggregator, "unrestricted")
    ) {
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
  fixed_aggregated_base <- fixed_aggregated
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
    fixed_aggregated <- dplyr::bind_rows(
      fixed_aggregated,
      parametric_fit$aggregated
    )
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
    fixed_aggregated = fixed_aggregated_base,
    models = stats::setNames(models, indic_meta$id),
    parametric_weights = parametric_weights,
    parametric_parameters = parametric_parameters,
    parametric_specs = parametric_specs,
    parametric_optimization = parametric_optimization,
    expalmon_weights = expalmon_weights,
    expalmon_parameters = expalmon_parameters,
    expalmon_optimization = expalmon_optimization,
    truncation_info = truncation_info
  )
}
