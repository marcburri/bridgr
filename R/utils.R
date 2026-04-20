#' @keywords internal
#' @noRd
default_frequency_conversions <- function() {
  c(
    "spm" = 60,
    "mph" = 60,
    "hpd" = 24,
    "dpw" = 7,
    "wpm" = 4,
    "mpq" = 3,
    "qpy" = 4
  )
}

#' @keywords internal
#' @noRd
frequency_levels <- function() {
  c("second", "minute", "hour", "day", "week", "month", "quarter", "year")
}

#' @keywords internal
#' @noRd
frequency_edges <- function() {
  c("spm", "mph", "hpd", "dpw", "wpm", "mpq", "qpy")
}

#' @keywords internal
#' @noRd
normalize_frequency_conversions <- function(
  frequency_conversions,
  call = rlang::caller_env()
) {
  defaults <- default_frequency_conversions()
  if (is.null(frequency_conversions)) {
    return(defaults)
  }

  if (!is.numeric(frequency_conversions)) {
    rlang::abort("`frequency_conversions` must be numeric.", call = call)
  }

  if (is.null(names(frequency_conversions))) {
    if (length(frequency_conversions) != length(defaults)) {
      rlang::abort(
        paste0(
          "`frequency_conversions` must be named, or it must provide all ",
          length(defaults), " default values in order."
        ),
        call = call
      )
    }
    names(frequency_conversions) <- names(defaults)
  }

  invalid_names <- setdiff(names(frequency_conversions), names(defaults))
  if (length(invalid_names) > 0) {
    rlang::abort(
      paste0(
        "Invalid names in `frequency_conversions`: ",
        paste(invalid_names, collapse = ", "),
        "."
      ),
      call = call
    )
  }

  if (any(frequency_conversions <= 0)) {
    rlang::abort(
      "All `frequency_conversions` values must be strictly positive.",
      call = call
    )
  }

  defaults[names(frequency_conversions)] <- as.numeric(frequency_conversions)
  defaults
}

#' @keywords internal
#' @noRd
normalize_bridge_bootstrap <- function(
  bootstrap,
  call = rlang::caller_env()
) {
  defaults <- list(
    N = 100L,
    block_length = NULL
  )

  if (is.null(bootstrap)) {
    return(defaults)
  }
  if (!is.list(bootstrap)) {
    rlang::abort("`bootstrap` must be a list.", call = call)
  }

  invalid <- setdiff(names(bootstrap), names(defaults))
  if (length(invalid) > 0) {
    rlang::abort(
      paste0(
        "Invalid `bootstrap` entries: ",
        paste(invalid, collapse = ", "),
        "."
      ),
      call = call
    )
  }

  defaults[names(bootstrap)] <- bootstrap

  if (!is.numeric(defaults$N) ||
    length(defaults$N) != 1 ||
    !is.finite(defaults$N) ||
    defaults$N < 1) {
    rlang::abort(
      "`bootstrap$N` must be a single integer >= 1.",
      call = call
    )
  }
  defaults$N <- as.integer(round(defaults$N))

  if (!is.null(defaults$block_length)) {
    if (!is.numeric(defaults$block_length) ||
      length(defaults$block_length) != 1 ||
      !is.finite(defaults$block_length) ||
      defaults$block_length < 1) {
      rlang::abort(
        "`bootstrap$block_length` must be `NULL` or a single integer >= 1.",
        call = call
      )
    }
    defaults$block_length <- as.integer(round(defaults$block_length))
  }

  defaults
}

#' @keywords internal
#' @noRd
normalize_bridge_default_id <- function(default_id) {
  default_id <- as.character(default_id)
  default_id <- default_id[nzchar(default_id)]

  if (length(default_id) == 0) {
    return("series")
  }

  paste(default_id, collapse = " ")
}

#' @keywords internal
#' @noRd
is_bridge_reference_expr <- function(expr) {
  if (is.symbol(expr)) {
    return(TRUE)
  }

  if (!is.call(expr)) {
    return(FALSE)
  }

  head <- expr[[1]]
  if (!is.symbol(head)) {
    return(FALSE)
  }

  as.character(head) %in% c("$", "[[", "@", "::", ":::")
}

#' @keywords internal
#' @noRd
bridge_argument_label <- function(expr, fallback) {
  if (is_bridge_reference_expr(expr)) {
    return(normalize_bridge_default_id(rlang::as_label(expr)))
  }

  normalize_bridge_default_id(fallback)
}

#' @srrstats {G2.4} Supported inputs are explicitly converted into one standard internal form through character and numeric coercion in the preprocessing helpers, alongside integer normalization for scalar control arguments.
#' @srrstats {G2.4b} Series values are explicitly standardized with `as.numeric()` before downstream use.
#' @srrstats {G2.4c} Series identifiers are explicitly standardized with `as.character()`.
#' @srrstats {G2.10} Downstream column selection happens only after conversion to a standard tibble, so it does not rely on class-specific extraction defaults of the original input object.
#' @srrstats {G2.6} Supported one-dimensional time-series inputs are standardized through tsbox regardless of their original supported class.
#' @srrstats {G2.7} The package accepts multiple ts-boxable tabular and time-series input forms through `tsbox::ts_boxable()`.
#' @srrstats {G2.8} `as_bridge_tbl()` is the common pre-processing step that converts supported inputs to one standardized internal table form.
#' @srrstats {G2.12} List-valued value columns are rejected with an informative preprocessing error before the data are standardized for modeling.
#' @srrstats {TS1.0} Only explicit time-series inputs accepted by `tsbox::ts_boxable()` enter the preprocessing pipeline; generic non-time-series objects are rejected.
#' @srrstats {TS1.2} `as_bridge_tbl()` validates that submitted series are acceptable ts-boxable time-series inputs.
#' @srrstats {TS1.3} `as_bridge_tbl()` and downstream normalization convert accepted inputs to one uniform internal table representation.
#' @srrstats {TS1.5} Input rows are sorted by series id and time before downstream processing.
#' @keywords internal
#' @noRd
as_bridge_tbl <- function(
  x,
  arg,
  default_id,
  call = rlang::caller_env()
) {
  default_id <- normalize_bridge_default_id(default_id)

  if (!tsbox::ts_boxable(x)) {
    rlang::abort(
      paste0("`", arg, "` must be a ts-boxable object."),
      call = call
    )
  }
  if (inherits(x, "data.frame")) {
    raw_values <- x[["values"]]
    if (is.null(raw_values)) {
      raw_values <- x[["value"]]
    }
    if (!is.null(raw_values) && !is.list(raw_values) && !is.numeric(raw_values)) {
      rlang::abort(
        paste0("`", arg, "` must contain a numeric `value`/`values` column."),
        call = call
      )
    }
    if (is.list(raw_values)) {
      rlang::abort(
        paste0(
          "`", arg,
          "` must contain a numeric `value`/`values` column; list columns are ",
          "not supported."
        ),
        call = call
      )
    }
  }

  out <- suppressMessages(tsbox::ts_tbl(x)) |>
    standardize_ts_tbl()
  if (nrow(out) == 0) {
    rlang::abort(
      paste0("`", arg, "` must contain at least one observation."),
      call = call
    )
  }

  if (!"time" %in% names(out) || !"values" %in% names(out)) {
    rlang::abort(
      paste0("`", arg, "` must contain `time` and `value`/`values` columns."),
      call = call
    )
  }
  if (!is.numeric(out$values)) {
    rlang::abort(
      paste0("`", arg, "` must contain a numeric `value`/`values` column."),
      call = call
    )
  }
  if (!"id" %in% names(out)) {
    out$id <- rep(default_id, nrow(out))
  }

  out |>
    dplyr::transmute(
      id = as.character(.data$id %||% default_id),
      time = .data$time,
      values = as.numeric(.data$values)
    ) |>
    dplyr::arrange(.data$id, .data$time)
}

#' @keywords internal
#' @noRd
standardize_ts_tbl <- function(ts_data) {
  suppressMessages(tsbox::ts_tbl(ts_data)) |>
    dplyr::rename(values = dplyr::any_of(c("value", "values")))
}

#' @keywords internal
#' @noRd
as_period_start <- function(times, unit) {
  converted <- lubridate::floor_date(times, unit = unit)

  if (inherits(times, "Date")) {
    return(as.Date(converted))
  }

  as.POSIXct(converted, tz = lubridate::tz(times[[1]]) %||% "UTC")
}

#' @keywords internal
#' @noRd
fallback_period_start_times <- function(times) {
  if (!(inherits(times, "Date") || inherits(times, "POSIXt"))) {
    return(times)
  }

  raw_times <- sort(unique(times))
  if (!is.null(detect_month_frequency(raw_times)) ||
    !is.null(detect_time_frequency(raw_times))) {
    return(times)
  }

  for (unit in c("year", "quarter", "month")) {
    candidate_times <- as_period_start(times, unit = unit)
    if (length(unique(candidate_times)) != length(unique(times))) {
      next
    }
    candidate_frequency <- detect_month_frequency(sort(unique(candidate_times)))

    if (
      !is.null(candidate_frequency) &&
        identical(candidate_frequency$unit, unit)
    ) {
      return(candidate_times)
    }
  }

  times
}

#' @keywords internal
#' @noRd
normalize_period_start_data <- function(data) {
  data |>
    dplyr::group_by(.data$id) |>
    dplyr::group_modify(
      ~ dplyr::mutate(.x, time = fallback_period_start_times(.data$time))
    ) |>
    dplyr::ungroup()
}

#' @srrstats {G2.13} Missing timestamps and values are rejected during early series validation before analytic routines are called.
#' @srrstats {G2.14a} Explicit missing values in submitted series trigger an immediate error during validation.
#' @srrstats {TS2.1a} Submitted series with explicit missing timestamps or values error during preprocessing rather than being analyzed silently.
#' @keywords internal
#' @noRd
check_bridge_series <- function(
  data,
  arg,
  call = rlang::caller_env()
) {
  if (anyNA(data$time)) {
    rlang::abort(
      paste0("`", arg, "` contains missing timestamps."),
      call = call
    )
  }
  if (anyNA(data$values)) {
    rlang::abort(paste0("`", arg, "` contains missing values."), call = call)
  }

  duplicate_rows <- data |>
    dplyr::count(.data$id, .data$time) |>
    dplyr::filter(.data$n > 1)

  if (nrow(duplicate_rows) > 0) {
    rlang::abort(
      paste0(
        "`", arg,
        "` contains duplicate timestamps within at least one series."
      ),
      call = call
    )
  }

  series_sizes <- data |>
    dplyr::count(.data$id)
  if (any(series_sizes$n < 2)) {
    rlang::abort(
      paste0(
        "Each series in `", arg, "` must contain at least two observations."
      ),
      call = call
    )
  }

  invisible(data)
}

#' @keywords internal
#' @noRd
infer_frequency_table <- function(data, call = rlang::caller_env()) {
  metadata <- data |>
    dplyr::group_by(.data$id) |>
    dplyr::group_map(
      ~ infer_series_frequency(.x$time, .y$id[[1]], call = call)
    ) |>
    dplyr::bind_rows()

  list(
    target = metadata[1, , drop = FALSE],
    indicators = metadata
  )
}

#' @keywords internal
#' @noRd
infer_series_frequency <- function(times, id, call = rlang::caller_env()) {
  times <- sort(unique(times))

  # Prefer calendar-aligned month/quarter/year detection when possible.
  month_candidate <- detect_month_frequency(times)
  if (!is.null(month_candidate)) {
    return(dplyr::tibble(
      id = id,
      unit = month_candidate$unit,
      step = month_candidate$step
    ))
  }

  time_candidate <- detect_time_frequency(times)
  if (!is.null(time_candidate)) {
    return(dplyr::tibble(
      id = id,
      unit = time_candidate$unit,
      step = time_candidate$step
    ))
  }

  rlang::abort(
    paste0(
      "Could not infer a supported regular frequency for series `", id, "`."
    ),
    call = call
  )
}

#' @keywords internal
#' @noRd
detect_month_frequency <- function(times) {
  month_index <- lubridate::year(times) * 12L + lubridate::month(times)
  month_diff <- unique(diff(month_index))

  if (length(month_diff) != 1 || month_diff <= 0) {
    return(NULL)
  }

  aligned_to_month <- all(times == lubridate::floor_date(times, unit = "month"))
  if (!aligned_to_month) {
    return(NULL)
  }

  if (month_diff %% 12 == 0 &&
    all(times == lubridate::floor_date(times, unit = "year"))) {
    return(list(unit = "year", step = month_diff / 12))
  }

  if (month_diff %% 3 == 0 &&
    all(times == lubridate::floor_date(times, unit = "quarter"))) {
    return(list(unit = "quarter", step = month_diff / 3))
  }

  list(unit = "month", step = month_diff)
}

#' @keywords internal
#' @noRd
detect_time_frequency <- function(times) {
  times_posix <- as.POSIXct(times, tz = lubridate::tz(times[[1]]) %||% "UTC")
  second_diff <- unique(as.numeric(diff(times_posix), units = "secs"))

  if (length(second_diff) != 1 || second_diff <= 0) {
    return(NULL)
  }

  if (second_diff %% (7 * 24 * 60 * 60) == 0) {
    return(list(unit = "week", step = second_diff / (7 * 24 * 60 * 60)))
  }
  if (second_diff %% (24 * 60 * 60) == 0) {
    return(list(unit = "day", step = second_diff / (24 * 60 * 60)))
  }
  if (second_diff %% (60 * 60) == 0) {
    return(list(unit = "hour", step = second_diff / (60 * 60)))
  }
  if (second_diff %% 60 == 0) {
    return(list(unit = "minute", step = second_diff / 60))
  }

  list(unit = "second", step = second_diff)
}

#' @keywords internal
#' @noRd
align_bridge_inputs <- function(
  target_tbl,
  indic_tbl,
  target_meta,
  indic_meta
) {
  common_start <- max(min(target_tbl$time), min(indic_tbl$time))

  target_tbl <- target_tbl |>
    dplyr::filter(.data$time >= common_start)
  indic_tbl <- indic_tbl |>
    dplyr::filter(.data$time >= common_start)

  list(
    target = target_tbl,
    indic = indic_tbl,
    target_anchor = min(target_tbl$time)
  )
}

#' @srrstats {G2.3} Univariate character control inputs are normalized case-insensitively and restricted to supported method names before use.
#' @srrstats {G2.3a} Indicator forecasting methods are restricted to an allowed set before use.
#' @srrstats {G2.3b} Character method names are normalized with `tolower()` so matching is case-insensitive.
#' @keywords internal
#' @noRd
normalize_indicator_methods <- function(
  methods,
  n_series,
  default,
  arg,
  valid,
  call = rlang::caller_env()
) {
  if (is.null(methods)) {
    return(rep(default, n_series))
  }

  if (length(methods) == 1) {
    methods <- rep(methods, n_series)
  }

  if (length(methods) != n_series) {
    rlang::abort(
      paste0(
        "`", arg,
        "` must have length 1 or match the number of indicator series."
      ),
      call = call
    )
  }

  methods <- tolower(unname(methods))
  invalid <- setdiff(methods, valid)
  if (length(invalid) > 0) {
    rlang::abort(
      paste0(
        "Unsupported values in `", arg, "`: ",
        paste(invalid, collapse = ", "),
        "."
      ),
      call = call
    )
  }

  unname(methods)
}

#' @srrstats {G2.3b} Character aggregation names are normalized with `tolower()` so matching is case-insensitive.
#' @keywords internal
#' @noRd
normalize_indicator_aggregators <- function(
  aggregators,
  n_series,
  call = rlang::caller_env()
) {
  if (is.null(aggregators)) {
    return(rep(list("mean"), n_series))
  }

  if (is.character(aggregators) && length(aggregators) == 1) {
    aggregators <- rep(list(aggregators), n_series)
  } else if (is.character(aggregators)) {
    if (length(aggregators) != n_series) {
      rlang::abort(
        paste(
          "`indic_aggregators` must have length 1 or match the number of",
          "indicator series."
        ),
        call = call
      )
    }
    aggregators <- as.list(unname(aggregators))
  } else if (is.numeric(aggregators)) {
    aggregators <- rep(list(aggregators), n_series)
  } else if (is.list(aggregators) && length(aggregators) == 1) {
    aggregators <- rep(aggregators, n_series)
  }

  if (!is.list(aggregators) || length(aggregators) != n_series) {
    rlang::abort(
      paste(
        "`indic_aggregators` must have length 1 or match the number of",
        "indicator series."
      ),
      call = call
    )
  }

  valid_names <- c("mean", "last", "sum", "expalmon", "beta")
  valid_names <- c(valid_names, "unrestricted")

  aggregators <- lapply(
    aggregators,
    function(aggregator) {
      if (is.character(aggregator)) {
        return(tolower(aggregator))
      }

      aggregator
    }
  )

  for (aggregator in aggregators) {
    if (is.character(aggregator)) {
      if (length(aggregator) != 1 || !aggregator %in% valid_names) {
        rlang::abort(
          paste0(
            "Character values in `indic_aggregators` must be one of: ",
            paste(valid_names, collapse = ", "), "."
          ),
          call = call
        )
      }
    } else if (!is.numeric(aggregator)) {
      rlang::abort(
        "`indic_aggregators` entries must be characters or numeric weights.",
        call = call
      )
    }
  }

  aggregators
}

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

#' @srrstats {G2.3a} `solver_options$method` is validated with `match.arg()` against the supported optimizer set.
#' @srrstats {G2.4a} Integer-valued solver controls are explicitly normalized with `as.integer(round(...))`.
#' @keywords internal
#' @noRd
normalize_parametric_solver_options <- function(
  solver_options,
  call = rlang::caller_env()
) {
  defaults <- list(
    method = "L-BFGS-B",
    maxiter = 1000L,
    n_starts = 5L,
    seed = NULL,
    trace = 0L,
    start_values = NULL
  )

  if (is.null(solver_options)) {
    return(defaults)
  }

  if (!is.list(solver_options)) {
    rlang::abort("`solver_options` must be a list.", call = call)
  }

  invalid <- setdiff(names(solver_options), names(defaults))
  if (length(invalid) > 0) {
    rlang::abort(
      paste0(
        "Invalid `solver_options`: ",
        paste(invalid, collapse = ", "),
        "."
      ),
      call = call
    )
  }

  defaults[names(solver_options)] <- solver_options
  defaults$method <- match.arg(
    defaults$method,
    c("L-BFGS-B", "BFGS", "Nelder-Mead", "nlminb")
  )

  if (!is.numeric(defaults$maxiter) ||
    length(defaults$maxiter) != 1 ||
    !is.finite(defaults$maxiter) ||
    defaults$maxiter < 1) {
    rlang::abort(
      "`solver_options$maxiter` must be a single integer >= 1.",
      call = call
    )
  }
  if (!is.numeric(defaults$n_starts) ||
    length(defaults$n_starts) != 1 ||
    !is.finite(defaults$n_starts) ||
    defaults$n_starts < 1) {
    rlang::abort(
      "`solver_options$n_starts` must be a single integer >= 1.",
      call = call
    )
  }
  if (!is.numeric(defaults$trace) ||
    length(defaults$trace) != 1 ||
    !is.finite(defaults$trace) ||
    defaults$trace < 0) {
    rlang::abort(
      "`solver_options$trace` must be a single integer >= 0.",
      call = call
    )
  }
  if (!is.null(defaults$seed) &&
    (!is.numeric(defaults$seed) ||
      length(defaults$seed) != 1 ||
      !is.finite(defaults$seed))) {
    rlang::abort(
      "`solver_options$seed` must be `NULL` or a single finite number.",
      call = call
    )
  }

  defaults$maxiter <- as.integer(round(defaults$maxiter))
  defaults$n_starts <- as.integer(round(defaults$n_starts))
  defaults$trace <- as.integer(round(defaults$trace))
  if (!is.null(defaults$seed)) {
    defaults$seed <- as.integer(round(defaults$seed))
  }

  defaults
}

#' @keywords internal
#' @noRd
validate_parametric_solver_start <- function(
  solver_options,
  parametric_specs,
  call = rlang::caller_env()
) {
  if (length(parametric_specs) == 0 || is.null(solver_options$start_values)) {
    return(solver_options)
  }

  spec_names <- names(parametric_specs)
  required_lengths <- vapply(
    parametric_specs,
    function(spec) parametric_parameter_count(spec$aggregator),
    FUN.VALUE = integer(1)
  )

  if (is.numeric(solver_options$start_values)) {
    expected_total <- sum(required_lengths)
    if (length(solver_options$start_values) != expected_total) {
      rlang::abort(
        paste0(
          "`solver_options$start_values` must contain exactly ",
          expected_total,
          " values across the parametric indicators."
        ),
        call = call
      )
    }

    if (any(!is.finite(solver_options$start_values))) {
      rlang::abort(
        "`solver_options$start_values` must contain only finite numbers.",
        call = call
      )
    }

    solver_options$start_values <- split_parameter_vector(
      parameters = as.numeric(solver_options$start_values),
      specs = parametric_specs
    )
    names(solver_options$start_values) <- spec_names
  }

  if (!is.list(solver_options$start_values)) {
    rlang::abort(
      paste(
        "`solver_options$start_values` must be a numeric vector or a named",
        "list of numeric vectors."
      ),
      call = call
    )
  }

  if (length(solver_options$start_values) != length(parametric_specs)) {
    rlang::abort(
      paste0(
        "`solver_options$start_values` must provide exactly ",
        length(parametric_specs),
        " parametric indicator start vector",
        if (length(parametric_specs) == 1) "" else "s",
        "."
      ),
      call = call
    )
  }

  if (is.null(names(solver_options$start_values))) {
    if (length(parametric_specs) > 1) {
      rlang::abort(
        paste0(
          paste0(
            "`solver_options$start_values` must be named for the ",
            "parametric indicators: "
          ),
          paste(spec_names, collapse = ", "),
          "."
        ),
        call = call
      )
    }
    names(solver_options$start_values) <- spec_names
  }

  invalid_names <- setdiff(names(solver_options$start_values), spec_names)
  if (length(invalid_names) > 0) {
    rlang::abort(
      paste0(
        "Unknown entries in `solver_options$start_values`: ",
        paste(invalid_names, collapse = ", "),
        "."
      ),
      call = call
    )
  }

  missing_names <- setdiff(spec_names, names(solver_options$start_values))
  if (length(missing_names) > 0) {
    rlang::abort(
      paste0(
        "`solver_options$start_values` is missing values for: ",
        paste(missing_names, collapse = ", "),
        "."
      ),
      call = call
    )
  }

  solver_options$start_values <- lapply(
    spec_names,
    function(indicator_id) {
      start_values <- solver_options$start_values[[indicator_id]]
      required_length <- required_lengths[[indicator_id]]
      aggregator <- parametric_specs[[indicator_id]]$aggregator

      if (!is.numeric(start_values) ||
        any(!is.finite(start_values)) ||
        length(start_values) != required_length) {
        rlang::abort(
          paste0(
            "`solver_options$start_values$", indicator_id,
            "` must contain exactly ",
            required_length,
            " value",
            if (required_length == 1) "" else "s",
            "."
          ),
          call = call
        )
      }

      if (identical(aggregator, "beta") && any(start_values <= 0)) {
        rlang::abort(
          paste0(
            "`solver_options$start_values$", indicator_id,
            "` must be strictly positive for `beta` aggregation."
          ),
          call = call
        )
      }

      as.numeric(start_values)
    }
  )
  names(solver_options$start_values) <- spec_names

  solver_options
}

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
fit_target_model <- function(
  estimation_set,
  target_name,
  regressor_names,
  formula,
  target_lags
) {
  if (target_lags == 0) {
    return(stats::lm(formula = formula, data = estimation_set))
  }

  forecast::Arima(
    y = estimation_set[[target_name]],
    order = c(target_lags, 0, 0),
    xreg = as.matrix(estimation_set[, regressor_names, drop = FALSE])
  )
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
forecast_target_model_mean <- function(
  model,
  forecast_set,
  target_name,
  regressor_names
) {
  if (inherits(model, "lm")) {
    newdata <- forecast_set
    newdata[[target_name]] <- NA_real_
    return(as.numeric(stats::predict(model, newdata = newdata)))
  }

  xreg_values <- if (length(regressor_names) == 0) {
    NULL
  } else {
    as.matrix(forecast_set[, regressor_names, drop = FALSE])
  }

  as.numeric(forecast::forecast(model, xreg = xreg_values)$mean)
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
bootstrap_target_equation <- function(
  enabled,
  model,
  estimation_set,
  forecast_set,
  target_name,
  regressor_names,
  formula,
  target_lags,
  bootstrap,
  call = rlang::caller_env()
) {
  if (!enabled) {
    return(list(
      enabled = FALSE,
      type = bootstrap$type,
      N = bootstrap$N,
      valid_N = 0L,
      block_length = bootstrap$block_length,
      conditional = TRUE,
      coefficient_draws = NULL,
      coefficient_se = NULL,
      forecast_draws = NULL,
      models = NULL
    ))
  }

  n_rows <- nrow(estimation_set)
  block_length <- resolve_bootstrap_block_length(
    n_rows = n_rows,
    block_length = bootstrap$block_length
  )
  coefficient_names <- names(stats::coef(model))
  requested_n <- bootstrap$N

  models <- vector("list", requested_n)
  coefficient_draws <- matrix(
    NA_real_,
    nrow = requested_n,
    ncol = length(coefficient_names),
    dimnames = list(NULL, coefficient_names)
  )
  forecast_draws <- matrix(
    NA_real_,
    nrow = requested_n,
    ncol = nrow(forecast_set)
  )
  valid <- rep(FALSE, requested_n)

  for (draw_index in seq_len(requested_n)) {
    draw_indices <- circular_block_bootstrap_indices(
      n_rows = n_rows,
      block_length = block_length
    )
    draw_data <- estimation_set[draw_indices, , drop = FALSE]
    draw_fit <- suppressWarnings(try(
      fit_target_model(
        estimation_set = draw_data,
        target_name = target_name,
        regressor_names = regressor_names,
        formula = formula,
        target_lags = target_lags
      ),
      silent = TRUE
    ))

    if (inherits(draw_fit, "try-error")) {
      next
    }

    draw_forecast <- suppressWarnings(try(
      predictive_target_model_draw(
        model = draw_fit,
        forecast_set = forecast_set,
        target_name = target_name,
        regressor_names = regressor_names
      ),
      silent = TRUE
    ))

    if (
      inherits(draw_forecast, "try-error") ||
        any(!is.finite(draw_forecast))
    ) {
      next
    }

    models[[draw_index]] <- draw_fit
    coefficient_draws[draw_index, ] <- extract_model_coefficients(
      model = draw_fit,
      coefficient_names = coefficient_names
    )
    forecast_draws[draw_index, ] <- draw_forecast
    valid[[draw_index]] <- TRUE
  }

  if (!any(valid)) {
    rlang::warn(
      paste(
        "Conditional block bootstrap failed for every resample.",
        "Point estimates will still be returned without uncertainty output."
      ),
      call = call
    )

    return(list(
      enabled = FALSE,
      type = bootstrap$type,
      N = requested_n,
      valid_N = 0L,
      block_length = block_length,
      conditional = TRUE,
      coefficient_draws = NULL,
      coefficient_se = NULL,
      forecast_draws = NULL,
      models = NULL
    ))
  }

  if (sum(valid) < requested_n) {
    rlang::warn(
      paste0(
        "Conditional block bootstrap produced ",
        sum(valid),
        " valid draws out of ",
        requested_n,
        "."
      ),
      call = call
    )
  }

  coefficient_draws <- coefficient_draws[valid, , drop = FALSE]
  forecast_draws <- forecast_draws[valid, , drop = FALSE]
  models <- models[valid]

  list(
    enabled = TRUE,
    type = bootstrap$type,
    N = requested_n,
    valid_N = length(models),
    block_length = block_length,
    conditional = TRUE,
    coefficient_draws = coefficient_draws,
    coefficient_se = apply(coefficient_draws, 2, stats::sd, na.rm = TRUE),
    forecast_draws = forecast_draws,
    models = models
  )
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
observations_per_target_period <- function(
  indicator_meta,
  target_meta,
  frequency_conversions,
  call = rlang::caller_env()
) {
  levels <- frequency_levels()
  indicator_unit <- indicator_meta$unit[[1]]
  target_unit <- target_meta$unit[[1]]

  indicator_index <- match(indicator_unit, levels)
  target_index <- match(target_unit, levels)

  if (indicator_index > target_index) {
    return(0)
  }

  ratio <- 1
  if (indicator_index < target_index) {
    edges <- frequency_edges()[indicator_index:(target_index - 1)]
    ratio <- prod(frequency_conversions[edges])
  }

  ratio <- ratio * target_meta$step[[1]] / indicator_meta$step[[1]]

  # Only integer ratios can be mapped cleanly into target-period blocks.
  if (!isTRUE(all.equal(ratio, round(ratio)))) {
    rlang::abort(
      paste0(
        "The inferred frequencies `", indicator_unit, "` and `", target_unit,
        "` are not aligned under the supplied `frequency_conversions`."
      ),
      call = call
    )
  }

  as.integer(round(ratio))
}

#' @keywords internal
#' @noRd
target_future_times <- function(last_time, target_meta, h) {
  shift_time_vec(
    time = last_time,
    n = seq_len(h) * target_meta$step[[1]],
    unit = target_meta$unit[[1]]
  )
}

#' @keywords internal
#' @noRd
compute_target_periods <- function(times, target_anchor, target_meta) {
  # Map each timestamp into the target period it belongs to.
  period_index <- floor(
    unit_distance(times, target_anchor, target_meta$unit[[1]]) /
      target_meta$step[[1]]
  )

  shift_time_vec(
    time = target_anchor,
    n = period_index * target_meta$step[[1]],
    unit = target_meta$unit[[1]]
  )
}

#' @keywords internal
#' @noRd
unit_distance <- function(times, origin, unit) {
  if (unit %in% c("second", "minute", "hour")) {
    scale <- c("second" = 1, "minute" = 60, "hour" = 3600)[[unit]]
    return(as.numeric(difftime(times, origin, units = "secs")) / scale)
  }
  if (unit == "day") {
    return(as.numeric(difftime(times, origin, units = "days")))
  }
  if (unit == "week") {
    return(as.numeric(difftime(times, origin, units = "days")) / 7)
  }

  origin_month <- lubridate::year(origin) * 12L + lubridate::month(origin)
  time_month <- lubridate::year(times) * 12L + lubridate::month(times)
  month_diff <- time_month - origin_month

  if (unit == "month") {
    return(month_diff)
  }
  if (unit == "quarter") {
    return(month_diff / 3)
  }
  if (unit == "year") {
    return(month_diff / 12)
  }

  rlang::abort(
    paste0("Unsupported unit `", unit, "`."),
    call = rlang::caller_env()
  )
}

#' @keywords internal
#' @noRd
shift_time <- function(time, n, unit) {
  if (unit == "second") {
    return(time + lubridate::seconds(n))
  }
  if (unit == "minute") {
    return(time + lubridate::minutes(n))
  }
  if (unit == "hour") {
    return(time + lubridate::hours(n))
  }
  if (unit == "day") {
    return(time + lubridate::days(n))
  }
  if (unit == "week") {
    return(time + lubridate::weeks(n))
  }
  if (unit == "month") {
    return(time %m+% lubridate::period(num = n, units = "month"))
  }
  if (unit == "quarter") {
    return(time %m+% lubridate::period(num = 3 * n, units = "month"))
  }
  if (unit == "year") {
    return(time %m+% lubridate::period(num = n, units = "year"))
  }

  rlang::abort(
    paste0("Unsupported unit `", unit, "`."),
    call = rlang::caller_env()
  )
}

#' @keywords internal
#' @noRd
shift_time_vec <- function(time, n, unit) {
  shifted <- lapply(
    as.list(n),
    function(step_count) shift_time(time = time, n = step_count, unit = unit)
  )
  do.call(c, shifted)
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

  counts <- indicator_tbl |>
    dplyr::mutate(period = periods) |>
    dplyr::count(.data$period, name = "n_obs")

  insufficient_periods <- counts |>
    dplyr::filter(.data$n_obs < obs_per_target)
  if (nrow(insufficient_periods) > 0) {
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

  truncated_periods <- counts |>
    dplyr::filter(.data$n_obs > obs_per_target)

  # Keep the most recent observations when a period is overfilled.
  grouped <- indicator_tbl |>
    dplyr::mutate(period = periods) |>
    dplyr::group_by(.data$period) |>
    dplyr::arrange(.data$time, .by_group = TRUE) |>
    dplyr::summarise(
      values = list(utils::tail(.data$values, obs_per_target)),
      .groups = "drop"
    )

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
      n_periods = nrow(truncated_periods)
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
aggregate_period_values <- function(
  values,
  aggregator,
  indicator_id,
  call = rlang::caller_env()
) {
  if (is.character(aggregator)) {
    if (identical(aggregator, "mean")) {
      return(mean(values))
    }
    if (identical(aggregator, "last")) {
      return(utils::tail(values, 1))
    }
    if (identical(aggregator, "sum")) {
      return(sum(values))
    }
  }

  if (!is.numeric(aggregator)) {
    rlang::abort(
      paste0(
        "Unsupported aggregation method for indicator `", indicator_id, "`."
      ),
      call = call
    )
  }

  if (length(aggregator) != length(values)) {
    rlang::abort(
      paste0(
        "Numeric weights for indicator `", indicator_id,
        "` must have length ", length(values), "."
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

  sum(values * aggregator)
}

#' @keywords internal
#' @noRd
aggregate_indicator_blocks <- function(
  blocks,
  aggregator,
  indicator_id,
  call = rlang::caller_env()
) {
  apply(
    blocks,
    1,
    aggregate_period_values,
    aggregator = aggregator,
    indicator_id = indicator_id,
    call = call
  )
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
build_bridge_estimation_set <- function(
  target_tbl,
  target_name,
  feature_long,
  indic_lags,
  estimation_times
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
    stats::na.omit()
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

  y <- estimation_set[[target_name]]
  xreg <- as.matrix(estimation_set[, regressor_names, drop = FALSE])

  if (target_lags == 0) {
    # With no AR terms, RSS is enough to compare expalmon candidates.
    fit <- stats::lm.fit(
      x = cbind("(Intercept)" = 1, xreg),
      y = y
    )
    rss <- sum(stats::residuals(fit)^2)
    if (!is.finite(rss)) {
      return(Inf)
    }
    return(rss)
  }

  fit <- suppressWarnings(try(
    forecast::Arima(
      y = y,
      order = c(target_lags, 0, 0),
      xreg = xreg,
      method = "CSS"
    ),
    silent = TRUE
  ))

  if (inherits(fit, "try-error")) {
    return(Inf)
  }

  loglik <- suppressWarnings(try(as.numeric(stats::logLik(fit)), silent = TRUE))
  if (inherits(loglik, "try-error") || !is.finite(loglik)) {
    rss <- sum(stats::residuals(fit)^2, na.rm = TRUE)
    if (!is.finite(rss)) {
      return(Inf)
    }
    return(rss)
  }

  -loglik
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
  lower <- unlist(
    lapply(
      specs,
      function(spec) {
        if (identical(spec$aggregator, "beta")) {
          return(rep(-10, parametric_parameter_count(spec$aggregator)))
        }
        rep(-10, parametric_parameter_count(spec$aggregator))
      }
    ),
    use.names = FALSE
  )
  upper <- unlist(
    lapply(
      specs,
      function(spec) {
        if (identical(spec$aggregator, "beta")) {
          return(rep(10, parametric_parameter_count(spec$aggregator)))
        }
        rep(10, parametric_parameter_count(spec$aggregator))
      }
    ),
    use.names = FALSE
  )

  list(lower = lower, upper = upper)
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
        iter.max = solver_options$maxiter
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

#' @srrstats {RE3.0} Non-converged parametric-weight optimizations trigger a warning before the best available result is retained.
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

  results <- bridgr_with_seed(solver_options$seed, {
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
            sd = 0.5
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
  })

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

  if (!isTRUE(best_result$convergence == 0)) {
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

#' @keywords internal
#' @noRd
as_forecast_xreg <- function(
  xreg,
  regressor_names,
  call = rlang::caller_env()
) {
  if (length(regressor_names) == 0) {
    return(NULL)
  }

  xreg_tbl <- as_bridge_tbl(
    x = xreg,
    arg = "xreg",
    default_id = regressor_names[[1]],
    call = call
  )
  xreg_wide <- suppressMessages(tsbox::ts_wide(xreg_tbl))

  missing_columns <- setdiff(regressor_names, colnames(xreg_wide))
  if (length(missing_columns) > 0) {
    rlang::abort(
      paste0(
        "`xreg` is missing required regressors: ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      call = call
    )
  }

  xreg_wide[, c("time", regressor_names), drop = FALSE]
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
normalize_bridge_bootstrap <- function(
  bootstrap,
  call = rlang::caller_env()
) {
  defaults <- list(
    N = 100L,
    block_length = NULL
  )

  if (is.null(bootstrap)) {
    return(defaults)
  }
  if (!is.list(bootstrap)) {
    rlang::abort("`bootstrap` must be a list.", call = call)
  }

  invalid <- setdiff(names(bootstrap), names(defaults))
  if (length(invalid) > 0) {
    rlang::abort(
      paste0(
        "Invalid `bootstrap` entries: ",
        paste(invalid, collapse = ", "),
        "."
      ),
      call = call
    )
  }

  defaults[names(bootstrap)] <- bootstrap

  if (!is.numeric(defaults$N) ||
    length(defaults$N) != 1 ||
    !is.finite(defaults$N) ||
    defaults$N < 1) {
    rlang::abort(
      "`bootstrap$N` must be a single integer >= 1.",
      call = call
    )
  }
  defaults$N <- as.integer(round(defaults$N))

  if (!is.null(defaults$block_length)) {
    if (!is.numeric(defaults$block_length) ||
      length(defaults$block_length) != 1 ||
      !is.finite(defaults$block_length) ||
      defaults$block_length < 1) {
      rlang::abort(
        "`bootstrap$block_length` must be `NULL` or a single integer >= 1.",
        call = call
      )
    }
    defaults$block_length <- as.integer(round(defaults$block_length))
  }

  defaults
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
fit_target_model <- function(
  estimation_set,
  target_name,
  regressor_names,
  formula,
  target_lags
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

#' @keywords internal
#' @noRd
hac_lag_order <- function(n_obs) {
  if (n_obs <= 1) {
    return(0L)
  }

  as.integer(min(n_obs - 1L, floor(4 * (n_obs / 100)^(2 / 9))))
}

#' @keywords internal
#' @noRd
hac_long_run_covariance <- function(scores, lag = NULL) {
  scores <- as.matrix(scores)
  n_obs <- nrow(scores)
  if (n_obs == 0) {
    return(matrix(NA_real_, nrow = ncol(scores), ncol = ncol(scores)))
  }

  lag <- lag %||% hac_lag_order(n_obs)
  lag <- max(0L, min(as.integer(lag), n_obs - 1L))
  covariance <- crossprod(scores) / n_obs

  if (lag == 0L) {
    return(covariance)
  }

  for (lag_index in seq_len(lag)) {
    weight <- 1 - lag_index / (lag + 1)
    gamma <- crossprod(
      scores[(lag_index + 1):n_obs, , drop = FALSE],
      scores[seq_len(n_obs - lag_index), , drop = FALSE]
    ) / n_obs
    covariance <- covariance + weight * (gamma + t(gamma))
  }

  covariance
}

#' @keywords internal
#' @noRd
vcov_from_jacobian <- function(jacobian, residuals, lag = NULL) {
  jacobian <- as.matrix(jacobian)
  n_obs <- nrow(jacobian)
  if (n_obs < 2 || ncol(jacobian) == 0) {
    return(matrix(NA_real_, nrow = ncol(jacobian), ncol = ncol(jacobian)))
  }

  bread <- crossprod(jacobian) / n_obs
  bread_inv <- tryCatch(
    solve(bread),
    error = function(...) {
      tryCatch(
        qr.solve(bread),
        error = function(...) {
          decomposition <- svd(bread)
          positive <- decomposition$d >
            max(decomposition$d) * .Machine$double.eps
          if (!any(positive)) {
            return(matrix(NA_real_, nrow = ncol(bread), ncol = ncol(bread)))
          }
          decomposition$v[, positive, drop = FALSE] %*%
            diag(1 / decomposition$d[positive], nrow = sum(positive)) %*%
            t(decomposition$u[, positive, drop = FALSE])
        }
      )
    }
  )
  scores <- residuals * jacobian
  meat <- hac_long_run_covariance(scores = scores, lag = lag)
  bread_inv %*% meat %*% bread_inv / n_obs
}

#' @keywords internal
#' @noRd
coefficient_vcov_hac <- function(model) {
  model_matrix <- stats::model.matrix(model)
  covariance <- vcov_from_jacobian(
    jacobian = model_matrix,
    residuals = stats::residuals(model)
  )
  dimnames(covariance) <- list(
    names(stats::coef(model)),
    names(stats::coef(model))
  )
  covariance
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
coefficient_vcov_delta_hac <- function(
  model,
  formula,
  target_tbl,
  target_name,
  fixed_aggregated,
  parametric_specs,
  parameter_blocks,
  indic_lags,
  target_lags,
  epsilon = 1e-6
) {
  coefficient_values <- stats::coef(model)
  baseline_set <- rebuild_parametric_estimation_set(
    target_tbl = target_tbl,
    target_name = target_name,
    fixed_aggregated = fixed_aggregated,
    parametric_specs = parametric_specs,
    parameter_blocks = parameter_blocks,
    indic_lags = indic_lags,
    target_lags = target_lags
  )
  baseline_matrix <- stats::model.matrix(
    object = stats::terms(model),
    data = baseline_set
  )
  theta_labels <- parametric_parameter_labels(parametric_specs)
  jacobian <- matrix(
    NA_real_,
    nrow = nrow(baseline_matrix),
    ncol = length(coefficient_values) + length(theta_labels),
    dimnames = list(NULL, c(names(coefficient_values), theta_labels))
  )
  jacobian[, names(coefficient_values)] <- baseline_matrix

  theta_vector <- flatten_parameter_blocks(
    parameter_blocks = parameter_blocks,
    specs = parametric_specs
  )

  for (theta_index in seq_along(theta_vector)) {
    plus_theta <- theta_vector
    minus_theta <- theta_vector
    plus_theta[[theta_index]] <- plus_theta[[theta_index]] + epsilon
    minus_theta[[theta_index]] <- minus_theta[[theta_index]] - epsilon

    plus_blocks <- split_parameter_vector(
      parameters = plus_theta,
      specs = parametric_specs
    )
    minus_blocks <- split_parameter_vector(
      parameters = minus_theta,
      specs = parametric_specs
    )
    names(plus_blocks) <- names(parametric_specs)
    names(minus_blocks) <- names(parametric_specs)

    mean_plus <- bridge_mean_from_parameters(
      coefficient_values = coefficient_values,
      formula = formula,
      estimation_set = rebuild_parametric_estimation_set(
        target_tbl = target_tbl,
        target_name = target_name,
        fixed_aggregated = fixed_aggregated,
        parametric_specs = parametric_specs,
        parameter_blocks = plus_blocks,
        indic_lags = indic_lags,
        target_lags = target_lags
      )
    )
    mean_minus <- bridge_mean_from_parameters(
      coefficient_values = coefficient_values,
      formula = formula,
      estimation_set = rebuild_parametric_estimation_set(
        target_tbl = target_tbl,
        target_name = target_name,
        fixed_aggregated = fixed_aggregated,
        parametric_specs = parametric_specs,
        parameter_blocks = minus_blocks,
        indic_lags = indic_lags,
        target_lags = target_lags
      )
    )
    jacobian[, length(coefficient_values) + theta_index] <-
      (mean_plus - mean_minus) / (2 * epsilon)
  }

  covariance <- vcov_from_jacobian(
    jacobian = jacobian,
    residuals = stats::residuals(model)
  )
  dimnames(covariance) <- list(colnames(jacobian), colnames(jacobian))
  covariance
}

#' @keywords internal
#' @noRd
compute_bridge_coefficient_uncertainty <- function(
  model,
  formula,
  target_tbl,
  target_name,
  fixed_aggregated,
  parametric_specs,
  parameter_blocks,
  indic_lags,
  target_lags
) {
  coefficient_names <- names(stats::coef(model))
  if (length(parametric_specs) == 0) {
    covariance <- coefficient_vcov_hac(model)
    return(list(
      method = "hac",
      covariance = covariance,
      se = sqrt(diag(covariance))[coefficient_names]
    ))
  }

  full_covariance <- coefficient_vcov_delta_hac(
    model = model,
    formula = formula,
    target_tbl = target_tbl,
    target_name = target_name,
    fixed_aggregated = fixed_aggregated,
    parametric_specs = parametric_specs,
    parameter_blocks = parameter_blocks,
    indic_lags = indic_lags,
    target_lags = target_lags
  )
  covariance <- full_covariance[
    coefficient_names,
    coefficient_names,
    drop = FALSE
  ]

  list(
    method = "delta_hac",
    covariance = covariance,
    se = sqrt(diag(covariance))[coefficient_names],
    full_covariance = full_covariance
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
build_forecast_target_times <- function(
  last_target_time,
  target_meta,
  h
) {
  target_future_times(
    last_time = last_target_time,
    target_meta = target_meta,
    h = h
  )
}

#' @keywords internal
#' @noRd
within_target_period_times <- function(period_start, indicator_meta, n_obs) {
  shift_time_vec(
    time = period_start,
    n = (seq_len(n_obs) - 1) * indicator_meta$step[[1]],
    unit = indicator_meta$unit[[1]]
  )
}

#' @keywords internal
#' @noRd
as_indicator_period_long <- function(
  indicator_id,
  target_times,
  blocks,
  indicator_meta
) {
  rows <- lapply(
    seq_len(nrow(blocks)),
    function(period_index) {
      dplyr::tibble(
        id = indicator_id,
        time = within_target_period_times(
          period_start = target_times[[period_index]],
          indicator_meta = indicator_meta,
          n_obs = ncol(blocks)
        ),
        values = as.numeric(blocks[period_index, ])
      )
    }
  )

  dplyr::bind_rows(rows)
}

#' @keywords internal
#' @noRd
prepare_future_indicator_blocks <- function(
  indicator_tbl,
  target_meta,
  target_anchor,
  future_target_times
) {
  if (length(future_target_times) == 0) {
    return(list(
      periods = future_target_times,
      values = list()
    ))
  }

  periods <- compute_target_periods(
    indicator_tbl$time,
    target_anchor = target_anchor,
    target_meta = target_meta
  )
  future_tbl <- indicator_tbl |>
    dplyr::mutate(period = periods) |>
    dplyr::filter(.data$period %in% future_target_times) |>
    dplyr::group_by(.data$period) |>
    dplyr::arrange(.data$time, .by_group = TRUE) |>
    dplyr::summarise(values = list(.data$values), .groups = "drop")

  list(
    periods = future_tbl$period,
    values = future_tbl$values
  )
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
  sampled_indices <- circular_block_bootstrap_indices(
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
