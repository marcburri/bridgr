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
align_mf_inputs <- function(
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
