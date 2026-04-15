make_monthly_indicator <- function(n = 24, start = "2020-01-01", offset = 0) {
  dplyr::tibble(
    time = seq(as.Date(start), by = "month", length.out = n),
    value = 10 + seq_len(n) + rep(c(0, 2, -1, 3), length.out = n) + offset
  )
}

make_quarter_target <- function(
  monthly_indicator,
  n_quarters = 6,
  intercept = 1,
  slope = 1
) {
  values <- monthly_indicator$value[seq_len(n_quarters * 3)]
  quarter_means <- vapply(
    seq_len(n_quarters),
    function(i) {
      idx <- ((i - 1) * 3 + 1):(i * 3)
      mean(values[idx])
    },
    FUN.VALUE = numeric(1)
  )

  dplyr::tibble(
    time = monthly_indicator$time[seq(1, n_quarters * 3, by = 3)],
    value = intercept + slope * quarter_means +
      rep(c(0.5, -0.25, 0.75, -0.5), length.out = n_quarters)
  )
}

make_multi_indicator <- function(n = 24, start = "2020-01-01") {
  dates <- seq(as.Date(start), by = "month", length.out = n)
  dplyr::tibble(
    id = rep(c("a", "b"), each = n),
    time = rep(dates, times = 2),
    value = c(
      10 + seq_len(n) + rep(c(0, 2, -1, 3), length.out = n),
      5 + seq_len(n) * 0.7 + rep(c(1, -2, 2, -1, 0), length.out = n)
    )
  )
}

make_daily_indicator <- function(n = 84, start = "2020-01-01") {
  dplyr::tibble(
    time = seq(as.Date(start), by = "day", length.out = n),
    value = 50 + seq_len(n) * 0.5 + rep(c(0, 1, -1, 2, -2, 1, 0), length.out = n)
  )
}

make_weekly_target <- function(daily_indicator, n_weeks = 8, intercept = 0, slope = 1) {
  values <- daily_indicator$value[seq_len(n_weeks * 7)]
  week_means <- vapply(
    seq_len(n_weeks),
    function(i) {
      idx <- ((i - 1) * 7 + 1):(i * 7)
      mean(values[idx])
    },
    FUN.VALUE = numeric(1)
  )

  dplyr::tibble(
    time = daily_indicator$time[seq(1, n_weeks * 7, by = 7)],
    value = intercept + slope * week_means + rep(c(0.25, -0.5, 0.75, -0.25), length.out = n_weeks)
  )
}

make_exact_multifrequency_simulation <- function(
  n_target = 10,
  h = 1,
  start = "2020-01-01 00:00:00",
  coefficients = c(
    intercept = 1.25,
    second = 0.8,
    minute = -1.1,
    hour = 0.6
  )
) {
  start_time <- as.POSIXct(start, tz = "UTC")
  n_hours_total <- n_target + h

  second_times <- seq(start_time, by = "30 sec", length.out = n_hours_total * 120)
  minute_times <- seq(start_time, by = "10 min", length.out = n_hours_total * 6)
  hour_times <- seq(start_time, by = "hour", length.out = n_hours_total)

  second_values <- sin(seq_along(second_times) / 240) + seq_along(second_times) / 20000
  minute_values <- cos(seq_along(minute_times) / 4) + seq_along(minute_times) / 500
  hour_values <- c(
    0.5, -1.2, 0.8, 1.4, -0.6, 0.1, 1.1, -0.9, 0.4, 1.6, -0.3
  )[seq_len(n_hours_total)]

  second_hourly <- vapply(
    seq_len(n_hours_total),
    function(i) {
      idx <- ((i - 1) * 120 + 1):(i * 120)
      mean(second_values[idx])
    },
    FUN.VALUE = numeric(1)
  )
  minute_hourly <- vapply(
    seq_len(n_hours_total),
    function(i) {
      idx <- ((i - 1) * 6 + 1):(i * 6)
      mean(minute_values[idx])
    },
    FUN.VALUE = numeric(1)
  )

  target_values <- coefficients[["intercept"]] +
    coefficients[["second"]] * second_hourly[seq_len(n_target)] +
    coefficients[["minute"]] * minute_hourly[seq_len(n_target)] +
    coefficients[["hour"]] * hour_values[seq_len(n_target)] +
    rep(c(0.01, -0.015, 0.02, -0.005, 0.012), length.out = n_target)

  list(
    target = dplyr::tibble(
      time = hour_times[seq_len(n_target)],
      value = target_values
    ),
    indic = dplyr::tibble(
      id = c(
        rep("second", length(second_times)),
        rep("minute", length(minute_times)),
        rep("hour", length(hour_times))
      ),
      time = c(second_times, minute_times, hour_times),
      value = c(second_values, minute_values, hour_values)
    ),
    coefficients = coefficients
  )
}

aggregate_latest_mean_by_period <- function(values, times, periods, n_keep) {
  period_levels <- unique(periods)
  vapply(
    period_levels,
    function(period) {
      period_values <- values[periods == period]
      mean(utils::tail(period_values, n_keep))
    },
    FUN.VALUE = numeric(1)
  )
}

make_day_week_month_simulation <- function(
  n_months = 8,
  h = 1,
  start = "2020-01-01",
  coefficients = c(
    intercept = 0.75,
    day = 1.2,
    week = -0.9,
    month = 0.5
  )
) {
  start_date <- as.Date(start)
  month_starts <- seq(start_date, by = "month", length.out = n_months + h)
  end_date <- seq(start_date, by = "month", length.out = n_months + h + 1)[n_months + h + 1] - 1

  day_times <- seq(start_date, end_date, by = "day")
  week_times <- seq(start_date, end_date, by = "week")
  month_times <- month_starts

  day_values <- 5 + seq_along(day_times) / 40 + sin(seq_along(day_times) / 3)
  week_values <- -2 + seq_along(week_times) / 10 + cos(seq_along(week_times) / 2)
  month_values <- c(0.5, -1.0, 1.2, 0.3, -0.4, 0.9, -0.7, 1.4, 0.2)[seq_len(n_months + h)]

  month_periods_day <- as.Date(lubridate::floor_date(day_times, unit = "month"))
  month_periods_week <- as.Date(lubridate::floor_date(week_times, unit = "month"))

  day_monthly <- aggregate_latest_mean_by_period(
    values = day_values,
    times = day_times,
    periods = month_periods_day,
    n_keep = 28
  )
  week_monthly <- aggregate_latest_mean_by_period(
    values = week_values,
    times = week_times,
    periods = month_periods_week,
    n_keep = 4
  )

  target_values <- coefficients[["intercept"]] +
    coefficients[["day"]] * day_monthly[seq_len(n_months)] +
    coefficients[["week"]] * week_monthly[seq_len(n_months)] +
    coefficients[["month"]] * month_values[seq_len(n_months)] +
    rep(c(0.015, -0.01, 0.02, -0.005), length.out = n_months)

  list(
    target = dplyr::tibble(
      time = month_times[seq_len(n_months)],
      value = target_values
    ),
    indic = dplyr::tibble(
      id = c(
        rep("day", length(day_times)),
        rep("week", length(week_times)),
        rep("month", length(month_times))
      ),
      time = c(day_times, week_times, month_times),
      value = c(day_values, week_values, month_values)
    ),
    coefficients = coefficients
  )
}

make_month_quarter_year_simulation <- function(
  n_years = 8,
  h = 1,
  start = "2012-01-01",
  coefficients = c(
    intercept = -0.5,
    month = 0.9,
    quarter = -1.3,
    year = 0.7
  )
) {
  start_date <- as.Date(start)
  month_times <- seq(start_date, by = "month", length.out = (n_years + h) * 12)
  quarter_times <- seq(start_date, by = "quarter", length.out = (n_years + h) * 4)
  year_times <- seq(start_date, by = "year", length.out = n_years + h)

  month_values <- 3 + seq_along(month_times) / 30 + sin(seq_along(month_times) / 2)
  quarter_values <- 1 + seq_along(quarter_times) / 8 + cos(seq_along(quarter_times) / 3)
  year_values <- c(1.4, -0.8, 0.6, 1.1, -0.5, 0.9, 1.7, -0.2, 0.3)[seq_len(n_years + h)]

  month_periods <- as.Date(lubridate::floor_date(month_times, unit = "year"))
  quarter_periods <- as.Date(lubridate::floor_date(quarter_times, unit = "year"))

  month_yearly <- aggregate_latest_mean_by_period(
    values = month_values,
    times = month_times,
    periods = month_periods,
    n_keep = 12
  )
  quarter_yearly <- aggregate_latest_mean_by_period(
    values = quarter_values,
    times = quarter_times,
    periods = quarter_periods,
    n_keep = 4
  )

  target_values <- coefficients[["intercept"]] +
    coefficients[["month"]] * month_yearly[seq_len(n_years)] +
    coefficients[["quarter"]] * quarter_yearly[seq_len(n_years)] +
    coefficients[["year"]] * year_values[seq_len(n_years)] +
    rep(c(0.02, -0.015, 0.01), length.out = n_years)

  list(
    target = dplyr::tibble(
      time = year_times[seq_len(n_years)],
      value = target_values
    ),
    indic = dplyr::tibble(
      id = c(
        rep("month", length(month_times)),
        rep("quarter", length(quarter_times)),
        rep("year", length(year_times))
      ),
      time = c(month_times, quarter_times, year_times),
      value = c(month_values, quarter_values, year_values)
    ),
    coefficients = coefficients
  )
}

make_seeded_ar1_indicator <- function(
  n = 240,
  phi = 0.7,
  start = "2000-01-01",
  seed = 123
) {
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit(
    {
      if (exists("old_seed", inherits = FALSE)) {
        if (is.null(old_seed)) {
          rm(".Random.seed", envir = .GlobalEnv)
        } else {
          assign(".Random.seed", old_seed, envir = .GlobalEnv) # nolint: object_name_linter.
        }
      }
    },
    add = TRUE
  )
  set.seed(seed)

  values <- as.numeric(stats::arima.sim(model = list(ar = phi), n = n))

  dplyr::tibble(
    time = seq(as.Date(start), by = "month", length.out = n),
    value = values
  )
}

make_daily_week_fixture <- function(
  n_weeks = 12,
  h = 1,
  start = "2020-01-06",
  indicator_values = NULL
) {
  total_days <- (n_weeks + h) * 7
  if (is.null(indicator_values)) {
    indicator_values <- 100 + seq_len(total_days) +
      rep(c(0, 2, -1, 3, -2, 1, 0), length.out = total_days)
  }

  indic <- dplyr::tibble(
    time = seq(as.Date(start), by = "day", length.out = total_days - 7 * h),
    value = indicator_values[seq_len(total_days - 7 * h)]
  )

  week_values <- vapply(
    seq_len(n_weeks),
    function(i) {
      idx <- ((i - 1) * 7 + 1):(i * 7)
      mean(indicator_values[idx]) + c(0.2, -0.1, 0.05, -0.15)[((i - 1) %% 4) + 1]
    },
    FUN.VALUE = numeric(1)
  )

  target <- dplyr::tibble(
    time = seq(as.Date(start), by = "week", length.out = n_weeks),
    value = week_values
  )

  list(
    target = target,
    indic = indic,
    full_indicator_values = indicator_values,
    observed_indicator_values = indicator_values[seq_len(total_days - 7 * h)]
  )
}

make_method_comparison_indicator <- function(n = 140, start = "2020-01-01") {
  index <- seq_len(n)
  dplyr::tibble(
    time = seq(as.Date(start), by = "day", length.out = n),
    value = 20 + 0.15 * index + sin(index / 5) + cos(index / 11)
  )
}

make_expalmon_joint_fixture <- function(
  n_periods = 24,
  h = 1,
  start = "2020-01-06",
  include_mean_indicator = FALSE
) {
  total_periods <- n_periods + h
  total_days <- total_periods * 7
  day_index <- rep(seq_len(7), times = total_periods)
  period_index <- rep(seq_len(total_periods), each = 7)
  times <- seq(as.Date(start), by = "day", length.out = total_days)
  period_seq <- seq_len(total_periods)

  x1_a <- 0.8 + sin(period_seq / 2.3)
  x1_b <- -0.4 + cos(period_seq / 3.1)
  x2_a <- -0.6 + cos(period_seq / 1.9)
  x2_b <- 0.5 + sin(period_seq / 2.7)

  basis_1a <- c(-3, -1, 0, 2, 4, 1, -2)
  basis_1b <- c(1, -2, 3, -1, 2, -3, 0)
  basis_2a <- c(2, -3, 1, -2, 3, -1, 0)
  basis_2b <- c(-2, 1, -3, 2, 0, 3, -1)

  x1_full <- 0.45 * period_index +
    x1_a[period_index] * basis_1a[day_index] +
    x1_b[period_index] * basis_1b[day_index]

  x2_full <- -0.3 * period_index +
    x2_a[period_index] * basis_2a[day_index] +
    x2_b[period_index] * basis_2b[day_index]

  true_weights <- list(
    x1 = bridgr:::exp_almon(c(1.1, -0.9), 7),
    x2 = bridgr:::exp_almon(c(-0.8, 0.7), 7)
  )

  agg_x1 <- vapply(
    seq_len(total_periods),
    function(i) {
      idx <- ((i - 1) * 7 + 1):(i * 7)
      sum(true_weights$x1 * x1_full[idx])
    },
    FUN.VALUE = numeric(1)
  )
  agg_x2 <- vapply(
    seq_len(total_periods),
    function(i) {
      idx <- ((i - 1) * 7 + 1):(i * 7)
      sum(true_weights$x2 * x2_full[idx])
    },
    FUN.VALUE = numeric(1)
  )

  indic <- dplyr::tibble(
    id = c(rep("x1", n_periods * 7), rep("x2", n_periods * 7)),
    time = rep(times[seq_len(n_periods * 7)], times = 2),
    value = c(x1_full[seq_len(n_periods * 7)], x2_full[seq_len(n_periods * 7)])
  )

  coefficients <- c(intercept = 1.5, x1 = 1.1, x2 = -1.1)
  target_value <- coefficients[["intercept"]] +
    coefficients[["x1"]] * agg_x1[seq_len(n_periods)] +
    coefficients[["x2"]] * agg_x2[seq_len(n_periods)]

  if (isTRUE(include_mean_indicator)) {
    x3_full <- 0.2 * period_index +
      c(-1, 0, 1, 2, 1, 0, -1)[day_index] +
      sin(period_index / 3.5)
    agg_x3 <- vapply(
      seq_len(total_periods),
      function(i) {
        idx <- ((i - 1) * 7 + 1):(i * 7)
        mean(x3_full[idx])
      },
      FUN.VALUE = numeric(1)
    )
    coefficients <- c(coefficients, x3 = 0.6)
    target_value <- target_value + coefficients[["x3"]] * agg_x3[seq_len(n_periods)]
    indic <- rbind(
      indic,
      dplyr::tibble(
        id = rep("x3", n_periods * 7),
        time = times[seq_len(n_periods * 7)],
        value = x3_full[seq_len(n_periods * 7)]
      )
    )
  }

  list(
    target = dplyr::tibble(
      time = seq(as.Date(start), by = "week", length.out = n_periods),
      value = target_value
    ),
    indic = indic,
    true_weights = true_weights,
    coefficients = coefficients
  )
}

make_expalmon_single_fixture <- function(
  n_periods = 12,
  h = 1,
  start = "2020-01-06"
) {
  total_periods <- n_periods + h
  total_days <- total_periods * 7
  day_index <- rep(seq_len(7), times = total_periods)
  period_index <- rep(seq_len(total_periods), each = 7)
  times <- seq(as.Date(start), by = "day", length.out = total_days)

  coef_a <- 1 + sin(seq_len(total_periods) / 2.5)
  coef_b <- -0.5 + cos(seq_len(total_periods) / 3.4)
  basis_a <- c(-3, -1, 0, 2, 4, 1, -2)
  basis_b <- c(2, -2, 3, -1, 1, -3, 0)

  indicator_full <- 0.35 * period_index +
    coef_a[period_index] * basis_a[day_index] +
    coef_b[period_index] * basis_b[day_index]

  true_weights <- bridgr:::exp_almon(c(-0.7, 0.9), 7)
  aggregated <- vapply(
    seq_len(total_periods),
    function(i) {
      idx <- ((i - 1) * 7 + 1):(i * 7)
      sum(true_weights * indicator_full[idx])
    },
    FUN.VALUE = numeric(1)
  )

  list(
    target = dplyr::tibble(
      time = seq(as.Date(start), by = "week", length.out = n_periods),
      value = 2 + 1.2 * aggregated[seq_len(n_periods)]
    ),
    indic = dplyr::tibble(
      time = times[seq_len(n_periods * 7)],
      value = indicator_full[seq_len(n_periods * 7)]
    ),
    true_weights = true_weights
  )
}
