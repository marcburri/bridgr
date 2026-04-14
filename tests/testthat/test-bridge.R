test_that("bridge keeps indicators that already extend beyond the forecast horizon", {
  indic <- make_monthly_indicator()
  target <- make_quarter_target(indic, n_quarters = 6)

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "last",
    h = 1
  )

  expect_match(format(model$formula), "indic")
  expect_true("indic" %in% model$regressor_names)
  expect_equal(nrow(model$forecast_set), 1)
})

test_that("bridge supports multiple indicators with per-series aggregation choices", {
  indic <- make_multi_indicator()
  target <- make_quarter_target(
    monthly_indicator = data.frame(
      time = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
      value = 10 + seq_len(24) + rep(c(0, 2, -1, 3), length.out = 24)
    ),
    n_quarters = 6
  )

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = c("last", "last"),
    indic_aggregators = c("last", "mean"),
    h = 1
  )

  expect_equal(sort(model$indic_name), c("a", "b"))
  expect_equal(sort(model$regressor_names), c("a", "b"))
})

test_that("extra high-frequency observations trigger a summarized warning", {
  indic <- make_daily_indicator()
  target <- make_weekly_target(indic, n_weeks = 8)

  expect_warning(
    model <- bridge(
      target = target,
      indic = indic,
      indic_predict = "last",
      frequency_conversions = c(dpw = 5),
      h = 1
    ),
    "Using the most recent observations"
  )

  expect_equal(model$frequency_conversions[["dpw"]], 5)
  expect_equal(model$frequency_conversions[["wpm"]], 4)
})

test_that("too few high-frequency observations within a target period fail", {
  indic <- make_daily_indicator()
  target <- make_weekly_target(indic, n_weeks = 8)

  expect_error(
    bridge(
      target = target,
      indic = indic,
      indic_predict = "last",
      frequency_conversions = c(dpw = 8),
      h = 1
    ),
    "fewer observations within at least one target period"
  )
})

test_that("invalid or lower-frequency indicators are rejected", {
  indic <- data.frame(
    time = seq(as.Date("2020-01-01"), by = "quarter", length.out = 4),
    value = c(1, 2, 3, 4)
  )
  target <- data.frame(
    time = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    value = seq_len(12)
  )

  expect_error(
    bridge(target = target, indic = indic),
    "lower-frequency than the target"
  )
})

test_that("forecast.bridge uses stored future regressors and accepts custom xreg", {
  indic <- make_monthly_indicator()
  target <- make_quarter_target(indic, n_quarters = 6)

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_lags = 1,
    h = 2
  )

  default_forecast <- forecast(model)
  expect_s3_class(default_forecast, "forecast")
  expect_equal(nrow(default_forecast$forecast_set), 2)

  custom_xreg <- data.frame(
    id = rep(c("indic", "indic_lag1"), each = 2),
    time = rep(model$forecast_set$time, times = 2),
    value = c(model$forecast_set$indic + 1, model$forecast_set$indic_lag1 + 1)
  )
  scenario_forecast <- forecast(model, xreg = custom_xreg)

  expect_s3_class(scenario_forecast, "forecast")
  expect_equal(nrow(scenario_forecast$forecast_set), 2)
})

test_that("summary.bridge prints model information", {
  indic <- make_monthly_indicator()
  target <- make_quarter_target(indic, n_quarters = 6)

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "last",
    h = 1
  )

  output <- capture.output(summary(model))
  expect_true(any(grepl("Bridge model summary", output)))
  expect_true(any(grepl("Target series:", output)))
})

test_that("bridge validates duplicate timestamps and missing values", {
  duplicate_target <- data.frame(
    time = as.Date(c("2020-01-01", "2020-01-01", "2020-04-01")),
    value = c(1, 2, 3)
  )
  indic <- make_monthly_indicator(n = 12)

  expect_error(
    bridge(target = duplicate_target, indic = indic),
    "duplicated values in time column|duplicate timestamps"
  )

  missing_indic <- indic
  missing_indic$value[[3]] <- NA_real_
  target <- make_quarter_target(indic, n_quarters = 4)

  expect_error(
    bridge(target = target, indic = missing_indic),
    "contains missing values"
  )
})

test_that("bridge preprocessing recovers known coefficients from a deterministic multi-frequency simulation", {
  simulated <- make_exact_multifrequency_simulation()

  target_tbl <- bridgr:::as_bridge_tbl(simulated$target, "target", "target") |>
    dplyr::mutate(id = "target")
  indic_tbl <- bridgr:::as_bridge_tbl(simulated$indic, "indic", "indic")

  config <- bridgr:::validate_bridge_inputs(
    target_tbl = target_tbl,
    indic_tbl = indic_tbl,
    indic_predict = c("last", "last", "last"),
    indic_aggregators = c("mean", "mean", "mean"),
    indic_lags = 0,
    target_lags = 0,
    h = 1,
    frequency_conversions = NULL
  )

  target_meta <- bridgr:::infer_frequency_table(target_tbl)$target
  indic_meta <- bridgr:::infer_frequency_table(indic_tbl)$indicators
  expect_equal(indic_meta$unit[match(c("second", "minute", "hour"), indic_meta$id)], c("second", "minute", "hour"))

  aligned <- bridgr:::align_bridge_inputs(target_tbl, indic_tbl, target_meta, indic_meta)
  future_target_times <- bridgr:::target_future_times(max(aligned$target$time), target_meta, 1)
  indicator_results <- bridgr:::build_indicator_features(
    indic_tbl = aligned$indic,
    indic_meta = indic_meta,
    target_tbl = aligned$target,
    target_meta = target_meta,
    target_anchor = aligned$target_anchor,
    future_target_times = future_target_times,
    indic_predict = c("last", "last", "last"),
    indic_aggregators = config$indic_aggregators,
    frequency_conversions = config$frequency_conversions
  )

  estimation_long <- dplyr::bind_rows(
    aligned$target |> dplyr::select(id, time, values),
    indicator_results$aggregated
  ) |>
    dplyr::filter(time %in% aligned$target$time)
  estimation_set <- suppressMessages(tsbox::ts_wide(estimation_long)) |>
    stats::na.omit()

  fit <- stats::lm(target ~ second + minute + hour, data = estimation_set)
  estimated <- stats::coef(fit)[c("(Intercept)", "second", "minute", "hour")]
  expect_equal(
    unname(estimated),
    unname(simulated$coefficients[c("intercept", "second", "minute", "hour")]),
    tolerance = 5e-2
  )
})

test_that("bridge preprocessing recovers known coefficients for day-week-month data", {
  simulated <- make_day_week_month_simulation()

  target_tbl <- bridgr:::as_bridge_tbl(simulated$target, "target", "target") |>
    dplyr::mutate(id = "target")
  indic_tbl <- bridgr:::as_bridge_tbl(simulated$indic, "indic", "indic")

  config <- bridgr:::validate_bridge_inputs(
    target_tbl = target_tbl,
    indic_tbl = indic_tbl,
    indic_predict = c("last", "last", "last"),
    indic_aggregators = c("mean", "mean", "mean"),
    indic_lags = 0,
    target_lags = 0,
    h = 1,
    frequency_conversions = NULL
  )

  target_meta <- bridgr:::infer_frequency_table(target_tbl)$target
  indic_meta <- bridgr:::infer_frequency_table(indic_tbl)$indicators
  expect_equal(indic_meta$unit[match(c("day", "week", "month"), indic_meta$id)], c("day", "week", "month"))

  aligned <- bridgr:::align_bridge_inputs(target_tbl, indic_tbl, target_meta, indic_meta)
  future_target_times <- bridgr:::target_future_times(max(aligned$target$time), target_meta, 1)
  expect_warning(
    indicator_results <- bridgr:::build_indicator_features(
      indic_tbl = aligned$indic,
      indic_meta = indic_meta,
      target_tbl = aligned$target,
      target_meta = target_meta,
      target_anchor = aligned$target_anchor,
      future_target_times = future_target_times,
      indic_predict = c("last", "last", "last"),
      indic_aggregators = config$indic_aggregators,
      frequency_conversions = config$frequency_conversions
    ),
    "Using the most recent observations"
  )

  estimation_long <- dplyr::bind_rows(
    aligned$target |> dplyr::select(id, time, values),
    indicator_results$aggregated
  ) |>
    dplyr::filter(time %in% aligned$target$time)
  estimation_set <- suppressMessages(tsbox::ts_wide(estimation_long)) |>
    stats::na.omit()

  fit <- stats::lm(target ~ day + week + month, data = estimation_set)
  estimated <- stats::coef(fit)[c("(Intercept)", "day", "week", "month")]
  expect_equal(
    unname(estimated),
    unname(simulated$coefficients[c("intercept", "day", "week", "month")]),
    tolerance = 7e-2
  )
})

test_that("bridge preprocessing recovers known coefficients for month-quarter-year data", {
  simulated <- make_month_quarter_year_simulation()

  target_tbl <- bridgr:::as_bridge_tbl(simulated$target, "target", "target") |>
    dplyr::mutate(id = "target")
  indic_tbl <- bridgr:::as_bridge_tbl(simulated$indic, "indic", "indic")

  config <- bridgr:::validate_bridge_inputs(
    target_tbl = target_tbl,
    indic_tbl = indic_tbl,
    indic_predict = c("last", "last", "last"),
    indic_aggregators = c("mean", "mean", "mean"),
    indic_lags = 0,
    target_lags = 0,
    h = 1,
    frequency_conversions = NULL
  )

  target_meta <- bridgr:::infer_frequency_table(target_tbl)$target
  indic_meta <- bridgr:::infer_frequency_table(indic_tbl)$indicators
  expect_equal(indic_meta$unit[match(c("month", "quarter", "year"), indic_meta$id)], c("month", "quarter", "year"))

  aligned <- bridgr:::align_bridge_inputs(target_tbl, indic_tbl, target_meta, indic_meta)
  future_target_times <- bridgr:::target_future_times(max(aligned$target$time), target_meta, 1)
  indicator_results <- bridgr:::build_indicator_features(
    indic_tbl = aligned$indic,
    indic_meta = indic_meta,
    target_tbl = aligned$target,
    target_meta = target_meta,
    target_anchor = aligned$target_anchor,
    future_target_times = future_target_times,
    indic_predict = c("last", "last", "last"),
    indic_aggregators = config$indic_aggregators,
    frequency_conversions = config$frequency_conversions
  )

  estimation_long <- dplyr::bind_rows(
    aligned$target |> dplyr::select(id, time, values),
    indicator_results$aggregated
  ) |>
    dplyr::filter(time %in% aligned$target$time)
  estimation_set <- suppressMessages(tsbox::ts_wide(estimation_long)) |>
    stats::na.omit()

  fit <- stats::lm(target ~ month + quarter + year, data = estimation_set)
  estimated <- stats::coef(fit)[c("(Intercept)", "month", "quarter", "year")]
  expect_equal(
    unname(estimated),
    unname(simulated$coefficients[c("intercept", "month", "quarter", "year")]),
    tolerance = 7e-2
  )
})

test_that("single-indicator expalmon recovers deterministic weights", {
  fixture <- make_expalmon_single_fixture()

  model <- bridge(
    target = fixture$target,
    indic = fixture$indic,
    indic_predict = "last",
    indic_aggregators = "expalmon",
    solver_options = list(seed = 123, n_starts = 2, maxiter = 100),
    h = 1
  )

  indicator_id <- model$indic_name[[1]]
  expect_equal(model$expalmon_weights[[indicator_id]], fixture$true_weights, tolerance = 0.08)
  expect_equal(model$expalmon_optimization$convergence, 0)
  expect_equal(sort(names(model$expalmon_parameters)), indicator_id)
})

test_that("joint expalmon optimization improves the final bridge fit over separate optimization", {
  fixture <- make_expalmon_joint_fixture(n_periods = 12)

  model <- bridge(
    target = fixture$target,
    indic = fixture$indic,
    indic_predict = c("last", "last"),
    indic_aggregators = c("expalmon", "expalmon"),
    solver_options = list(seed = 321, n_starts = 2, maxiter = 100),
    h = 1
  )

  estimation_times <- fixture$target$time
  target_tbl <- bridgr:::as_bridge_tbl(fixture$target, "target", "target") |>
    dplyr::mutate(id = "target")

  separate_weights <- lapply(
    c("x1", "x2"),
    function(indicator_id) {
      indicator_data <- subset(fixture$indic, id == indicator_id)
      periods <- rep(seq_len(nrow(fixture$target)), each = 7)
      objective <- function(parameters) {
        weights <- bridgr:::exp_almon(parameters, 7)
        aggregated <- vapply(
          seq_len(nrow(fixture$target)),
          function(i) {
            idx <- ((i - 1) * 7 + 1):(i * 7)
            sum(weights * indicator_data$value[idx])
          },
          FUN.VALUE = numeric(1)
        )
        sum(stats::residuals(stats::lm(fixture$target$value ~ aggregated))^2)
      }
      stats::optim(c(0, 0), objective, method = "BFGS", control = list(maxit = 100))$par
    }
  )
  names(separate_weights) <- c("x1", "x2")

  separate_long <- dplyr::bind_rows(
    dplyr::tibble(
      id = "x1",
      time = estimation_times,
      values = vapply(
        seq_len(nrow(fixture$target)),
        function(i) {
          idx <- ((i - 1) * 7 + 1):(i * 7)
          sum(bridgr:::exp_almon(separate_weights$x1, 7) * subset(fixture$indic, id == "x1")$value[idx])
        },
        FUN.VALUE = numeric(1)
      )
    ),
    dplyr::tibble(
      id = "x2",
      time = estimation_times,
      values = vapply(
        seq_len(nrow(fixture$target)),
        function(i) {
          idx <- ((i - 1) * 7 + 1):(i * 7)
          sum(bridgr:::exp_almon(separate_weights$x2, 7) * subset(fixture$indic, id == "x2")$value[idx])
        },
        FUN.VALUE = numeric(1)
      )
    )
  )
  separate_set <- bridgr:::build_bridge_estimation_set(
    target_tbl = target_tbl,
    target_name = "target",
    feature_long = separate_long,
    indic_lags = 0,
    estimation_times = estimation_times
  )

  joint_loss <- bridgr:::compute_bridge_loss(
    estimation_set = model$estimation_set,
    target_name = model$target_name,
    target_lags = model$target_lags
  )
  separate_loss <- bridgr:::compute_bridge_loss(
    estimation_set = separate_set,
    target_name = "target",
    target_lags = 0
  )

  expect_lt(joint_loss, separate_loss)
  expect_equal(model$expalmon_optimization$convergence, 0)
})

test_that("joint expalmon optimization works alongside fixed aggregators", {
  fixture <- make_expalmon_joint_fixture(n_periods = 12, include_mean_indicator = TRUE)

  model <- bridge(
    target = fixture$target,
    indic = fixture$indic,
    indic_predict = c("last", "last", "last"),
    indic_aggregators = c("expalmon", "expalmon", "mean"),
    solver_options = list(seed = 321, n_starts = 2, maxiter = 100),
    h = 1
  )

  expect_true(all(c("x1", "x2", "x3") %in% model$regressor_names))
  expect_equal(model$expalmon_optimization$convergence, 0)
  expect_equal(
    model$estimation_set$x3[[12]],
    mean(subset(fixture$indic, id == "x3")$value[78:84])
  )
})

test_that("auto.arima recovers the first-step indicator dynamics and propagates forecasts", {
  phi <- 0.7
  indic <- make_seeded_ar1_indicator(n = 240, phi = phi, seed = 123)
  target <- make_quarter_target(indic, n_quarters = 80, intercept = 0.5, slope = 1.1)

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "auto.arima",
    indic_aggregators = "mean",
    target_lags = 0,
    h = 1
  )

  expect_s3_class(model$indic_models$indic, "Arima")
  expect_true("ar1" %in% names(stats::coef(model$indic_models$indic)))
  expect_equal(unname(stats::coef(model$indic_models$indic)[["ar1"]]), phi, tolerance = 0.12)

  manual_monthly_forecast <- as.numeric(forecast::forecast(model$indic_models$indic, h = 3)$mean)
  expected_quarter_mean <- mean(manual_monthly_forecast)
  expect_equal(model$forecast_set$indic[[1]], expected_quarter_mean, tolerance = 1e-8)
})
