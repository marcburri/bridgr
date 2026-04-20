test_that("bridge warns and forwards to mf_model", {
  indic <- make_monthly_indicator()
  target <- make_quarter_target(indic, n_quarters = 6)

  expect_warning(
    model <- bridge(
      target = target,
      indic = indic,
      indic_predict = "last",
      h = 1
    ),
    "deprecated"
  )

  expect_s3_class(model, "mf_model")
  expect_equal(model$target_name, "target")
})

test_that("bridge keeps indicators beyond the forecast horizon", {
  indic <- make_monthly_indicator()
  target <- make_quarter_target(indic, n_quarters = 6)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    h = 1
  )

  expect_match(format(model$formula), "indic")
  expect_true("indic" %in% model$regressor_names)
  expect_equal(nrow(model$forecast_set), 1)
})

test_that("mf_model supports do.call with literal ts-boxable objects", {
  indic <- make_monthly_indicator()
  target <- make_quarter_target(indic, n_quarters = 6)

  model <- do.call(
    mf_model,
    list(
      target = target,
      indic = indic,
      indic_predict = "LAST",
      indic_aggregators = "MEAN",
      h = 1L
    )
  )

  expect_s3_class(model, "mf_model")
  expect_equal(model$target_name, "target")
  expect_equal(model$indic_name, "indic")
  expect_equal(model$indic_predict, "last")
  expect_equal(model$indic_aggregators, list("mean"))
})

test_that("mf_model supports quoted do.call inputs", {
  indic <- make_monthly_indicator()
  target <- make_quarter_target(indic, n_quarters = 6)

  model <- do.call(
    mf_model,
    list(
      target = target,
      indic = indic,
      indic_predict = "LAST",
      indic_aggregators = "MEAN",
      indic_lags = 1L,
      target_lags = 1L,
      h = 1L,
      se = TRUE,
      bootstrap = list(N = 5),
      full_system_bootstrap = FALSE
    ),
    quote = TRUE
  )

  expect_s3_class(model, "mf_model")
  expect_equal(model$target_name, "target")
  expect_equal(model$indic_name, "indic")
  expect_equal(model$indic_predict, "last")
  expect_equal(model$indic_aggregators, list("mean"))
})

test_that("bridge supports multiple indicators with mixed aggregation", {
  indic <- make_multi_indicator()
  target <- make_quarter_target(
    monthly_indicator = dplyr::tibble(
      time = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
      value = 10 + seq_len(24) + rep(c(0, 2, -1, 3), length.out = 24)
    ),
    n_quarters = 6
  )

  model <- mf_model(
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
    model <- mf_model(
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
    mf_model(
      target = target,
      indic = indic,
      indic_predict = "last",
      frequency_conversions = c(dpw = 8),
      h = 1
    ),
    "fewer observations within at least one target period"
  )
})

test_that("zero-length target or indicator inputs fail early", {
  empty_series <- dplyr::tibble(
    time = as.Date(character()),
    value = numeric()
  )
  indic <- make_monthly_indicator(n = 24)
  target <- make_quarter_target(indic, n_quarters = 8)

  expect_error(
    mf_model(target = empty_series, indic = indic, h = 1),
    "must contain at least one observation"
  )
  expect_error(
    mf_model(target = target, indic = empty_series, h = 1),
    "must contain at least one observation"
  )
})

test_that("unsupported value types fail during preprocessing", {
  char_target <- dplyr::tibble(
    time = seq(as.Date("2020-01-01"), by = "quarter", length.out = 8),
    value = rep("bad", 8)
  )
  complex_indic <- dplyr::tibble(
    time = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
    value = complex(real = seq_len(24), imaginary = 1)
  )
  indic <- make_monthly_indicator(n = 24)
  target <- make_quarter_target(indic, n_quarters = 8)

  expect_error(
    mf_model(target = char_target, indic = indic, h = 1),
    "must contain a numeric `value`/`values` column"
  )
  expect_error(
    mf_model(target = target, indic = complex_indic, h = 1),
    "must contain a numeric `value`/`values` column"
  )
})

test_that("all-NA and all-identical series edge cases are covered", {
  indic <- make_monthly_indicator(n = 24)
  target <- make_quarter_target(indic, n_quarters = 8)
  all_na_target <- dplyr::tibble(
    time = target$time,
    value = rep(NA_real_, nrow(target))
  )
  all_na_indic <- dplyr::tibble(
    time = indic$time,
    value = rep(NA_real_, nrow(indic))
  )
  all_identical_indic <- dplyr::tibble(
    time = indic$time,
    value = rep(5, nrow(indic))
  )

  expect_error(
    mf_model(target = all_na_target, indic = indic, h = 1),
    "contains missing values"
  )
  expect_error(
    mf_model(target = target, indic = all_na_indic, h = 1),
    "contains missing values"
  )

  identical_model <- mf_model(
    target = target,
    indic = all_identical_indic,
    indic_predict = "last",
    h = 1
  )
  expect_s3_class(identical_model, "mf_model")
  expect_false(anyNA(stats::fitted(identical_model$model)))
  expect_false(anyNA(identical_model$forecast_set[[identical_model$indic_name[[1]]]]))
})

test_that("invalid or lower-frequency indicators are rejected", {
  indic <- dplyr::tibble(
    time = seq(as.Date("2020-01-01"), by = "quarter", length.out = 4),
    value = c(1, 2, 3, 4)
  )
  target <- dplyr::tibble(
    time = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    value = seq_len(12)
  )

  expect_error(
    mf_model(target = target, indic = indic),
    "lower-frequency than the target"
  )
})

test_that("direct alignment must be used for all indicators", {
  indic <- make_multi_indicator()
  target <- make_quarter_target(
    monthly_indicator = dplyr::tibble(
      time = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
      value = 10 + seq_len(24)
    ),
    n_quarters = 6
  )

  expect_error(
    mf_model(
      target = target,
      indic = indic,
      indic_predict = c("direct", "last"),
      h = 1
    ),
    "must be used for all indicators"
  )
})

test_that("parametric starting values must have the exact required length", {
  indic <- make_monthly_indicator()
  target <- make_quarter_target(indic, n_quarters = 6)

  expect_error(
    mf_model(
      target = target,
      indic = indic,
      indic_predict = "last",
      indic_aggregators = "beta",
      solver_options = list(start_values = 0.1),
      h = 1
    ),
    "must contain exactly 2 values"
  )

  expect_error(
    mf_model(
      target = target,
      indic = dplyr::tibble(
        id = rep(c("a", "b"), each = nrow(indic)),
        time = rep(indic$time, times = 2),
        value = c(indic$value, indic$value + 5)
      ),
      indic_predict = c("last", "last"),
      indic_aggregators = c("expalmon", "beta"),
      solver_options = list(start_values = list(a = c(0, 0))),
      h = 1
    ),
    "must provide exactly 2 parametric indicator start vectors"
  )

  expect_error(
    mf_model(
      target = target,
      indic = indic,
      indic_predict = "last",
      indic_aggregators = "legendre",
      h = 1
    ),
    "Character values in `indic_aggregators` must be one of"
  )
})

test_that("deprecated solver option `start` is rejected", {
  indic <- make_monthly_indicator()
  target <- make_quarter_target(indic, n_quarters = 6)

  expect_error(
    mf_model(
      target = target,
      indic = indic,
      indic_predict = "last",
      indic_aggregators = "expalmon",
      solver_options = list(start = c(0, 0)),
      h = 1
    ),
    "Invalid `solver_options`: start"
  )
})

test_that("forecast.mf_model uses stored regressors and accepts xreg", {
  indic <- make_monthly_indicator()
  target <- make_quarter_target(indic, n_quarters = 6)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_lags = 1,
    se = TRUE,
    bootstrap = list(N = 8, block_length = 2),
    h = 2
  )

  default_forecast <- forecast(model)
  expect_s3_class(default_forecast, "mf_model_forecast")
  expect_s3_class(default_forecast, "forecast")
  expect_equal(nrow(default_forecast$forecast_set), 2)
  expect_equal(length(default_forecast$se), 2)
  expect_false(default_forecast$bootstrap$enabled)
  expect_equal(
    default_forecast$uncertainty$prediction_method,
    "residual_resampling"
  )
  expect_equal(
    default_forecast$uncertainty$simulation_paths,
    8L
  )

  custom_xreg <- dplyr::tibble(
    id = rep(c("indic", "indic_lag1"), each = 2),
    time = rep(model$forecast_set$time, times = 2),
    value = c(model$forecast_set$indic + 1, model$forecast_set$indic_lag1 + 1)
  )
  scenario_forecast <- forecast(model, xreg = custom_xreg)

  expect_s3_class(scenario_forecast, "forecast")
  expect_equal(nrow(scenario_forecast$forecast_set), 2)
})

test_that("summary.mf_model prints model information", {
  indic <- make_monthly_indicator()
  target <- make_quarter_target(indic, n_quarters = 6)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    h = 1
  )

  output <- capture.output(summary(model))
  expect_true(any(grepl("Mixed-frequency model summary", output)))
  expect_true(any(grepl("Target series:", output)))
  expect_true(any(grepl("Indicator summary:", output)))
})

test_that("se = FALSE leaves bootstrap inactive", {
  indic <- make_monthly_indicator()
  target <- make_quarter_target(indic, n_quarters = 6)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    se = FALSE,
    bootstrap = list(N = 10, block_length = 2),
    h = 1
  )
  fc <- forecast(model)

  expect_false(model$bootstrap$enabled)
  expect_true(all(is.na(fc$se)))
  expect_true(all(is.na(fc$lower)))
  expect_true(all(is.na(fc$upper)))
})

test_that("full_system_bootstrap is validated as logical", {
  indic <- make_monthly_indicator()
  target <- make_quarter_target(indic, n_quarters = 6)

  expect_error(
    mf_model(
      target = target,
      indic = indic,
      full_system_bootstrap = "yes"
    ),
    "`full_system_bootstrap` must be either `TRUE` or `FALSE`"
  )
})

test_that("bridge validates duplicate timestamps and missing values", {
  duplicate_target <- dplyr::tibble(
    time = as.Date(c("2020-01-01", "2020-01-01", "2020-04-01")),
    value = c(1, 2, 3)
  )
  indic <- make_monthly_indicator(n = 12)

  expect_error(
    mf_model(target = duplicate_target, indic = indic),
    "duplicated values in time column|duplicate timestamps"
  )

  missing_indic <- indic
  missing_indic$value[[3]] <- NA_real_
  target <- make_quarter_target(indic, n_quarters = 4)

  expect_error(
    mf_model(target = target, indic = missing_indic),
    "contains missing values"
  )
})

test_that(
  paste(
    "bridge preprocessing recovers known coefficients from a",
    "deterministic multi-frequency simulation"
  ),
  {
    simulated <- make_exact_multifrequency_simulation()

    target_tbl <- bridgr:::as_bridge_tbl(
      simulated$target,
      "target",
      "target"
    ) |>
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
    expect_equal(
      indic_meta$unit[match(c("second", "minute", "hour"), indic_meta$id)],
      c("second", "minute", "hour")
    )

    aligned <- bridgr:::align_bridge_inputs(
      target_tbl,
      indic_tbl,
      target_meta,
      indic_meta
    )
    future_target_times <- bridgr:::target_future_times(
      max(aligned$target$time),
      target_meta,
      1
    )
    all_target_times <- c(unique(aligned$target$time), future_target_times)
    indicator_results <- bridgr:::build_indicator_features(
      indic_tbl = aligned$indic,
      indic_meta = indic_meta,
      target_tbl = aligned$target,
      target_meta = target_meta,
      target_anchor = aligned$target_anchor,
      all_target_times = all_target_times,
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
      unname(
        simulated$coefficients[c("intercept", "second", "minute", "hour")]
      ),
      tolerance = 5e-2
    )
  }
)

test_that(
  "bridge preprocessing recovers known coefficients for day-week-month data",
  {
    simulated <- make_day_week_month_simulation()

    target_tbl <- bridgr:::as_bridge_tbl(
      simulated$target,
      "target",
      "target"
    ) |>
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
    expect_equal(
      indic_meta$unit[match(c("day", "week", "month"), indic_meta$id)],
      c("day", "week", "month")
    )

    aligned <- bridgr:::align_bridge_inputs(
      target_tbl,
      indic_tbl,
      target_meta,
      indic_meta
    )
    future_target_times <- bridgr:::target_future_times(
      max(aligned$target$time),
      target_meta,
      1
    )
    all_target_times <- c(unique(aligned$target$time), future_target_times)
    expect_warning(
      indicator_results <- bridgr:::build_indicator_features(
        indic_tbl = aligned$indic,
        indic_meta = indic_meta,
        target_tbl = aligned$target,
        target_meta = target_meta,
        target_anchor = aligned$target_anchor,
        all_target_times = all_target_times,
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
  }
)

test_that(
  "bridge preprocessing recovers month-quarter-year coefficients",
  {
    simulated <- make_month_quarter_year_simulation()

    target_tbl <- bridgr:::as_bridge_tbl(
      simulated$target,
      "target",
      "target"
    ) |>
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
    expect_equal(
      indic_meta$unit[match(c("month", "quarter", "year"), indic_meta$id)],
      c("month", "quarter", "year")
    )

    aligned <- bridgr:::align_bridge_inputs(
      target_tbl,
      indic_tbl,
      target_meta,
      indic_meta
    )
    future_target_times <- bridgr:::target_future_times(
      max(aligned$target$time),
      target_meta,
      1
    )
    all_target_times <- c(unique(aligned$target$time), future_target_times)
    indicator_results <- bridgr:::build_indicator_features(
      indic_tbl = aligned$indic,
      indic_meta = indic_meta,
      target_tbl = aligned$target,
      target_meta = target_meta,
      target_anchor = aligned$target_anchor,
      all_target_times = all_target_times,
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
      unname(
        simulated$coefficients[c("intercept", "month", "quarter", "year")]
      ),
      tolerance = 7e-2
    )
  }
)

test_that("single-indicator expalmon recovers deterministic weights", {
  fixture <- make_expalmon_single_fixture()

  model <- mf_model(
    target = fixture$target,
    indic = fixture$indic,
    indic_predict = "last",
    indic_aggregators = "expalmon",
    solver_options = list(seed = 123, n_starts = 2, maxiter = 100),
    h = 1
  )

  indicator_id <- model$indic_name[[1]]
  expect_equal(
    model$expalmon_weights[[indicator_id]],
    fixture$true_weights,
    tolerance = 0.08
  )
  expect_equal(model$expalmon_optimization$convergence, 0)
  expect_equal(sort(names(model$expalmon_parameters)), indicator_id)
})

test_that("joint expalmon optimization improves the fit", {
  fixture <- make_expalmon_joint_fixture(n_periods = 12)

  model <- mf_model(
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
      stats::optim(
        c(0, 0),
        objective,
        method = "BFGS",
        control = list(maxit = 100)
      )$par
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
          sum(
            bridgr:::exp_almon(separate_weights$x1, 7) *
              subset(fixture$indic, id == "x1")$value[idx]
          )
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
          sum(
            bridgr:::exp_almon(separate_weights$x2, 7) *
              subset(fixture$indic, id == "x2")$value[idx]
          )
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
  fixture <- make_expalmon_joint_fixture(
    n_periods = 12,
    include_mean_indicator = TRUE
  )

  model <- mf_model(
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

test_that("auto.arima recovers indicator dynamics", {
  phi <- 0.7
  indic <- make_seeded_ar1_indicator(n = 240, phi = phi, seed = 123)
  target <- make_quarter_target(
    indic,
    n_quarters = 80,
    intercept = 0.5,
    slope = 1.1
  )

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "auto.arima",
    indic_aggregators = "mean",
    target_lags = 0,
    h = 1
  )

  expect_s3_class(model$indic_models$indic, "Arima")
  expect_true("ar1" %in% names(stats::coef(model$indic_models$indic)))
  expect_equal(
    unname(stats::coef(model$indic_models$indic)[["ar1"]]),
    phi,
    tolerance = 0.12
  )

  manual_monthly_forecast <- as.numeric(
    forecast::forecast(model$indic_models$indic, h = 3)$mean
  )
  expected_quarter_mean <- mean(manual_monthly_forecast)
  expect_equal(
    model$forecast_set$indic[[1]],
    expected_quarter_mean,
    tolerance = 1e-8
  )
})

test_that("indicator-dynamics recovery is stable across multiple seeds", {
  phi <- 0.7
  seeds <- c(11, 123, 987)

  estimates <- vapply(
    seeds,
    function(seed) {
      indic <- make_seeded_ar1_indicator(n = 240, phi = phi, seed = seed)
      target <- make_quarter_target(
        indic,
        n_quarters = 80,
        intercept = 0.5,
        slope = 1.1
      )
      model <- mf_model(
        target = target,
        indic = indic,
        indic_predict = "auto.arima",
        indic_aggregators = "mean",
        target_lags = 0,
        h = 1
      )

      unname(stats::coef(model$indic_models$indic)[["ar1"]])
    },
    numeric(1)
  )

  expect_equal(estimates, rep(phi, length(seeds)), tolerance = 0.12)
})
