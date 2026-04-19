test_that(
  "forecast method `last` repeats the latest observed value",
  {
  fixture <- make_daily_week_fixture(
    n_weeks = 6,
    h = 1,
    indicator_values = seq_len(49)
  )
  target <- fixture$target
  indic <- fixture$indic

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_aggregators = "mean",
    h = 1
  )

  expected <- utils::tail(fixture$observed_indicator_values, 1)
  expect_equal(model$forecast_set[[model$indic_name[[1]]]][[1]], expected)
  }
)

test_that("forecast method `mean` repeats the last block mean", {
  fixture <- make_daily_week_fixture(
    n_weeks = 6,
    h = 1,
    indicator_values = seq_len(49)
  )
  target <- fixture$target
  indic <- fixture$indic

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "mean",
    indic_aggregators = "mean",
    h = 1
  )

  expected <- mean(fixture$observed_indicator_values[36:42])
  expect_equal(
    model$forecast_set[[model$indic_name[[1]]]][[1]],
    expected
  )
})

test_that("bridge suppresses tsbox value-name messages", {
  gdp_growth <- suppressMessages(tsbox::ts_na_omit(tsbox::ts_pc(gdp)))
  gdp_nowcast <- gdp_growth |>
    dplyr::slice_head(n = nrow(gdp_growth) - 1)
  baro_ragged <- baro |>
    dplyr::slice_head(n = nrow(baro) - 2)

  expect_no_message(
    bridge(
      target = gdp_nowcast,
      indic = baro_ragged,
      indic_predict = "mean",
      indic_aggregators = "mean",
      target_lags = 1,
      h = 1
    )
  )
})

test_that("forecast method `auto.arima` matches direct indicator forecasting", {
  indicator_values <- make_method_comparison_indicator(n = 147)$value
  fixture <- make_daily_week_fixture(
    n_weeks = 20,
    h = 1,
    indicator_values = indicator_values
  )
  target <- fixture$target
  indic <- fixture$indic

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "auto.arima",
    indic_aggregators = "mean",
    h = 1
  )

  direct_fit <- forecast::auto.arima(tsbox::ts_xts(indic))
  expected <- mean(as.numeric(forecast::forecast(direct_fit, h = 7)$mean))

  expect_s3_class(model$indic_models[[model$indic_name[[1]]]], "Arima")
  expect_equal(
    unname(stats::coef(model$indic_models[[model$indic_name[[1]]]])),
    unname(stats::coef(direct_fit)),
    tolerance = 1e-8
  )
  expect_equal(
    model$forecast_set[[model$indic_name[[1]]]][[1]],
    expected,
    tolerance = 1e-8
  )
})

test_that("forecast method `ets` matches direct indicator forecasting", {
  indicator_values <- make_method_comparison_indicator(n = 147)$value
  fixture <- make_daily_week_fixture(
    n_weeks = 20,
    h = 1,
    indicator_values = indicator_values
  )
  target <- fixture$target
  indic <- fixture$indic

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "ets",
    indic_aggregators = "mean",
    h = 1
  )

  direct_fit <- forecast::ets(tsbox::ts_xts(indic))
  expected <- mean(as.numeric(forecast::forecast(direct_fit, h = 7)$mean))

  expect_s3_class(model$indic_models[[model$indic_name[[1]]]], "ets")
  expect_equal(
    model$forecast_set[[model$indic_name[[1]]]][[1]],
    expected,
    tolerance = 1e-8
  )
})

test_that("aggregation method `mean` uses the within-period mean", {
  fixture <- make_daily_week_fixture(
    n_weeks = 6,
    h = 1,
    indicator_values = seq_len(49)
  )
  target <- fixture$target
  indic <- fixture$indic

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_aggregators = "mean",
    h = 1
  )

  expect_equal(model$estimation_set[[model$indic_name[[1]]]][[6]], mean(36:42))
})

test_that("aggregation method `last` uses the within-period last observation", {
  fixture <- make_daily_week_fixture(
    n_weeks = 6,
    h = 1,
    indicator_values = seq_len(49)
  )
  target <- fixture$target
  indic <- fixture$indic

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_aggregators = "last",
    h = 1
  )

  expect_equal(model$estimation_set[[model$indic_name[[1]]]][[6]], 42)
})

test_that("aggregation method `sum` uses the within-period sum", {
  fixture <- make_daily_week_fixture(
    n_weeks = 6,
    h = 1,
    indicator_values = seq_len(49)
  )
  target <- fixture$target
  indic <- fixture$indic

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_aggregators = "sum",
    h = 1
  )

  expect_equal(model$estimation_set[[model$indic_name[[1]]]][[6]], sum(36:42))
})

test_that("aggregation with numeric weights uses the supplied weights", {
  fixture <- make_daily_week_fixture(
    n_weeks = 6,
    h = 1,
    indicator_values = seq_len(49)
  )
  weights <- c(0.05, 0.1, 0.1, 0.15, 0.15, 0.2, 0.25)
  target <- fixture$target
  indic <- fixture$indic

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_aggregators = list(weights),
    h = 1
  )

  expect_equal(
    model$estimation_set[[model$indic_name[[1]]]][[6]],
    sum(weights * (36:42))
  )
})

test_that(
  "aggregation method `unrestricted` keeps one regressor per slot",
  {
  fixture <- make_daily_week_fixture(
    n_weeks = 6,
    h = 1,
    indicator_values = seq_len(49)
  )

  model <- suppressWarnings(bridge(
    target = fixture$target,
    indic = fixture$indic,
    indic_predict = "last",
    indic_aggregators = "unrestricted",
    h = 1
  ))

  prefix <- model$indic_name[[1]]
  expect_true(all(paste0(prefix, "_hf", 1:7) %in% model$regressor_names))
  expect_equal(model$estimation_set[[paste0(prefix, "_hf1")]][[6]], 36)
  expect_equal(model$estimation_set[[paste0(prefix, "_hf7")]][[6]], 42)
  }
)

test_that(
  "aggregation method `unrestricted` estimates distinct monthly slots",
  {
  n_quarters <- 40
  quarter_index <- rep(seq_len(n_quarters), each = 3)
  slot <- rep(1:3, times = n_quarters)
  monthly_time <- seq(
    as.Date("2010-01-01"),
    by = "month",
    length.out = n_quarters * 3
  )
  indic <- dplyr::tibble(
    time = monthly_time,
    value = 15 + quarter_index * 0.35 +
      ifelse(slot == 1, 0.8 * sin(quarter_index / 2), 0) +
      ifelse(slot == 2, -0.6 * cos(quarter_index / 3), 0) +
      ifelse(slot == 3, 0.7 * sin(quarter_index / 4 + 0.3), 0)
  )
  target <- dplyr::tibble(
    time = monthly_time[seq(1, length(monthly_time), by = 3)],
    value = 0.5 +
      vapply(
        seq_len(n_quarters),
        function(i) {
          block <- indic$value[((i - 1) * 3 + 1):(i * 3)]
          0.2 * block[[1]] + 0.6 * block[[2]] + 0.2 * block[[3]]
        },
        numeric(1)
      ) +
      rep(c(0.1, -0.05, 0.08, -0.02), length.out = n_quarters)
  )

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_aggregators = "unrestricted",
    h = 1
  )

  prefix <- model$indic_name[[1]]
  estimates <- stats::coef(model$model)[paste0(prefix, "_hf", 1:3)]

  expect_false(anyNA(estimates))
  expect_equal(unname(estimates), c(0.2, 0.6, 0.2), tolerance = 0.05)
  }
)

test_that("aggregation method `expalmon` uses its estimated weights", {
  fixture <- make_daily_week_fixture(
    n_weeks = 12,
    h = 1,
    indicator_values = 50 + seq_len(91) +
      rep(c(0, 2, -1, 3, -2, 1, 0), length.out = 91)
  )
  target <- fixture$target
  indic <- fixture$indic

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_aggregators = "expalmon",
    solver_options = list(seed = 42, n_starts = 3, maxiter = 300),
    h = 1
  )

  weights <- model$expalmon_weights[[model$indic_name[[1]]]]
  expect_length(weights, 7)
  expect_equal(sum(weights), 1, tolerance = 1e-8)
  expect_equal(
    model$estimation_set[[model$indic_name[[1]]]][[12]],
    sum(weights * fixture$observed_indicator_values[78:84]),
    tolerance = 1e-8
  )
})

test_that("aggregation method `beta` uses its estimated weights", {
  fixture <- make_daily_week_fixture(
    n_weeks = 12,
    h = 1,
    indicator_values = 60 + seq_len(91) +
      rep(c(1, -1, 2, -2, 3, -3, 0), length.out = 91)
  )

  model <- bridge(
    target = fixture$target,
    indic = fixture$indic,
    indic_predict = "last",
    indic_aggregators = "beta",
    solver_options = list(seed = 42, n_starts = 2, maxiter = 150),
    h = 1
  )

  weights <- model$parametric_weights[[model$indic_name[[1]]]]
  expect_length(weights, 7)
  expect_equal(sum(weights), 1, tolerance = 1e-8)
  expect_equal(
    model$estimation_set[[model$indic_name[[1]]]][[12]],
    sum(weights * fixture$observed_indicator_values[78:84]),
    tolerance = 1e-8
  )
})

test_that("forecast method `direct` aligns blocks backward", {
  fixture <- make_daily_week_fixture(
    n_weeks = 6,
    h = 1,
    indicator_values = seq_len(49)
  )
  indic <- dplyr::tibble(
    time = seq(min(fixture$indic$time), by = "day", length.out = 43),
    value = seq_len(43)
  )

  model <- bridge(
    target = fixture$target,
    indic = indic,
    indic_predict = "direct",
    indic_aggregators = "mean",
    h = 1
  )

  expect_equal(model$forecast_set[[model$indic_name[[1]]]][[1]], mean(37:43))
  expect_equal(min(model$estimation_set$time), fixture$target$time[[2]])
})

test_that("forecast method `direct` ignores supplied aggregators", {
  fixture <- make_daily_week_fixture(
    n_weeks = 6,
    h = 1,
    indicator_values = seq_len(49)
  )
  indic <- dplyr::tibble(
    time = seq(min(fixture$indic$time), by = "day", length.out = 43),
    value = seq_len(43)
  )

  model <- bridge(
    target = fixture$target,
    indic = indic,
    indic_predict = "direct",
    indic_aggregators = "expalmon",
    solver_options = list(seed = 7, n_starts = 2, maxiter = 100),
    h = 1
  )

  expect_equal(model$forecast_set[[model$indic_name[[1]]]][[1]], mean(37:43))
  expect_length(model$parametric_weights, 0)
  expect_null(model$parametric_optimization)
})

test_that("forecast method `direct` supports horizons greater than one", {
  fixture <- make_daily_week_fixture(
    n_weeks = 6,
    h = 2,
    indicator_values = seq_len(56)
  )
  indic <- dplyr::tibble(
    time = seq(min(fixture$indic$time), by = "day", length.out = 43),
    value = seq_len(43)
  )

  model <- bridge(
    target = fixture$target,
    indic = indic,
    indic_predict = "direct",
    indic_aggregators = "mean",
    h = 2
  )

  expect_equal(nrow(model$forecast_set), 2)
  expect_equal(model$forecast_set[[model$indic_name[[1]]]][[1]], mean(30:36))
  expect_equal(model$forecast_set[[model$indic_name[[1]]]][[2]], mean(37:43))
})

test_that("beta weights match the normalized beta definition used by midasr", {
  weights <- bridgr:::parametric_weights("beta", c(2, 4), 7)
  eps <- .Machine$double.eps
  positions <- (0:6) / 6
  positions[[1]] <- positions[[1]] + eps
  positions[[7]] <- positions[[7]] - eps
  expected <- positions^(2 - 1) * (1 - positions)^(4 - 1)
  expected <- expected / sum(expected)

  expect_equal(weights, expected, tolerance = 1e-12)
})

test_that(
  "aggregation method `unrestricted` warns for dense predictors",
  {
  fixture <- make_daily_week_fixture(
    n_weeks = 12,
    h = 1,
    indicator_values = seq_len(91)
  )

  expect_warning(
    bridge(
      target = fixture$target,
      indic = fixture$indic,
      indic_predict = "last",
      indic_aggregators = "unrestricted",
      h = 1
    ),
    "10-observations-per-predictor guideline"
  )
  }
)

test_that("a full pipeline example works end to end", {
  indicator_values <- make_method_comparison_indicator(n = 147)$value
  fixture <- make_daily_week_fixture(
    n_weeks = 20,
    h = 1,
    indicator_values = indicator_values
  )
  target <- fixture$target
  indic <- fixture$indic

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "auto.arima",
    indic_aggregators = "mean",
    indic_lags = 1,
    target_lags = 1,
    h = 1
  )

  fcst <- forecast(model)

  expect_s3_class(model, "bridge")
  expect_s3_class(fcst, "forecast")
  expect_equal(nrow(model$forecast_set), 1)
  expect_equal(nrow(fcst$forecast_set), 1)
  expect_true(all(c("indic", "indic_lag1") %in% model$regressor_names))
})

test_that("plot.bridge renders forecast and fit views", {
  fixture <- make_daily_week_fixture(
    n_weeks = 60,
    h = 3,
    indicator_values = make_method_comparison_indicator(n = 441)$value
  )

  model <- bridge(
    target = fixture$target,
    indic = fixture$indic,
    indic_predict = "auto.arima",
    indic_aggregators = "mean",
    indic_lags = 1,
    target_lags = 1,
    h = 1
  )

  interval_model <- bridge(
    target = fixture$target,
    indic = fixture$indic,
    indic_predict = "auto.arima",
    indic_aggregators = "mean",
    indic_lags = 1,
    target_lags = 1,
    h = 3,
    se = TRUE,
    bootstrap = list(N = 8, block_length = 4)
  )

  forecast_plot <- plot(interval_model)
  fit_plot <- plot(model, type = "fit")
  forecast_build <- ggplot2::ggplot_build(forecast_plot)
  expected_start <- as.numeric(fixture$target$time[[11]])

  expect_s3_class(forecast_plot, "ggplot")
  expect_s3_class(fit_plot, "ggplot")
  expect_equal(min(forecast_build$data[[1]]$x), expected_start)
  expect_equal(nrow(forecast_build$data[[1]]), 50)
})

test_that("theme_bridgr accepts legacy dotted legend arguments", {
  legacy_theme <- theme_bridgr(
    legend.position = "top",
    legend.direction = "vertical"
  )

  expect_equal(legacy_theme$legend.position, "top")
  expect_equal(legacy_theme$legend.direction, "vertical")
})
