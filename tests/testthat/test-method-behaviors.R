test_that("forecast method `last` repeats the latest observed high-frequency value", {
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
})

test_that("forecast method `mean` repeats the last target-period mean", {
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
  expect_equal(model$forecast_set[[model$indic_name[[1]]]][[1]], expected)
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
  expect_equal(model$forecast_set[[model$indic_name[[1]]]][[1]], expected, tolerance = 1e-8)
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
  expect_equal(model$forecast_set[[model$indic_name[[1]]]][[1]], expected, tolerance = 1e-8)
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

  expect_equal(model$estimation_set[[model$indic_name[[1]]]][[6]], sum(weights * (36:42)))
})

test_that("aggregation method `expalmon` uses its estimated weights", {
  fixture <- make_daily_week_fixture(
    n_weeks = 12,
    h = 1,
    indicator_values = 50 + seq_len(91) + rep(c(0, 2, -1, 3, -2, 1, 0), length.out = 91)
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
