test_that("summary.bridge reports deterministic custom weights", {
  indic <- make_monthly_indicator()
  target <- make_quarter_target(indic, n_quarters = 6)

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_aggregators = list(c(0.2, 0.3, 0.5)),
    h = 1
  )

  output <- capture.output(summary(model))

  expect_true(any(grepl("Bridge model summary", output, fixed = TRUE)))
  expect_true(any(grepl(
    "Indicator model: deterministic extension",
    output,
    fixed = TRUE
  )))
  expect_true(any(grepl("Aggregation: custom weights", output, fixed = TRUE)))
  expect_true(any(grepl("0.2, 0.3, 0.5", output, fixed = TRUE)))
})

test_that("summary.bridge reports parametric optimization details", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- suppressWarnings(bridge(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_aggregators = "beta",
    solver_options = list(
      start_values = c(2, 3),
      seed = 42,
      n_starts = 1,
      maxiter = 100
    ),
    h = 1
  ))
  model$parametric_optimization$message <- "optimizer note"

  output <- capture.output(summary(model))

  expect_true(any(grepl("Estimated parametric weights:", output, fixed = TRUE)))
  expect_true(any(grepl(
    "Estimated parametric parameters:",
    output,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Joint parametric aggregation optimization:",
    output,
    fixed = TRUE
  )))
  expect_true(any(grepl("Message: optimizer note", output, fixed = TRUE)))
})

test_that("forecast.bridge accepts custom xreg for ARIMA bridge models", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_lags = 1,
    target_lags = 1,
    h = 2
  )

  default_forecast <- forecast(model)
  custom_xreg <- dplyr::tibble(
    id = rep(model$regressor_names, each = nrow(model$forecast_set)),
    time = rep(model$forecast_set$time, times = length(model$regressor_names)),
    value = c(model$forecast_set$indic + 10, model$forecast_set$indic_lag1 + 10)
  )
  scenario_forecast <- forecast(model, xreg = custom_xreg)

  expect_s3_class(model$model, "Arima")
  expect_s3_class(scenario_forecast, "forecast")
  expect_equal(nrow(scenario_forecast$forecast_set), 2)
  expect_gt(
    max(abs(as.numeric(scenario_forecast$mean - default_forecast$mean))),
    1e-6
  )
  expect_equal(
    scenario_forecast$forecast_set[, model$regressor_names, drop = FALSE],
    dplyr::tibble(
      indic = model$forecast_set$indic + 10,
      indic_lag1 = model$forecast_set$indic_lag1 + 10
    )
  )
})

test_that("forecast.bridge errors when custom xreg omits required regressors", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_lags = 1,
    target_lags = 1,
    h = 2
  )

  incomplete_xreg <- dplyr::tibble(
    id = "indic",
    time = model$forecast_set$time,
    value = model$forecast_set$indic
  )

  expect_error(
    forecast(model, xreg = incomplete_xreg),
    "missing required regressors: indic_lag1"
  )
})
