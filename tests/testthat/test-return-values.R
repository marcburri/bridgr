#' @srrstats {G5.3} Standard fitted models and uncertainty-enabled forecasts
#' return finite numeric outputs with no `NA`, `NaN`, or `Inf` values in
#' components expected to be defined.
test_that(
  "standard fitted models and forecasts return finite numeric outputs",
  {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_lags = 1,
    target_lags = 1,
    se = TRUE,
    bootstrap = list(N = 8, block_length = 3),
    h = 2
  )
  fc <- forecast(model)

  expect_false(anyNA(stats::coef(model)))
  expect_true(all(is.finite(stats::coef(model))))
  expect_false(anyNA(stats::vcov(model)))
  expect_true(all(is.finite(stats::vcov(model))))
  expect_false(anyNA(stats::fitted(model)))
  expect_true(all(is.finite(stats::fitted(model))))
  expect_false(anyNA(stats::residuals(model)))
  expect_true(all(is.finite(stats::residuals(model))))

  estimation_numeric <- model$estimation_set[
    setdiff(names(model$estimation_set), "time")
  ]
  forecast_numeric <- model$forecast_set[
    setdiff(names(model$forecast_set), "time")
  ]
  expect_false(anyNA(estimation_numeric))
  expect_true(all(vapply(
    estimation_numeric,
    function(column) all(is.finite(as.numeric(column))),
    logical(1)
  )))
  expect_false(anyNA(forecast_numeric))
  expect_true(all(vapply(
    forecast_numeric,
    function(column) all(is.finite(as.numeric(column))),
    logical(1)
  )))

  expect_false(anyNA(fc$mean))
  expect_true(all(is.finite(fc$mean)))
  expect_false(anyNA(fc$se))
  expect_true(all(is.finite(fc$se)))
  expect_false(anyNA(fc$lower))
  expect_true(all(is.finite(fc$lower)))
  expect_false(anyNA(fc$upper))
  expect_true(all(is.finite(fc$upper)))
  }
)
