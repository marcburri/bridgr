#' @srrstats {RE7.3} Returned model objects are exercised through `coef()`,
#' `confint()`, `formula()`, `nobs()`, `vcov()`, `fitted()`, `residuals()`,
#' and `print()`.
test_that("coef.mf_model delegates to the stored target regression", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_lags = 1,
    target_lags = 1,
    h = 1
  )

  expect_equal(coef(model), stats::coef(model$model))
  expect_equal(stats::coefficients(model), stats::coef(model$model))
})

test_that("confint.mf_model uses the reported coefficient covariance matrix", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    target_lags = 1,
    se = TRUE,
    bootstrap = list(N = 8, block_length = 3),
    h = 1
  )

  intervals <- stats::confint(model)
  critical_value <- stats::qt(0.975, df = stats::df.residual(model$model))
  standard_errors <- sqrt(diag(stats::vcov(model)))
  expected <- cbind(
    stats::coef(model) - critical_value * standard_errors,
    stats::coef(model) + critical_value * standard_errors
  )
  colnames(expected) <- colnames(intervals)

  expect_equal(intervals, expected)
})

test_that("formula.mf_model returns the stored bridge regression formula", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_lags = 1,
    target_lags = 1,
    h = 1
  )

  expect_identical(formula(model), model$formula)
})

test_that("nobs.mf_model returns the fitted target-equation sample size", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_lags = 1,
    target_lags = 1,
    h = 1
  )

  expect_equal(stats::nobs(model), nrow(model$estimation_set))
  expect_equal(stats::nobs(model), stats::nobs(model$model))
})

test_that("vcov.mf_model returns stored covariance or the model fallback", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  plain_model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    target_lags = 1,
    h = 1
  )
  se_model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    target_lags = 1,
    se = TRUE,
    bootstrap = list(N = 8, block_length = 3),
    h = 1
  )

  expect_equal(stats::vcov(plain_model), stats::vcov(plain_model$model))
  expect_equal(
    stats::vcov(se_model),
    se_model$uncertainty$coefficient_covariance
  )
})

test_that("fitted.mf_model returns the in-sample fitted path", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    target_lags = 1,
    h = 1
  )

  expect_equal(stats::fitted(model), stats::fitted(model$model))
  expect_equal(length(stats::fitted(model)), nrow(model$estimation_set))
})

test_that(
  "residuals.mf_model delegates residual diagnostics to the target fit",
  {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    target_lags = 1,
    h = 1
  )

  expect_equal(stats::residuals(model), stats::residuals(model$model))
  expect_equal(
    as.numeric(stats::fitted(model) + stats::residuals(model)),
    as.numeric(model$estimation_set[[model$target_name]])
  )
  }
)

test_that("print.mf_model exposes the package summary layout", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    h = 1
  )

  output <- capture.output(print(model))

  expect_true(any(grepl("Mixed-frequency model summary", output, fixed = TRUE)))
  expect_true(any(grepl("Target equation coefficients:", output, fixed = TRUE)))
  expect_true(any(grepl("Indicator summary:", output, fixed = TRUE)))
})

test_that(
  "returned model objects behave consistently across accessor methods",
  {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    target_lags = 1,
    se = TRUE,
    bootstrap = list(N = 8, block_length = 3),
    h = 1
  )

  coefficients <- stats::coef(model)
  intervals <- stats::confint(model)

  expect_identical(rownames(intervals), names(coefficients))
  expect_identical(stats::formula(model), model$formula)
  expect_equal(stats::nobs(model), nrow(model$estimation_set))
  expect_equal(stats::vcov(model), model$uncertainty$coefficient_covariance)
  expect_equal(stats::fitted(model), stats::fitted(model$model))
  expect_equal(stats::residuals(model), stats::residuals(model$model))
  expect_invisible(print(model))
  }
)
