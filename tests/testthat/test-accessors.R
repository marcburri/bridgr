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

test_that("residuals.mf_model delegates residual diagnostics to the target fit", {
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
})
