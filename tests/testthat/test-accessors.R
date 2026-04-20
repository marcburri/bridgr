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
