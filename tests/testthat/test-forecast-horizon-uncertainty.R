#' @srrstats {TS3.0} Forecast tests demonstrate at least one recursive bridge
#' case in which prediction uncertainty widens with forecast horizon.
#' @srrstats {RE4.15} Forecast tests demonstrate that recursive bridge
#' prediction intervals widen across horizons in a fitted mixed-frequency
#' regression.
#' @srrstats {RE7.4} Regression tests confirm that forecast uncertainty grows
#' with horizon for at least one fitted bridge model.
test_that("forecast uncertainty widens with horizon in recursive bridges", {
  withr::local_seed(123)

  indic <- make_monthly_indicator(n = 60)
  target <- make_quarter_target(indic, n_quarters = 20)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    target_lags = 1,
    se = TRUE,
    bootstrap = list(N = 400, block_length = 3),
    h = 3
  )
  fc <- forecast(model, level = c(80, 95))

  width_80 <- fc$upper[, "80%"] - fc$lower[, "80%"]
  width_95 <- fc$upper[, "95%"] - fc$lower[, "95%"]

  expect_true(all(diff(fc$se) > 0))
  expect_true(all(diff(width_80) > 0))
  expect_true(all(diff(width_95) > 0))
})
