#' @srrstats {G5.10} Extended regression tests live under the standard
#' `testthat` framework and are enabled with the `BRIDGR_EXTENDED_TESTS=true`
#' environment flag.
#' @srrstats {G5.12} The activation flag, expected scope, and runtime intent for
#' the extended test tier are documented in `tests/README.md`.
test_that("extended bootstrap forecasts stay finite on a larger sample", {
  skip_if_not_bridgr_extended_tests()
  withr::local_seed(2024)

  indic <- make_multi_indicator(n = 180)
  target_source <- indic |>
    dplyr::filter(.data$id == "a") |>
    dplyr::select(time, value)
  target <- make_quarter_target(target_source, n_quarters = 60, slope = 0.75)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = c("last", "last"),
    indic_aggregators = c("mean", "last"),
    target_lags = 1,
    indic_lags = 1,
    se = TRUE,
    bootstrap = list(N = 250, block_length = 4),
    h = 4
  )
  fc <- forecast(model, level = c(80, 95))

  expect_s3_class(model, "mf_model")
  expect_s3_class(fc, "mf_model_forecast")
  expect_true(all(is.finite(fitted(model))))
  expect_true(all(is.finite(residuals(model))))
  expect_true(all(is.finite(fc$mean)))
  expect_true(all(is.finite(fc$se)))
  expect_true(all(is.finite(fc$lower)))
  expect_true(all(is.finite(fc$upper)))
})
