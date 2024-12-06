library(testthat)
library(bridgr)

test_that("bridge handles missing inputs gracefully", {
  expect_error(bridge(), "argument \"target\" is missing")
  expect_error(bridge(target = NULL, indic = NULL), "@target must be a ts-boxable object, not NULL")
})

test_that("bridge validates input frequencies", {
  # Mock target and indicator time series
  target <- ts(c(1, 2, 1), start = c(2000, 1), frequency = 4) # Quarterly
  indic <- ts(rnorm(8), start = c(2000, 1), frequency = 12) # Monthly

  result <- bridge(target = target, indic = indic)
  expect_no_error(S7::check_is_S7(result)) # Assuming you define a custom class for bridge output
})

test_that("bridge supports multiple indicators", {
  target <- ts(rnorm(10), start = c(2000, 1), frequency = 4) # Quarterly
  indic <- ts(cbind(a = rnorm(10*3 +1), b = rnorm(10*3+1)), start = c(2000, 1), frequency = 12) # Two monthly series

  result <- bridge(target = target, indic = indic)
  expect_equal(length(unique(result@indic$id)), 2) # Check the number of indicators
})

test_that("bridge supports custom aggregators", {
  target <- ts(rnorm(10), start = c(2000, 1), frequency = 4) # Quarterly
  indic <- ts(cbind(a = rnorm(10*3 +1), b = rnorm(10*3+1)), start = c(2000, 1), frequency = 12) # Two monthly series

  result <- bridge(target = target, indic = indic, indic_aggregators = c("last", "mean"))
  expect_true(all(!is.na(result@indic)))
})

test_that("bridge calculates correct forecasting horizon", {
  target <- ts(rnorm(10), start = c(2000, 1), frequency = 4) # Quarterly
  indic <- ts(cbind(a = rnorm(10*3 +1), b = rnorm(10*3+1)), start = c(2000, 1), frequency = 12) # Two monthly series

  result <- bridge(target = target, indic = indic, h = 2)
  expect_true(result@h == 2)
})

test_that("bridge handles mismatched frequencies", {
  target <- ts(c(1, 2, 3, 4), start = c(2000, 1), frequency = 4)
  indic <- ts(c(1, 2, 3), start = c(2000, 1), frequency = 1)

  expect_error(bridge(target = target, indic = indic), "@indic frequency in @indic must be the same as or higher than the @target frequency.")
})



