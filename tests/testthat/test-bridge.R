library(testthat)
library(bridgr)

test_that("bridge handles missing inputs gracefully", {
  expect_error(bridge(), "argument \"target\" is missing")
  expect_error(bridge(target = NULL, indic = NULL), "@target must be a ts-boxable object, not")
})

test_that("bridge validates input frequencies", {
  # Mock target and indicator time series
  target <- ts(c(1, 2, 1), start = c(2000, 1), frequency = 4) # Quarterly
  indic <- ts(rnorm(8), start = c(2000, 1), frequency = 12) # Monthly

  result <- bridge(target = target, indic = indic)
  expect_no_error(result) # Assuming you define a custom class for bridge output
})

test_that("bridge supports multiple indicators", {
  target <- ts(rnorm(10), start = c(2000, 1), frequency = 4) # Quarterly
  indic <- ts(cbind(a = rnorm(10*3 +1), b = rnorm(10*3+1)), start = c(2000, 1), frequency = 12) # Two monthly series

  result <- bridge(target = target, indic = indic)
  expect_equal(length(unique(result$indic$id)), 2) # Check the number of indicators
})

test_that("bridge supports custom aggregators", {
  target <- ts(rnorm(10), start = c(2000, 1), frequency = 4) # Quarterly
  indic <- ts(cbind(a = rnorm(10*3 +1), b = rnorm(10*3+1)), start = c(2000, 1), frequency = 12) # Two monthly series

  result <- bridge(target = target, indic = indic, indic_aggregators = c("last", "mean"))
  expect_true(all(!is.na(result$indic)))
})

test_that("bridge calculates correct forecasting horizon", {
  target <- ts(rnorm(10), start = c(2000, 1), frequency = 4) # Quarterly
  indic <- ts(cbind(a = rnorm(10*3 +1), b = rnorm(10*3+1)), start = c(2000, 1), frequency = 12) # Two monthly series

  result <- bridge(target = target, indic = indic, h = 2)
  expect_true(result$h == 2)
})

test_that("bridge handles mismatched frequencies", {
  target <- ts(rnorm(12), start = c(2000, 1), frequency = 12)
  indic <- ts(c(1, 2, 3,4), start = c(2000, 1), frequency = 4)

  expect_error(bridge(target = target, indic = indic))
})


test_that("bridge handles mismatched start dates", {
  target <- ts(rnorm(8), start = c(1999, 1), frequency = 4)
  indic <- ts(rnorm(15), start = c(2000, 1), frequency = 12)

  expect_message(bridge(target = target, indic = indic), "The start dates of the target and indicator variables do not match.")
})


test_that("bridge supports expalmon aggregator function", {
  target <- ts(rnorm(10), start = c(2000, 1), frequency = 4) # Quarterly
  indic <- ts(cbind(a = rnorm(10*3 +1), b = rnorm(10*3+1)), start = c(2000, 1), frequency = 12) # Two monthly series

  result <- bridge(target = target, indic = indic, indic_aggregators = c("expalmon", "expalmon"))
  expect_true(all(!is.na(result$indic)))
})


test_that("bridge supports custom aggregator function", {
  target <- ts(rnorm(10), start = c(2000, 1), frequency = 4) # Quarterly
  indic <- ts(cbind(a = rnorm(10*3 +1), b = rnorm(10*3+1)), start = c(2000, 1), frequency = 12) # Two monthly series

  result1 <- bridge(target = target, indic = indic, indic_aggregators = list(c(1/3, 1/3, 1/3), c(1/3, 1/3, 1/3)))
  result2 <- bridge(target = target, indic = indic) # Aggregator is mean by default

  expect_equal(result1$forecast_set, result2$forecast_set)
})


test_that("bridge allows additional indicator lags", {
  # Mock target and indicator time series
  target <- ts(rnorm(12), start = c(2000, 1), frequency = 4) # Quarterly
  indic <- ts(rnorm(38), start = c(2000, 1), frequency = 12) # Monthly

  result <- bridge(target = target, indic = indic, indic_lags = 1)
  expect_no_error(result) # Assuming you define a custom class for bridge output
})

test_that("bridge does not allow invalid frequency conversion names", {
  # Mock target and indicator time series
  target <- ts(rnorm(12), start = c(2000, 1), frequency = 4) # Quarterly
  indic <- ts(rnorm(38), start = c(2000, 1), frequency = 12) # Monthly

  expect_error(bridge(target = target, indic = indic,
                      frequency_conversions = c("dddpw" = 5))) # Assuming you define a custom class for bridge output
})


test_that("bridge summary works", {
  # # Mock target and indicator time series
  target <- ts(rnorm(12), start = c(2000, 1), frequency = 4) # Quarterly
  indic <- ts(rnorm(38), start = c(2000, 1), frequency = 12) # Monthly

  result <- bridge(target = target, indic = indic, indic_lags = 1)
  expect_no_error(summary(result)) # Assuming you define a custom class for bridge output
})

test_that("bridge forecast works", {
  # # Mock target and indicator time series
  target <- ts(rnorm(12), start = c(2000, 1), frequency = 4) # Quarterly
  indic <- ts(rnorm(38), start = c(2000, 1), frequency = 12) # Monthly

  result <- bridge(target = target, indic = indic, indic_lags = 1)
  expect_no_error(forecast(result)) # Assuming you define a custom class for bridge output
})
