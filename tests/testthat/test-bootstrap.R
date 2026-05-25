test_that("bootstrap helpers resolve defaults and valid index draws", {
  expect_equal(bridgr:::default_bootstrap_block_length(1), 1L)
  expect_equal(bridgr:::default_bootstrap_block_length(8), 2L)
  expect_equal(bridgr:::default_bootstrap_block_length(27), 3L)

  expect_equal(
    bridgr:::resolve_bootstrap_block_length(n_rows = 5, block_length = NULL),
    2L
  )
  expect_equal(
    bridgr:::resolve_bootstrap_block_length(n_rows = 5, block_length = 10),
    5L
  )

  indices <- bridgr:::circular_block_bootstrap_indices(
    n_rows = 7,
    block_length = 3
  )

  expect_length(indices, 7)
  expect_true(all(indices %in% seq_len(7)))
})

test_that("bootstrap intervals are computed from the draw distribution", {
  draws <- rbind(
    c(1, 10),
    c(2, 12),
    c(4, 14),
    c(8, 16)
  )

  intervals <- bridgr:::bootstrap_interval_matrices(
    draws = draws,
    level = c(80, 95),
    horizon = 2
  )

  expect_equal(intervals$se, apply(draws, 2, stats::sd))
  expect_equal(
    intervals$lower[, "80%"],
    apply(draws, 2, stats::quantile, probs = 0.10, type = 8)
  )
  expect_equal(
    intervals$upper[, "80%"],
    apply(draws, 2, stats::quantile, probs = 0.90, type = 8)
  )
  expect_equal(
    intervals$lower[, "95%"],
    apply(draws, 2, stats::quantile, probs = 0.025, type = 8)
  )
  expect_equal(
    intervals$upper[, "95%"],
    apply(draws, 2, stats::quantile, probs = 0.975, type = 8)
  )

  empty_intervals <- bridgr:::bootstrap_interval_matrices(
    draws = matrix(c(1, 2), nrow = 1),
    level = 80,
    horizon = 2
  )

  expect_true(all(is.na(empty_intervals$se)))
  expect_true(all(is.na(empty_intervals$lower)))
  expect_true(all(is.na(empty_intervals$upper)))
})

test_that("predictive_target_model_draw adds forecast shocks", {
  estimation_set <- dplyr::tibble(
    y = c(3, 5, 7, 9, 11, 13),
    x = c(1, 2, 3, 4, 5, 6)
  )
  model <- stats::lm(y ~ x, data = estimation_set)
  forecast_set <- dplyr::tibble(x = c(7, 8))
  expected_mean <- as.numeric(stats::predict(model, newdata = forecast_set))

  testthat::local_mocked_bindings(
    simulate_target_model_draws = function(model,
                                           forecast_set,
                                           target_name,
                                           regressor_names,
                                           target_lags = 0,
                                           target_history = NULL,
                                           n_paths = 100L,
                                           innovations = NULL) {
      matrix(expected_mean + 0.5, nrow = 1)
    },
    .package = "bridgr"
  )

  draw <- bridgr:::predictive_target_model_draw(
    model = model,
    forecast_set = forecast_set,
    target_name = "y",
    regressor_names = "x"
  )

  expect_equal(draw, expected_mean + 0.5)
})

test_that("bridge keeps full bootstrap opt-in for direct forecasts", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "direct",
    se = TRUE,
    full_system_bootstrap = TRUE,
    bootstrap = list(N = 5),
    h = 1
  )
  fc <- forecast(model)

  expect_true(model$bootstrap$enabled)
  expect_equal(model$bootstrap$N, 5L)
  expect_equal(
    model$bootstrap$block_length,
    ceiling(nrow(model$target)^(1 / 3))
  )
  expect_equal(model$uncertainty$coefficient_method, "block_bootstrap")
  expect_false(any(is.na(model$uncertainty$coefficient_se)))
  expect_equal(fc$bootstrap$block_length, model$bootstrap$block_length)
  expect_true(fc$uncertainty$enabled)
  expect_equal(fc$uncertainty$prediction_method, "block_bootstrap")
  expect_equal(fc$uncertainty$simulation_paths, model$bootstrap$valid_N)
  expect_equal(length(fc$se), nrow(fc$forecast_set))
  expect_false(any(grepl(
    "Indicator handling: direct alignment",
    capture.output(print(fc)),
    fixed = TRUE
  )))
})

test_that(
  "forecast.mf_model uses bootstrap scenario draws under full-system bootstrap",
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
    full_system_bootstrap = TRUE,
    bootstrap = list(N = 6, block_length = 3),
    h = 2
  )

  custom_xreg <- dplyr::tibble(
    id = rep(model$xreg_names, each = nrow(model$forecast_base_set)),
    time = rep(model$forecast_base_set$time, times = length(model$xreg_names)),
    value = c(
      model$forecast_base_set$indic + 3,
      model$forecast_base_set$indic_lag1 + 3
    )
  )

  scenario_fc <- forecast(model, xreg = custom_xreg)

  expect_equal(scenario_fc$uncertainty$prediction_method, "block_bootstrap")
  expect_equal(scenario_fc$bootstrap$enabled, TRUE)
  expect_equal(
    scenario_fc$uncertainty$simulation_paths,
    model$bootstrap$valid_N
  )
  expect_false(all(is.na(scenario_fc$se)))
  expect_equal(ncol(scenario_fc$lower), 2)
  expect_equal(ncol(scenario_fc$upper), 2)
  }
)

test_that(
  "build_prediction_uncertainty disables unavailable full bootstrap output",
  {
  result <- bridgr:::build_prediction_uncertainty(
    enabled = TRUE,
    model = NULL,
    forecast_set = dplyr::tibble(x = 1),
    target_name = "y",
    regressor_names = "x",
    target_lags = 0,
    target_history = NULL,
    bootstrap = list(N = 5L, block_length = 2L),
    full_system_bootstrap = TRUE,
    full_bootstrap = list(
      enabled = FALSE,
      prediction_draws = NULL,
      valid_N = 0L
    )
  )

  expect_false(result$enabled)
  expect_null(result$method)
  expect_null(result$draws)
  expect_equal(result$N, 0L)
  }
)

test_that("build_prediction_uncertainty warns when residual simulation fails", {
  model <- stats::lm(y ~ x, data = dplyr::tibble(y = 1:5, x = 1:5))
  forecast_set <- dplyr::tibble(x = c(6, 7))

  testthat::local_mocked_bindings(
    simulate_target_model_draws = function(...) {
      stop("simulation failed")
    },
    .package = "bridgr"
  )

  expect_warning(
    result <- bridgr:::build_prediction_uncertainty(
      enabled = TRUE,
      model = model,
      forecast_set = forecast_set,
      target_name = "y",
      regressor_names = "x",
      target_lags = 0,
      target_history = NULL,
      bootstrap = list(N = 5L, block_length = 2L),
      full_system_bootstrap = FALSE,
      full_bootstrap = list(enabled = FALSE)
    ),
    "Prediction intervals will be unavailable"
  )

  expect_false(result$enabled)
  expect_null(result$method)
  expect_null(result$draws)
  expect_equal(result$N, 0L)
})
