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

  testthat::local_mocked_bindings(
    simulate_target_model_draws = function(model,
                                           forecast_set,
                                           target_name,
                                           regressor_names,
                                           target_lags = 0,
                                           target_history = NULL,
                                           n_paths = 100L,
                                           innovations = NULL) {
      matrix(
        bridgr:::forecast_target_model_mean(
          model = model,
          forecast_set = forecast_set,
          target_name = target_name,
          regressor_names = regressor_names
        ) + 0.5,
        nrow = 1
      )
    },
    .package = "bridgr"
  )

  draw <- bridgr:::predictive_target_model_draw(
    model = model,
    forecast_set = forecast_set,
    target_name = "y",
    regressor_names = "x"
  )

  expect_equal(
    draw,
    bridgr:::forecast_target_model_mean(
      model = model,
      forecast_set = forecast_set,
      target_name = "y",
      regressor_names = "x"
    ) + 0.5
  )
})

test_that(
  "bootstrap_target_equation keeps valid resamples and warns on losses",
  {
  estimation_set <- dplyr::tibble(
    y = c(3, 5, 7, 9, 11, 13),
    x = c(1, 2, 3, 4, 5, 6)
  )
  forecast_set <- dplyr::tibble(x = c(7, 8))
  base_model <- stats::lm(y ~ x, data = estimation_set)
  draw_counter <- 0L

  testthat::local_mocked_bindings(
    circular_block_bootstrap_indices = function(n_rows, block_length) {
      seq_len(n_rows)
    },
    fit_target_model = function(estimation_set,
                                target_name,
                                regressor_names,
                                formula,
                                target_lags) {
      draw_counter <<- draw_counter + 1L
      if (draw_counter == 2L) {
        stop("draw failed")
      }
      stats::lm(formula = formula, data = estimation_set)
    },
    predictive_target_model_draw = function(model,
                                            forecast_set,
                                            target_name,
                                            regressor_names) {
      rep(draw_counter, nrow(forecast_set))
    },
    .package = "bridgr"
  )

  expect_warning(
    result <- bridgr:::bootstrap_target_equation(
      enabled = TRUE,
      model = base_model,
      estimation_set = estimation_set,
      forecast_set = forecast_set,
      target_name = "y",
      regressor_names = "x",
      formula = y ~ x,
      target_lags = 0,
      bootstrap = list(N = 3L, block_length = 2L)
    ),
    "produced 2 valid draws out of 3"
  )

  expect_true(result$enabled)
  expect_equal(result$valid_N, 2L)
  expect_equal(result$block_length, 2L)
  expect_equal(dim(result$coefficient_draws), c(2, 2))
  expect_equal(dim(result$forecast_draws), c(2, 2))
  expect_equal(
    result$forecast_draws,
    matrix(c(1, 1, 3, 3), nrow = 2, byrow = TRUE)
  )
  expect_length(result$models, 2)
  }
)

test_that(
  "bootstrap_target_equation disables uncertainty when all draws fail",
  {
  estimation_set <- dplyr::tibble(
    y = c(3, 5, 7, 9, 11, 13),
    x = c(1, 2, 3, 4, 5, 6)
  )
  forecast_set <- dplyr::tibble(x = c(7, 8))
  base_model <- stats::lm(y ~ x, data = estimation_set)

  testthat::local_mocked_bindings(
    circular_block_bootstrap_indices = function(n_rows, block_length) {
      seq_len(n_rows)
    },
    fit_target_model = function(estimation_set,
                                target_name,
                                regressor_names,
                                formula,
                                target_lags) {
      stop("draw failed")
    },
    .package = "bridgr"
  )

  expect_warning(
    result <- bridgr:::bootstrap_target_equation(
      enabled = TRUE,
      model = base_model,
      estimation_set = estimation_set,
      forecast_set = forecast_set,
      target_name = "y",
      regressor_names = "x",
      formula = y ~ x,
      target_lags = 0,
      bootstrap = list(N = 4L, block_length = NULL)
    ),
    "failed for every resample"
  )

  expect_false(result$enabled)
  expect_equal(result$valid_N, 0L)
  expect_equal(result$block_length, 2L)
  expect_null(result$coefficient_draws)
  expect_null(result$forecast_draws)
  expect_null(result$models)
  }
)

test_that("bridge keeps full bootstrap opt-in for direct forecasts", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- bridge(
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
