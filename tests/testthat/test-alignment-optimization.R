test_that("direct alignment reuses the latest complete blocks and drops incomplete tails", {
  indicator_tbl <- data.frame(
    id = "x",
    time = seq(as.Date("2020-01-01"), by = "day", length.out = 17),
    values = seq_len(17)
  )
  target_times <- seq(as.Date("2020-01-06"), by = "week", length.out = 4)

  direct_blocks <- bridgr:::prepare_indicator_direct_blocks(
    indicator_tbl = indicator_tbl,
    indicator_id = "x",
    target_times = target_times,
    obs_per_target = 7
  )

  expect_equal(direct_blocks$periods, target_times[3:4])
  expect_equal(direct_blocks$blocks[1, ], 4:10)
  expect_equal(direct_blocks$blocks[2, ], 11:17)
  expect_equal(direct_blocks$truncation$n_periods, 0)
})

test_that("direct bridge alignment works with numeric and unrestricted aggregation together", {
  indic_a <- data.frame(
    id = "a",
    time = seq(as.Date("2020-01-01"), by = "month", length.out = 16),
    value = seq_len(16)
  )
  indic_b <- data.frame(
    id = "b",
    time = seq(as.Date("2020-01-01"), by = "month", length.out = 16),
    value = 100 + seq_len(16)
  )
  indic <- rbind(indic_a, indic_b)
  target <- data.frame(
    time = seq(as.Date("2020-01-01"), by = "quarter", length.out = 6),
    value = seq_len(6)
  )
  weights <- c(0.2, 0.3, 0.5)

  model <- suppressWarnings(bridge(
    target = target,
    indic = indic,
    indic_predict = c("direct", "direct"),
    indic_aggregators = list(weights, "unrestricted"),
    h = 1
  ))

  expect_lt(nrow(model$estimation_set), nrow(target))
  expect_equal(model$forecast_set$a[[1]], sum(weights * c(14, 15, 16)))
  expect_equal(model$forecast_set$b_hf1[[1]], 114)
  expect_equal(model$forecast_set$b_hf2[[1]], 115)
  expect_equal(model$forecast_set$b_hf3[[1]], 116)
})

test_that("target period helpers respect mixed-frequency calendar boundaries", {
  monthly_times <- seq(as.Date("2020-01-01"), by = "month", length.out = 5)
  quarterly_meta <- tibble::tibble(id = "target", unit = "quarter", step = 1)

  expect_equal(
    bridgr:::compute_target_periods(
      times = monthly_times,
      target_anchor = as.Date("2020-01-01"),
      target_meta = quarterly_meta
    ),
    as.Date(c("2020-01-01", "2020-01-01", "2020-01-01", "2020-04-01", "2020-04-01"))
  )
  expect_equal(
    bridgr:::unit_distance(
      times = as.Date(c("2020-01-01", "2020-04-01", "2020-07-01")),
      origin = as.Date("2020-01-01"),
      unit = "quarter"
    ),
    c(0, 1, 2)
  )
  expect_equal(
    bridgr:::target_future_times(
      last_time = as.Date("2020-01-31"),
      target_meta = tibble::tibble(id = "target", unit = "month", step = 1),
      h = 3
    ),
    as.Date(c("2020-02-29", "2020-03-31", "2020-04-30"))
  )
})

test_that("optimizer warning path keeps the best available non-converged start", {
  parametric_specs <- list(
    x = list(
      indicator_id = "x",
      aggregator = "beta",
      periods = seq(as.Date("2020-01-01"), by = "quarter", length.out = 4),
      blocks = matrix(seq_len(12), nrow = 4, byrow = TRUE),
      obs_per_target = 3
    )
  )
  target_tbl <- tibble::tibble(
    id = "target",
    time = seq(as.Date("2020-01-01"), by = "quarter", length.out = 4),
    values = c(1, 2, 3, 4)
  )
  call_index <- 0L

  testthat::local_mocked_bindings(
    run_parametric_optimizer = function(objective,
                                        start,
                                        lower,
                                        upper,
                                        solver_options) {
      call_index <<- call_index + 1L
      list(
        par = start,
        value = c(10, 2)[[call_index]],
        convergence = 1L,
        message = "stalled",
        method = solver_options$method
      )
    },
    .package = "bridgr"
  )

  expect_warning(
    result <- bridgr:::optimize_parametric_weights(
      parametric_specs = parametric_specs,
      fixed_aggregated = tibble::tibble(
        id = character(),
        time = target_tbl$time[0],
        values = numeric()
      ),
      target_tbl = target_tbl,
      target_name = "target",
      indic_lags = 0,
      target_lags = 0,
      solver_options = list(
        method = "BFGS",
        maxiter = 5L,
        n_starts = 2L,
        seed = 123L,
        trace = 0L,
        start_values = list(x = c(2, 3))
      )
    ),
    "did not fully converge"
  )

  expect_equal(result$optimization$best_start, 2L)
  expect_equal(result$optimization$n_starts, 2L)
  expect_equal(result$optimization$convergence, 1L)
  expect_equal(result$optimization$message, "stalled")
  expect_equal(length(result$weights$x), 3)
})

test_that("optimizer aborts when every candidate has a non-finite objective value", {
  parametric_specs <- list(
    x = list(
      indicator_id = "x",
      aggregator = "expalmon",
      periods = seq(as.Date("2020-01-01"), by = "quarter", length.out = 4),
      blocks = matrix(seq_len(12), nrow = 4, byrow = TRUE),
      obs_per_target = 3
    )
  )
  target_tbl <- tibble::tibble(
    id = "target",
    time = seq(as.Date("2020-01-01"), by = "quarter", length.out = 4),
    values = c(1, 2, 3, 4)
  )

  testthat::local_mocked_bindings(
    run_parametric_optimizer = function(objective,
                                        start,
                                        lower,
                                        upper,
                                        solver_options) {
      list(
        par = start,
        value = Inf,
        convergence = 1L,
        message = "failed",
        method = solver_options$method
      )
    },
    .package = "bridgr"
  )

  expect_error(
    bridgr:::optimize_parametric_weights(
      parametric_specs = parametric_specs,
      fixed_aggregated = tibble::tibble(
        id = character(),
        time = target_tbl$time[0],
        values = numeric()
      ),
      target_tbl = target_tbl,
      target_name = "target",
      indic_lags = 0,
      target_lags = 0,
      solver_options = list(
        method = "BFGS",
        maxiter = 5L,
        n_starts = 1L,
        seed = NULL,
        trace = 0L,
        start_values = list(x = c(0, 0))
      )
    ),
    "failed to find a finite objective value"
  )
})

test_that("unrestricted warning depends on predictor density after lags and multiple indicators", {
  monthly_indicator <- make_monthly_indicator(n = 120)
  target <- make_quarter_target(monthly_indicator, n_quarters = 40)

  expect_no_warning(
    bridge(
      target = target,
      indic = monthly_indicator,
      indic_predict = "last",
      indic_aggregators = "unrestricted",
      h = 1
    )
  )

  multi_indic <- make_multi_indicator(n = 120)
  target_multi <- make_quarter_target(
    monthly_indicator = subset(multi_indic, id == "a", select = c("time", "value")),
    n_quarters = 40
  )

  expect_warning(
    bridge(
      target = target_multi,
      indic = multi_indic,
      indic_predict = c("last", "last"),
      indic_aggregators = c("unrestricted", "unrestricted"),
      indic_lags = 1,
      h = 1
    ),
    "10-observations-per-predictor guideline"
  )
})

test_that("named start_values flow through mixed parametric aggregators end to end", {
  base_indicator <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(base_indicator, n_quarters = 12)
  indic <- rbind(
    data.frame(id = "a", time = base_indicator$time, value = base_indicator$value),
    data.frame(id = "b", time = base_indicator$time, value = base_indicator$value + 10),
    data.frame(id = "c", time = base_indicator$time, value = base_indicator$value - 5)
  )
  start_values <- list(
    a = c(0.1, 0.2),
    b = c(2, 3),
    c = c(-0.4, 0.6)
  )

  testthat::local_mocked_bindings(
    run_parametric_optimizer = function(objective,
                                        start,
                                        lower,
                                        upper,
                                        solver_options) {
      list(
        par = start,
        value = 0,
        convergence = 0L,
        message = "",
        method = solver_options$method
      )
    },
    .package = "bridgr"
  )

  model <- bridge(
    target = target,
    indic = indic,
    indic_predict = c("last", "last", "last"),
    indic_aggregators = c("expalmon", "beta", "legendre"),
    solver_options = list(
      start_values = start_values,
      method = "BFGS",
      n_starts = 1,
      maxiter = 5
    ),
    h = 1
  )

  expect_equal(model$parametric_parameters$a, start_values$a)
  expect_equal(model$parametric_parameters$b, start_values$b)
  expect_equal(model$parametric_parameters$c, start_values$c)
  expect_equal(model$parametric_optimization$best_start, 1L)
  expect_true(all(c("a", "b", "c") %in% names(model$parametric_weights)))
})
