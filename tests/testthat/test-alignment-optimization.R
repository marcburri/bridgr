test_that("direct alignment reuses complete blocks", {
  indicator_tbl <- dplyr::tibble(
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

test_that("mean extension uses the latest available high-frequency block", {
  target <- dplyr::tibble(
    time = as.Date(c("2020-01-01", "2020-04-01", "2020-07-01")),
    value = c(1, 2, 3)
  )
  indic <- dplyr::tibble(
    time = as.Date(c(
      "2020-01-01", "2020-02-01", "2020-03-01",
      "2020-04-01", "2020-05-01", "2020-06-01",
      "2020-07-01", "2020-08-01", "2020-09-01",
      "2020-10-01"
    )),
    value = c(5, 5, 5, 10, 10, 10, 20, 20, 20, 1000)
  )

  target_tbl <- bridgr:::as_bridge_tbl(target, "target", "target")
  indic_tbl <- bridgr:::as_bridge_tbl(indic, "indic", "indic")
  target_meta <- bridgr:::infer_frequency_table(target_tbl)$target
  indic_meta <- bridgr:::infer_frequency_table(indic_tbl)$indicators

  extension <- bridgr:::extend_indicator_series(
    indicator_tbl = indic_tbl,
    indicator_id = "indic",
    indicator_meta = indic_meta,
    target_meta = target_meta,
    target_anchor = min(target$time),
    future_target_times = as.Date("2020-10-01"),
    obs_per_target = 3,
    predict_method = "mean"
  )

  expect_equal(
    utils::tail(extension$data$values, 2),
    c(346.667, 346.667),
    tolerance = 1e-6
  )
})

test_that(
  "direct bridge alignment ignores supplied aggregation settings",
  {
  indic_a <- dplyr::tibble(
    id = "a",
    time = seq(as.Date("2020-01-01"), by = "month", length.out = 16),
    value = seq_len(16)
  )
  indic_b <- dplyr::tibble(
    id = "b",
    time = seq(as.Date("2020-01-01"), by = "month", length.out = 16),
    value = 100 + seq_len(16)
  )
  indic <- rbind(indic_a, indic_b)
  target <- dplyr::tibble(
    time = seq(as.Date("2020-01-01"), by = "quarter", length.out = 6),
    value = seq_len(6)
  )
  weights <- c(0.2, 0.3, 0.5)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = c("direct", "direct"),
    indic_aggregators = list(weights, "unrestricted"),
    h = 1
  )

  expect_lt(nrow(model$estimation_set), nrow(target))
  expect_equal(model$forecast_set$a[[1]], mean(c(14, 15, 16)))
  expect_equal(model$forecast_set$b[[1]], mean(c(114, 115, 116)))
  expect_false(any(grepl("^b_hf", names(model$forecast_set))))
  }
)

test_that("target period helpers respect mixed-frequency calendar boundaries", {
  monthly_times <- seq(as.Date("2020-01-01"), by = "month", length.out = 5)
  quarterly_meta <- dplyr::tibble(
    id = "target",
    unit = "quarter",
    step = 1
  )

  expect_equal(
    bridgr:::compute_target_periods(
      times = monthly_times,
      target_anchor = as.Date("2020-01-01"),
      target_meta = quarterly_meta
    ),
    as.Date(c(
      "2020-01-01", "2020-01-01", "2020-01-01",
      "2020-04-01", "2020-04-01"
    ))
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
      target_meta = dplyr::tibble(
        id = "target",
        unit = "month",
        step = 1
      ),
      h = 3
    ),
    as.Date(c("2020-02-29", "2020-03-31", "2020-04-30"))
  )
})

test_that("bridge accepts end-of-period dates via fallback", {
  indic <- dplyr::tibble(
    time = lubridate::ceiling_date(
      seq(as.Date("2020-01-01"), by = "month", length.out = 15),
      unit = "month"
    ) - lubridate::days(1),
    value = seq_len(15)
  )
  target <- dplyr::tibble(
    time = lubridate::ceiling_date(
      seq(as.Date("2020-01-01"), by = "quarter", length.out = 5),
      unit = "quarter"
    ) - lubridate::days(1),
    value = c(2, 5.2, 8, 11, 14)
  )

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_aggregators = "mean",
    h = 1
  )

  expect_equal(
    model$target$time,
    as.Date(c(
      "2020-01-01", "2020-04-01", "2020-07-01",
      "2020-10-01", "2021-01-01"
    ))
  )
  expect_equal(
    utils::head(model$indic$time, 3),
    as.Date(c("2020-01-01", "2020-02-01", "2020-03-01"))
  )
})

test_that("optimizer warning keeps the best non-converged start", {
  parametric_specs <- list(
    x = list(
      indicator_id = "x",
      aggregator = "beta",
      periods = seq(as.Date("2020-01-01"), by = "quarter", length.out = 4),
      blocks = matrix(seq_len(12), nrow = 4, byrow = TRUE),
      obs_per_target = 3
    )
  )
  target_tbl <- dplyr::tibble(
    id = "target",
    time = seq(as.Date("2020-01-01"), by = "quarter", length.out = 4),
    values = c(1, 2, 3, 4)
  )
  call_index <- 0L

  testthat::local_mocked_bindings(
    run_parametric_optimizer = function(objective,
                                        gradient,
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
      fixed_aggregated = dplyr::tibble(
        id = character(),
        time = as.Date(character()),
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

test_that(
  "mf_model can suppress convergence warnings while keeping metadata",
  {
    indic <- make_monthly_indicator(n = 36)
    target <- make_quarter_target(indic, n_quarters = 12)

    testthat::local_mocked_bindings(
      run_parametric_optimizer = function(objective,
                                          gradient,
                                          start,
                                          lower,
                                          upper,
                                          solver_options) {
        list(
          par = start,
          value = 0,
          convergence = 1L,
          message = "stalled",
          method = solver_options$method
        )
      },
      .package = "bridgr"
    )

    expect_no_warning(
      model <- mf_model(
        target = target,
        indic = indic,
        indic_predict = "last",
        indic_aggregators = "beta",
        solver_options = list(
          method = "BFGS",
          n_starts = 1L,
          maxiter = 5L,
          warn = FALSE,
          start_values = c(2, 3)
        ),
        h = 1
      )
    )

    expect_equal(model$parametric_optimization$convergence, 1L)
    expect_equal(model$parametric_optimization$message, "stalled")
    expect_equal(model$parametric_optimization$method, "BFGS")
  }
)

test_that("optimizer aborts when every candidate is non-finite", {
  parametric_specs <- list(
    x = list(
      indicator_id = "x",
      aggregator = "expalmon",
      periods = seq(as.Date("2020-01-01"), by = "quarter", length.out = 4),
      blocks = matrix(seq_len(12), nrow = 4, byrow = TRUE),
      obs_per_target = 3
    )
  )
  target_tbl <- dplyr::tibble(
    id = "target",
    time = seq(as.Date("2020-01-01"), by = "quarter", length.out = 4),
    values = c(1, 2, 3, 4)
  )

  testthat::local_mocked_bindings(
    run_parametric_optimizer = function(objective,
                                        gradient,
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
      fixed_aggregated = dplyr::tibble(
        id = character(),
        time = as.Date(character()),
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

test_that("unrestricted warning depends on predictor density", {
  monthly_indicator <- make_monthly_indicator(n = 120)
  target <- make_quarter_target(monthly_indicator, n_quarters = 40)

  expect_no_warning(
    mf_model(
      target = target,
      indic = monthly_indicator,
      indic_predict = "last",
      indic_aggregators = "unrestricted",
      h = 1
    )
  )

  multi_indic <- make_multi_indicator(n = 120)
  target_multi <- make_quarter_target(
      monthly_indicator = subset(
        multi_indic,
        id == "a",
        select = c("time", "value")
      ),
    n_quarters = 40
  )

  expect_warning(
    mf_model(
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

test_that("named start_values flow through mixed aggregators", {
  base_indicator <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(base_indicator, n_quarters = 12)
  indic <- rbind(
    dplyr::tibble(
      id = "a",
      time = base_indicator$time,
      value = base_indicator$value
    ),
    dplyr::tibble(
      id = "b",
      time = base_indicator$time,
      value = base_indicator$value + 10
    )
  )
  start_values <- list(
    a = c(0.1, 0.2),
    b = c(2, 3)
  )

  testthat::local_mocked_bindings(
    run_parametric_optimizer = function(objective,
                                        gradient,
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

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = c("last", "last"),
    indic_aggregators = c("expalmon", "beta"),
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
  expect_equal(model$parametric_optimization$best_start, 1L)
  expect_true(all(c("a", "b") %in% names(model$parametric_weights)))
})

test_that("beta aggregation stays positive under BFGS", {
  indic <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indic, n_quarters = 12)

  model <- mf_model(
    target = target,
    indic = indic,
    indic_predict = "last",
    indic_aggregators = "beta",
    solver_options = list(
      method = "BFGS",
      n_starts = 1,
      maxiter = 50,
      start_values = c(2, 3)
    ),
    h = 1
  )

  expect_true(all(model$parametric_parameters$indic > 0))
  }
)
