test_that("solver option normalization validates and rounds control values", {
  options <- bridgr:::normalize_parametric_solver_options(
    list(method = "BFGS", maxiter = 10.4, n_starts = 2.6, trace = 1.2, seed = 9.7)
  )

  expect_equal(options$method, "BFGS")
  expect_equal(options$maxiter, 10L)
  expect_equal(options$n_starts, 3L)
  expect_equal(options$trace, 1L)
  expect_equal(options$seed, 10L)

  expect_error(
    bridgr:::normalize_parametric_solver_options(list(method = "CG")),
    "should be one of"
  )
  expect_error(
    bridgr:::normalize_parametric_solver_options(list(maxiter = 0)),
    "must be a single integer >= 1"
  )
  expect_error(
    bridgr:::normalize_parametric_solver_options(list(n_starts = 0)),
    "must be a single integer >= 1"
  )
  expect_error(
    bridgr:::normalize_parametric_solver_options(list(trace = -1)),
    "must be a single integer >= 0"
  )
  expect_error(
    bridgr:::normalize_parametric_solver_options(list(seed = c(1, 2))),
    "must be `NULL` or a single finite number"
  )
})

test_that("parametric start values are validated by type, names, and constraints", {
  parametric_specs <- list(
    a = list(aggregator = "beta"),
    b = list(aggregator = "legendre")
  )

  expect_error(
    bridgr:::validate_parametric_solver_start(
      solver_options = list(start_values = "bad"),
      parametric_specs = parametric_specs
    ),
    "must be a numeric vector or a named list"
  )
  expect_error(
    bridgr:::validate_parametric_solver_start(
      solver_options = list(start_values = list(c(2, 3), c(0, 0))),
      parametric_specs = parametric_specs
    ),
    "must be named for the parametric indicators"
  )
  expect_error(
    bridgr:::validate_parametric_solver_start(
      solver_options = list(start_values = list(a = c(2, 3), c = c(0, 0))),
      parametric_specs = parametric_specs
    ),
    "Unknown entries in `solver_options\\$start_values`: c"
  )
  expect_error(
    bridgr:::validate_parametric_solver_start(
      solver_options = list(start_values = list(a = c(2, 3))),
      parametric_specs = parametric_specs
    ),
    "must provide exactly 2 parametric indicator start vectors"
  )
  expect_error(
    bridgr:::validate_parametric_solver_start(
      solver_options = list(start_values = list(a = c(0, 3), b = c(0, 0))),
      parametric_specs = parametric_specs
    ),
    "must be strictly positive for `beta` aggregation"
  )
})

test_that("month frequency detection distinguishes month, quarter, and year steps", {
  expect_equal(
    bridgr:::detect_month_frequency(
      seq(as.Date("2020-01-01"), by = "month", length.out = 4)
    ),
    list(unit = "month", step = 1)
  )
  expect_equal(
    bridgr:::detect_month_frequency(
      seq(as.Date("2020-01-01"), by = "quarter", length.out = 4)
    ),
    list(unit = "quarter", step = 1)
  )
  expect_equal(
    bridgr:::detect_month_frequency(
      seq(as.Date("2020-01-01"), by = "year", length.out = 4)
    ),
    list(unit = "year", step = 1)
  )
})

test_that("observations per target period requires integer frequency alignment", {
  defaults <- bridgr:::default_frequency_conversions()

  expect_equal(
    bridgr:::observations_per_target_period(
      indicator_meta = tibble::tibble(id = "x", unit = "month", step = 1),
      target_meta = tibble::tibble(id = "target", unit = "quarter", step = 1),
      frequency_conversions = defaults
    ),
    3L
  )

  expect_error(
    bridgr:::observations_per_target_period(
      indicator_meta = tibble::tibble(id = "x", unit = "month", step = 2),
      target_meta = tibble::tibble(id = "target", unit = "quarter", step = 1),
      frequency_conversions = defaults
    ),
    "not aligned under the supplied `frequency_conversions`"
  )
})

test_that("shift_time preserves calendar-end behavior across larger units", {
  expect_equal(
    bridgr:::shift_time(as.Date("2020-01-31"), 1, "month"),
    as.Date("2020-02-29")
  )
  expect_equal(
    bridgr:::shift_time(as.Date("2020-01-31"), 1, "quarter"),
    as.Date("2020-04-30")
  )
  expect_equal(
    bridgr:::shift_time(as.Date("2020-02-29"), 1, "year"),
    as.Date("2021-02-28")
  )
  expect_error(
    bridgr:::shift_time(as.Date("2020-01-31"), 1, "fortnight"),
    "Unsupported unit `fortnight`"
  )
})
