test_that("solver option normalization validates controls", {
  defaults <- bridgr:::normalize_parametric_solver_options(NULL)
  options <- bridgr:::normalize_parametric_solver_options(
    list(
      method = "BFGS",
      maxiter = 10.4,
      n_starts = 2.6,
      trace = 1.2,
      warn = FALSE,
      seed = 9.7
    )
  )

  expect_equal(defaults$reltol, 1e-8)
  expect_equal(defaults$warn, TRUE)
  expect_equal(options$method, "BFGS")
  expect_equal(options$maxiter, 10L)
  expect_equal(options$n_starts, 3L)
  expect_equal(options$trace, 1L)
  expect_equal(options$warn, FALSE)
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
    bridgr:::normalize_parametric_solver_options(list(warn = NA)),
    "`solver_options\\$warn` must be either `TRUE` or `FALSE`"
  )
  expect_error(
    bridgr:::normalize_parametric_solver_options(list(seed = c(1, 2))),
    "must be `NULL` or a single finite number"
  )
  expect_error(
    bridgr:::normalize_parametric_solver_options(list(reltol = 0)),
    "must be a single finite number > 0"
  )
})

test_that("bootstrap controls are normalized and validated", {
  options <- bridgr:::normalize_bridge_bootstrap(
    list(N = 25.2, block_length = 4.8)
  )

  expect_equal(options$N, 25L)
  expect_equal(options$block_length, 5L)

  expect_error(
    bridgr:::normalize_bridge_bootstrap("bad"),
    "`bootstrap` must be a list"
  )
  expect_error(
    bridgr:::normalize_bridge_bootstrap(list(N = 0)),
    "`bootstrap\\$N` must be a single integer >= 1"
  )
  expect_error(
    bridgr:::normalize_bridge_bootstrap(list(block_length = 0)),
    "`bootstrap\\$block_length` must be `NULL` or a single integer >= 1"
  )
})

test_that("default series ids are scalarized before assignment", {
  input <- dplyr::tibble(
    time = as.Date("2020-01-01") + 0:2,
    value = 1:3
  )

  output <- bridgr:::as_bridge_tbl(
    x = input,
    arg = "target",
    default_id = c("very", "long", "expression")
  )

  expect_equal(output$id, rep("very long expression", 3))
})

test_that(
  "list-valued data columns fail with an informative preprocessing error",
  {
  input <- dplyr::tibble(
    time = as.Date("2020-01-01") + 0:2,
    value = I(list(1, 2, 3))
  )

  expect_error(
    bridgr:::as_bridge_tbl(
      x = input,
      arg = "target",
      default_id = "target"
    ),
    "list columns are not supported"
  )
  }
)

test_that("argument labels fall back cleanly for literal objects", {
  expect_equal(
    bridgr:::bridge_argument_label(quote(example_series), "target"),
    "example_series"
  )
  expect_equal(
    bridgr:::bridge_argument_label(quote(pkg::example_series), "target"),
    "pkg::example_series"
  )
  expect_equal(
    bridgr:::bridge_argument_label(dplyr::tibble(x = 1), "target"),
    "target"
  )
  expect_equal(
    bridgr:::bridge_argument_label(
      quote(base::quote(list(time = 1, value = 2))),
      "target"
    ),
    "target"
  )
})

test_that("indicator methods and aggregators are matched case-insensitively", {
  expect_equal(
    bridgr:::normalize_indicator_methods(
      methods = c("AUTO.ARIMA", "Direct"),
      n_series = 2,
      default = "auto.arima",
      arg = "indic_predict",
      valid = c("mean", "last", "auto.arima", "ets", "direct")
    ),
    c("auto.arima", "direct")
  )

  expect_equal(
    bridgr:::normalize_indicator_aggregators(
      aggregators = c("MEAN", "BeTa"),
      n_series = 2
    ),
    list("mean", "beta")
  )
})

test_that("parametric start values are validated", {
  parametric_specs <- list(
    a = list(aggregator = "beta"),
    b = list(aggregator = "expalmon")
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

test_that("month frequency detection distinguishes calendar steps", {
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

test_that("period-end dates are standardized to period starts", {
  monthly_end <- dplyr::tibble(
    id = "x",
    time = lubridate::ceiling_date(
      seq(as.Date("2020-01-01"), by = "month", length.out = 4),
      unit = "month"
    ) - lubridate::days(1),
    values = 1:4
  )
  quarter_end <- dplyr::tibble(
    id = "x",
    time = lubridate::ceiling_date(
      seq(as.Date("2020-01-01"), by = "quarter", length.out = 4),
      unit = "quarter"
    ) - lubridate::days(1),
    values = 1:4
  )

  expect_equal(
    bridgr:::normalize_period_start_data(monthly_end)$time,
    as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01"))
  )
  expect_equal(
    bridgr:::normalize_period_start_data(quarter_end)$time,
    as.Date(c("2020-01-01", "2020-04-01", "2020-07-01", "2020-10-01"))
  )
})

test_that("observations per target period requires integer alignment", {
  defaults <- bridgr:::default_frequency_conversions()

  expect_equal(
    bridgr:::observations_per_target_period(
      indicator_meta = dplyr::tibble(
        id = "x",
        unit = "month",
        step = 1
      ),
      target_meta = dplyr::tibble(
        id = "target",
        unit = "quarter",
        step = 1
      ),
      frequency_conversions = defaults
    ),
    3L
  )

  expect_error(
    bridgr:::observations_per_target_period(
      indicator_meta = dplyr::tibble(
        id = "x",
        unit = "month",
        step = 2
      ),
      target_meta = dplyr::tibble(
        id = "target",
        unit = "quarter",
        step = 1
      ),
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

test_that("beta parameters are mapped to and from optimizer scale positively", {
  expect_equal(
    bridgr:::to_optimizer_scale(c(2, 3), "beta"),
    log(c(2, 3))
  )
  expect_equal(
    bridgr:::from_optimizer_scale(log(c(2, 3)), "beta"),
    c(2, 3)
  )
})

test_that("removed parametric aggregators are rejected by helper functions", {
  expect_error(
    bridgr:::parametric_parameter_names("legendre"),
    "Unsupported parametric aggregator `legendre`"
  )
  expect_error(
    bridgr:::parametric_polynomial_basis("beta", c(1, 1), 3),
    "Unsupported polynomial aggregator `beta`"
  )
})

finite_difference_jacobian <- function(fun, x, eps = 1e-6) {
  baseline <- fun(x)
  jacobian <- matrix(NA_real_, nrow = length(baseline), ncol = length(x))

  for (index in seq_along(x)) {
    step <- rep(0, length(x))
    step[[index]] <- eps
    jacobian[, index] <- (
      fun(x + step) - fun(x - step)
    ) / (2 * eps)
  }

  jacobian
}

finite_difference_gradient <- function(fun, x, eps = 1e-6) {
  gradient <- numeric(length(x))

  for (index in seq_along(x)) {
    step <- rep(0, length(x))
    step[[index]] <- eps
    gradient[[index]] <- (fun(x + step) - fun(x - step)) / (2 * eps)
  }

  gradient
}

parametric_gradient_fixture <- function(aggregator) {
  indicator <- make_monthly_indicator(n = 36)
  target <- make_quarter_target(indicator, n_quarters = 12)
  target_tbl <- bridgr:::as_bridge_tbl(
    target,
    arg = "target",
    default_id = "target"
  )
  indicator_tbl <- bridgr:::as_bridge_tbl(
    indicator,
    arg = "indic",
    default_id = "indic"
  )
  target_meta <- bridgr:::infer_frequency_table(target_tbl)$target
  indicator_meta <- bridgr:::infer_frequency_table(indicator_tbl)$indicators
  aligned <- bridgr:::align_bridge_inputs(
    target_tbl = target_tbl,
    indic_tbl = indicator_tbl,
    target_meta = target_meta,
    indic_meta = indicator_meta
  )
  obs_per_target <- bridgr:::observations_per_target_period(
    indicator_meta = indicator_meta,
    target_meta = target_meta,
    frequency_conversions = bridgr:::default_frequency_conversions()
  )
  period_blocks <- bridgr:::prepare_indicator_period_blocks(
    indicator_tbl = aligned$indic,
    indicator_id = "indic",
    target_meta = target_meta,
    target_anchor = aligned$target_anchor,
    obs_per_target = obs_per_target
  )
  parametric_specs <- list(
    indic = list(
      indicator_id = "indic",
      aggregator = aggregator,
      periods = period_blocks$periods,
      blocks = period_blocks$blocks,
      obs_per_target = obs_per_target
    )
  )
  fixed_aggregated <- dplyr::tibble(
    id = character(),
    time = aligned$target$time[0],
    values = numeric()
  )
  parameters <- switch(
    aggregator,
    expalmon = c(0.25, -0.15),
    beta = bridgr:::to_optimizer_scale(c(2.2, 3.4), "beta")
  )

  list(
    parameters = parameters,
    parametric_specs = parametric_specs,
    fixed_aggregated = fixed_aggregated,
    target_tbl = aligned$target,
    target_name = "target",
    indic_lags = 1L,
    target_lags = 1L
  )
}

test_that("parametric weight gradients match finite differences", {
  cases <- list(
    expalmon = list(parameters = c(0.3, -0.2), n_weights = 5L),
    beta = list(parameters = c(2.4, 3.1), n_weights = 5L)
  )

  for (aggregator in names(cases)) {
    params <- cases[[aggregator]]$parameters
    n_weights <- cases[[aggregator]]$n_weights

    analytic <- bridgr:::parametric_weight_gradient(
      aggregator = aggregator,
      parameters = params,
      n_weights = n_weights
    )
    numeric <- finite_difference_jacobian(
      fun = function(current_parameters) {
        bridgr:::parametric_weights(
          aggregator = aggregator,
          parameters = current_parameters,
          n_weights = n_weights
        )
      },
      x = params
    )

    expect_equal(
      analytic,
      numeric,
      tolerance = 1e-5,
      info = paste("aggregator =", aggregator)
    )
  }
})

test_that("parametric objective gradients match finite differences", {
  for (aggregator in c("expalmon", "beta")) {
    fixture <- parametric_gradient_fixture(aggregator)

    analytic <- bridgr:::evaluate_parametric_objective(
      parameters = fixture$parameters,
      parametric_specs = fixture$parametric_specs,
      fixed_aggregated = fixture$fixed_aggregated,
      target_tbl = fixture$target_tbl,
      target_name = fixture$target_name,
      indic_lags = fixture$indic_lags,
      target_lags = fixture$target_lags
    )
    numeric <- finite_difference_gradient(
      fun = function(current_parameters) {
        bridgr:::evaluate_parametric_objective(
          parameters = current_parameters,
          parametric_specs = fixture$parametric_specs,
          fixed_aggregated = fixture$fixed_aggregated,
          target_tbl = fixture$target_tbl,
          target_name = fixture$target_name,
          indic_lags = fixture$indic_lags,
          target_lags = fixture$target_lags
        )$value
      },
      x = fixture$parameters
    )

    expect_equal(
      analytic$gradient,
      numeric,
      tolerance = 1e-4,
      info = paste("aggregator =", aggregator)
    )
  }
})
