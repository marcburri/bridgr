
#' @keywords internal
#' @noRd
hac_lag_order <- function(n_obs) {
  if (n_obs <= 1) {
    return(0L)
  }

  as.integer(min(n_obs - 1L, floor(4 * (n_obs / 100)^(2 / 9))))
}


#' @keywords internal
#' @noRd
hac_long_run_covariance <- function(scores, lag = NULL) {
  scores <- as.matrix(scores)
  n_obs <- nrow(scores)
  if (n_obs == 0) {
    return(matrix(NA_real_, nrow = ncol(scores), ncol = ncol(scores)))
  }

  lag <- lag %||% hac_lag_order(n_obs)
  lag <- max(0L, min(as.integer(lag), n_obs - 1L))
  covariance <- crossprod(scores) / n_obs

  if (lag == 0L) {
    return(covariance)
  }

  for (lag_index in seq_len(lag)) {
    weight <- 1 - lag_index / (lag + 1)
    gamma <- crossprod(
      scores[(lag_index + 1):n_obs, , drop = FALSE],
      scores[seq_len(n_obs - lag_index), , drop = FALSE]
    ) / n_obs
    covariance <- covariance + weight * (gamma + t(gamma))
  }

  covariance
}


#' @keywords internal
#' @noRd
vcov_from_jacobian <- function(jacobian, residuals, lag = NULL) {
  jacobian <- as.matrix(jacobian)
  n_obs <- nrow(jacobian)
  if (n_obs < 2 || ncol(jacobian) == 0) {
    return(matrix(NA_real_, nrow = ncol(jacobian), ncol = ncol(jacobian)))
  }

  bread <- crossprod(jacobian) / n_obs
  bread_inv <- tryCatch(
    solve(bread),
    error = function(...) {
      tryCatch(
        qr.solve(bread),
        error = function(...) {
          decomposition <- svd(bread)
          positive <- decomposition$d >
            max(decomposition$d) * .Machine$double.eps
          if (!any(positive)) {
            return(matrix(NA_real_, nrow = ncol(bread), ncol = ncol(bread)))
          }
          decomposition$v[, positive, drop = FALSE] %*%
            diag(1 / decomposition$d[positive], nrow = sum(positive)) %*%
            t(decomposition$u[, positive, drop = FALSE])
        }
      )
    }
  )
  scores <- residuals * jacobian
  meat <- hac_long_run_covariance(scores = scores, lag = lag)
  bread_inv %*% meat %*% bread_inv / n_obs
}


#' @keywords internal
#' @noRd
coefficient_vcov_hac <- function(model) {
  model_matrix <- stats::model.matrix(model)
  covariance <- vcov_from_jacobian(
    jacobian = model_matrix,
    residuals = stats::residuals(model)
  )
  dimnames(covariance) <- list(
    names(stats::coef(model)),
    names(stats::coef(model))
  )
  covariance
}


#' @keywords internal
#' @noRd
coefficient_vcov_delta_hac <- function(
  model,
  formula,
  target_tbl,
  target_name,
  fixed_aggregated,
  parametric_specs,
  parameter_blocks,
  indic_lags,
  target_lags,
  epsilon = 1e-6
) {
  coefficient_values <- stats::coef(model)
  baseline_set <- rebuild_parametric_estimation_set(
    target_tbl = target_tbl,
    target_name = target_name,
    fixed_aggregated = fixed_aggregated,
    parametric_specs = parametric_specs,
    parameter_blocks = parameter_blocks,
    indic_lags = indic_lags,
    target_lags = target_lags
  )
  baseline_matrix <- stats::model.matrix(
    object = stats::terms(model),
    data = baseline_set
  )
  theta_labels <- parametric_parameter_labels(parametric_specs)
  jacobian <- matrix(
    NA_real_,
    nrow = nrow(baseline_matrix),
    ncol = length(coefficient_values) + length(theta_labels),
    dimnames = list(NULL, c(names(coefficient_values), theta_labels))
  )
  jacobian[, names(coefficient_values)] <- baseline_matrix

  theta_vector <- flatten_parameter_blocks(
    parameter_blocks = parameter_blocks,
    specs = parametric_specs
  )

  for (theta_index in seq_along(theta_vector)) {
    # Relative step keeps the central difference well-scaled even when theta
    # is far from zero (notably for log-scale beta parameters).
    step <- epsilon * max(1, abs(theta_vector[[theta_index]]))
    plus_theta <- theta_vector
    minus_theta <- theta_vector
    plus_theta[[theta_index]] <- plus_theta[[theta_index]] + step
    minus_theta[[theta_index]] <- minus_theta[[theta_index]] - step

    plus_blocks <- split_parameter_vector(
      parameters = plus_theta,
      specs = parametric_specs
    )
    minus_blocks <- split_parameter_vector(
      parameters = minus_theta,
      specs = parametric_specs
    )
    names(plus_blocks) <- names(parametric_specs)
    names(minus_blocks) <- names(parametric_specs)

    mean_plus <- bridge_mean_from_parameters(
      coefficient_values = coefficient_values,
      formula = formula,
      estimation_set = rebuild_parametric_estimation_set(
        target_tbl = target_tbl,
        target_name = target_name,
        fixed_aggregated = fixed_aggregated,
        parametric_specs = parametric_specs,
        parameter_blocks = plus_blocks,
        indic_lags = indic_lags,
        target_lags = target_lags
      )
    )
    mean_minus <- bridge_mean_from_parameters(
      coefficient_values = coefficient_values,
      formula = formula,
      estimation_set = rebuild_parametric_estimation_set(
        target_tbl = target_tbl,
        target_name = target_name,
        fixed_aggregated = fixed_aggregated,
        parametric_specs = parametric_specs,
        parameter_blocks = minus_blocks,
        indic_lags = indic_lags,
        target_lags = target_lags
      )
    )
    jacobian[, length(coefficient_values) + theta_index] <-
      (mean_plus - mean_minus) / (2 * step)
  }

  covariance <- vcov_from_jacobian(
    jacobian = jacobian,
    residuals = stats::residuals(model)
  )
  dimnames(covariance) <- list(colnames(jacobian), colnames(jacobian))
  covariance
}


#' @keywords internal
#' @noRd
compute_bridge_coefficient_uncertainty <- function(
  model,
  formula,
  target_tbl,
  target_name,
  fixed_aggregated,
  parametric_specs,
  parameter_blocks,
  indic_lags,
  target_lags
) {
  coefficient_names <- names(stats::coef(model))
  if (length(parametric_specs) == 0) {
    covariance <- coefficient_vcov_hac(model)
    return(list(
      method = "hac",
      covariance = covariance,
      se = sqrt(diag(covariance))[coefficient_names]
    ))
  }

  full_covariance <- coefficient_vcov_delta_hac(
    model = model,
    formula = formula,
    target_tbl = target_tbl,
    target_name = target_name,
    fixed_aggregated = fixed_aggregated,
    parametric_specs = parametric_specs,
    parameter_blocks = parameter_blocks,
    indic_lags = indic_lags,
    target_lags = target_lags
  )
  covariance <- full_covariance[
    coefficient_names,
    coefficient_names,
    drop = FALSE
  ]

  list(
    method = "delta_hac",
    covariance = covariance,
    se = sqrt(diag(covariance))[coefficient_names],
    full_covariance = full_covariance
  )
}
