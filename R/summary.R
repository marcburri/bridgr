#' Summarize a Mixed-Frequency Model
#'
#' @param object A `"mf_model"` object returned by [mf_model()].
#' @param ... Unused.
#'
#' @return `object`, invisibly.
#'
#' @srrstats {RE4.18} Provides a dedicated `summary.mf_model()` method.
#' @srrstats {RE4.11} Reports GOF metrics with the coefficient table.
#'
#' @examples
#' gdp_growth <- tsbox::ts_pc(gdp)
#' gdp_growth <- tsbox::ts_na_omit(gdp_growth)
#' model <- mf_model(
#'   target = gdp_growth,
#'   indic = baro,
#'   indic_predict = "auto.arima",
#'   indic_aggregators = "mean",
#'   h = 1
#' )
#'
#' summary(model)
#' @method summary mf_model
#' @export
summary.mf_model <- function(object, ...) {
  print_model_header(object)
  print_coefficient_table(object)
  print_gof_table(object)
  print_indicator_summary(object)
  print_custom_weights_block(object)
  print_parametric_block(object)
  print_uncertainty_block(object)
  print_optimization_block(object)
  cat("-----------------------------------\n")

  invisible(object)
}


#' @keywords internal
#' @noRd
format_summary_number <- function(x) {
  formatC(x, format = "f", digits = 3)
}


#' @keywords internal
#' @noRd
print_model_header <- function(object) {
  cat("Mixed-frequency model summary\n")
  cat("-----------------------------------\n")
  cat("Target series: ", object$target_name, "\n", sep = "")
  cat("Target frequency: ", object$target_frequency$unit[[1]], "\n", sep = "")
  cat("Forecast horizon: ", object$h, "\n", sep = "")
  cat("Estimation rows: ", nrow(object$estimation_set), "\n", sep = "")
  cat(
    "Regressors: ",
    paste(object$regressor_names, collapse = ", "),
    "\n",
    sep = ""
  )
}


#' @keywords internal
#' @noRd
print_coefficient_table <- function(object) {
  coefficient_estimates <- stats::coef(object$model)
  coefficient_table <- data.frame(
    Estimate = format_summary_number(as.numeric(coefficient_estimates)),
    check.names = FALSE
  )
  rownames(coefficient_table) <- names(coefficient_estimates)

  if (!is.null(object$uncertainty$coefficient_se)) {
    coefficient_label <- switch(
      object$uncertainty$coefficient_method %||% "hac",
      "delta_hac" = "Delta-HAC SE",
      "block_bootstrap" = "Bootstrap SE",
      "HAC SE"
    )
    coefficient_table[[coefficient_label]] <- format_summary_number(
      as.numeric(
        object$uncertainty$coefficient_se[names(coefficient_estimates)]
      )
    )
  }

  cat("-----------------------------------\n")
  cat("Target equation coefficients:\n")
  print(
    noquote(format(coefficient_table, justify = "right")),
    quote = FALSE,
    right = TRUE
  )
}


#' @keywords internal
#' @noRd
print_gof_table <- function(object) {
  lm_summary <- summary(object$model)
  gof_table <- data.frame(
    Statistic = c(
      "R-squared",
      "Adjusted R-squared",
      "Residual standard error"
    ),
    Value = format_summary_number(c(
      unname(lm_summary$r.squared),
      unname(lm_summary$adj.r.squared),
      unname(lm_summary$sigma)
    )),
    check.names = FALSE
  )

  cat("-----------------------------------\n")
  cat("Model fit:\n")
  print(
    noquote(format(gof_table, justify = "left")),
    quote = FALSE,
    right = FALSE,
    row.names = FALSE
  )
}


#' @keywords internal
#' @noRd
print_indicator_summary <- function(object) {
  indicator_summary <- lapply(
    seq_along(object$indic_name),
    function(index) {
      indicator_id <- object$indic_name[[index]]
      indicator_meta <- object$indicator_frequencies[
        match(indicator_id, object$indicator_frequencies$id),
        ,
        drop = FALSE
      ]
      requested <- object$indic_aggregators_requested[[index]]
      data.frame(
        Frequency = indicator_meta$unit[[1]],
        Predict = object$indic_predict[[index]],
        Aggregation = if (is.character(requested)) requested else "custom_weights",
        check.names = FALSE,
        row.names = indicator_id
      )
    }
  )
  indicator_summary <- do.call(rbind, indicator_summary)

  cat("-----------------------------------\n")
  cat("Indicator summary:\n")
  print(
    noquote(format(indicator_summary, justify = "left")),
    quote = FALSE,
    right = FALSE
  )
}


#' @keywords internal
#' @noRd
print_custom_weights_block <- function(object) {
  has_custom_weights <- any(vapply(
    object$indic_aggregators,
    function(aggregator) !is.character(aggregator),
    FUN.VALUE = logical(1)
  ))
  if (!has_custom_weights) {
    return(invisible(NULL))
  }

  cat("-----------------------------------\n")
  cat("Custom aggregation weights:\n")
  for (index in seq_along(object$indic_aggregators)) {
    aggregator <- object$indic_aggregators[[index]]
    if (!is.character(aggregator)) {
      cat(
        object$indic_name[[index]],
        ": ",
        paste(format_summary_number(aggregator), collapse = ", "),
        "\n",
        sep = ""
      )
    }
  }
}


#' @keywords internal
#' @noRd
print_parametric_block <- function(object) {
  parametric_ids <- names(object$parametric_parameters)
  if (length(parametric_ids) == 0) {
    return(invisible(NULL))
  }

  cat("-----------------------------------\n")
  cat("Estimated parametric aggregation:\n")
  for (indicator_id in parametric_ids) {
    cat(
      indicator_id,
      " weights: ",
      paste(
        format_summary_number(object$parametric_weights[[indicator_id]]),
        collapse = ", "
      ),
      "\n",
      sep = ""
    )
    if (!is.null(object$parametric_parameters[[indicator_id]])) {
      cat(
        indicator_id,
        " parameters: ",
        paste(
          format_summary_number(object$parametric_parameters[[indicator_id]]),
          collapse = ", "
        ),
        "\n",
        sep = ""
      )
    }
  }
}


#' @keywords internal
#' @noRd
print_uncertainty_block <- function(object) {
  coefficient_uncertainty_enabled <- !is.null(object$uncertainty$coefficient_se)
  prediction_uncertainty_enabled <- !is.null(object$uncertainty$prediction_method)
  if (!coefficient_uncertainty_enabled && !prediction_uncertainty_enabled) {
    return(invisible(NULL))
  }

  cat("-----------------------------------\n")
  cat("Uncertainty:\n")
  if (coefficient_uncertainty_enabled) {
    cat(
      "Coefficient SEs: ",
      object$uncertainty$coefficient_method,
      "\n",
      sep = ""
    )
  }
  if (identical(object$uncertainty$prediction_method, "block_bootstrap")) {
    cat("Prediction intervals: full-system block bootstrap\n")
    cat(
      "Bootstrap draws: ",
      object$bootstrap$valid_N,
      " / ",
      object$bootstrap$N,
      "\n",
      sep = ""
    )
    cat("Block length: ", object$bootstrap$block_length, "\n", sep = "")
  } else if (
    identical(object$uncertainty$prediction_method, "residual_resampling")
  ) {
    cat("Prediction intervals: residual resampling\n")
    cat(
      "Simulation paths: ",
      object$uncertainty$simulation_paths,
      "\n",
      sep = ""
    )
  } else if (isTRUE(object$bootstrap$requested)) {
    cat("Prediction intervals: unavailable\n")
  }
}


#' @keywords internal
#' @noRd
print_optimization_block <- function(object) {
  if (is.null(object$parametric_optimization)) {
    return(invisible(NULL))
  }

  cat("-----------------------------------\n")
  cat("Joint parametric aggregation optimization:\n")
  cat("Method: ", object$parametric_optimization$method, "\n", sep = "")
  cat(
    "Objective value: ",
    format_summary_number(object$parametric_optimization$value),
    "\n",
    sep = ""
  )
  cat(
    "Convergence code: ",
    object$parametric_optimization$convergence,
    "\n",
    sep = ""
  )
  cat(
    "Best start: ",
    object$parametric_optimization$best_start,
    " / ",
    object$parametric_optimization$n_starts,
    "\n",
    sep = ""
  )
}
