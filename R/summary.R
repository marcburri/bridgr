#' Summarize a Bridge Model
#'
#' @param object A `"bridge"` object returned by [bridge()].
#' @param ... Unused.
#'
#' @return `object`, invisibly.
#' @method summary bridge
#' @export
summary.bridge <- function(object, ...) {
  format_number <- function(x) {
    formatC(x, format = "f", digits = 3)
  }

  coefficient_estimates <- stats::coef(object$model)
  coefficient_uncertainty_enabled <-
    !is.null(object$uncertainty$coefficient_se)
  prediction_uncertainty_enabled <-
    !is.null(object$uncertainty$prediction_method)
  coefficient_table <- data.frame(
    Estimate = format_number(as.numeric(coefficient_estimates)),
    check.names = FALSE
  )
  rownames(coefficient_table) <- names(coefficient_estimates)

  if (coefficient_uncertainty_enabled) {
    coefficient_label <- if (
      identical(object$uncertainty$coefficient_method, "delta_hac")
    ) {
      "Delta-HAC SE"
    } else if (
      identical(object$uncertainty$coefficient_method, "block_bootstrap")
    ) {
      "Bootstrap SE"
    } else {
      "HAC SE"
    }
    coefficient_table[[coefficient_label]] <- format_number(
      as.numeric(
        object$uncertainty$coefficient_se[names(coefficient_estimates)]
      )
    )
  }

  indicator_summary <- lapply(
    seq_along(object$indic_name),
    function(index) {
      indicator_id <- object$indic_name[[index]]
      indicator_meta <- object$indicator_frequencies[
        match(indicator_id, object$indicator_frequencies$id),
        ,
        drop = FALSE
      ]

      data.frame(
        Frequency = indicator_meta$unit[[1]],
        Predict = object$indic_predict[[index]],
        Aggregation = if (is.character(
          object$indic_aggregators_requested[[index]]
        )) {
          object$indic_aggregators_requested[[index]]
        } else {
          "custom_weights"
        },
        check.names = FALSE,
        row.names = indicator_id
      )
    }
  )
  indicator_summary <- do.call(rbind, indicator_summary)
  indicator_summary <- indicator_summary[
    ,
    c("Frequency", "Predict", "Aggregation"),
    drop = FALSE
  ]

  cat("Bridge model summary\n")
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

  cat("-----------------------------------\n")
  cat("Target equation coefficients:\n")
  print(
    noquote(format(coefficient_table, justify = "right")),
    quote = FALSE,
    right = TRUE
  )
  cat("-----------------------------------\n")
  cat("Indicator summary:\n")
  print(
    noquote(format(indicator_summary, justify = "left")),
    quote = FALSE,
    right = FALSE
  )

  has_custom_weights <- any(vapply(
    object$indic_aggregators,
    function(aggregator) !is.character(aggregator),
    FUN.VALUE = logical(1)
  ))
  if (has_custom_weights) {
    cat("-----------------------------------\n")
    cat("Custom aggregation weights:\n")
    for (index in seq_along(object$indic_aggregators)) {
      aggregator <- object$indic_aggregators[[index]]
      if (!is.character(aggregator)) {
        cat(
          object$indic_name[[index]],
          ": ",
          paste(format_number(aggregator), collapse = ", "),
          "\n",
          sep = ""
        )
      }
    }
  }

  parametric_ids <- names(object$parametric_parameters)
  if (length(parametric_ids) > 0) {
    cat("-----------------------------------\n")
    cat("Estimated parametric aggregation:\n")
    for (indicator_id in parametric_ids) {
      cat(
        indicator_id,
        " weights: ",
        paste(
          format_number(object$parametric_weights[[indicator_id]]),
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
            format_number(object$parametric_parameters[[indicator_id]]),
            collapse = ", "
          ),
          "\n",
          sep = ""
        )
      }
    }
  }

  if (coefficient_uncertainty_enabled || prediction_uncertainty_enabled) {
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
    } else if (identical(
      object$uncertainty$prediction_method,
      "residual_resampling"
    )) {
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

  if (!is.null(object$parametric_optimization)) {
    cat("-----------------------------------\n")
    cat("Joint parametric aggregation optimization:\n")
    cat("Method: ", object$parametric_optimization$method, "\n", sep = "")
    cat(
      "Objective value: ",
      format_number(object$parametric_optimization$value),
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
  cat("-----------------------------------\n")

  invisible(object)
}
