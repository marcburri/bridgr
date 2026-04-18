#' Summarize a Bridge Model
#'
#' @param object A `"bridge"` object returned by [bridge()].
#' @param ... Unused.
#'
#' @return `object`, invisibly.
#' @method summary bridge
#' @export
summary.bridge <- function(object, ...) {
  coefficient_estimates <- stats::coef(object$model)
  coefficient_table <- dplyr::tibble(
    term = names(coefficient_estimates),
    estimate = as.numeric(coefficient_estimates),
    bootstrap_se = if (isTRUE(object$bootstrap$enabled)) {
      as.numeric(object$bootstrap$coefficient_se[names(coefficient_estimates)])
    } else {
      NA_real_
    }
  )

  indicator_summary <- lapply(
    seq_along(object$indic_name),
    function(index) {
      indicator_id <- object$indic_name[[index]]
      indicator_meta <- object$indicator_frequencies[
        match(indicator_id, object$indicator_frequencies$id),
        ,
        drop = FALSE
      ]
      indicator_model <- object$indic_models[[indicator_id]]
      aggregator <- object$indic_aggregators[[index]]

      dplyr::tibble(
        indicator = indicator_id,
        frequency = paste0(
          indicator_meta$unit[[1]],
          " (step ",
          indicator_meta$step[[1]],
          ")"
        ),
        predict = object$indic_predict[[index]],
        aggregation = if (is.character(aggregator)) {
          aggregator
        } else {
          "custom_weights"
        },
        indicator_model = if (is.null(indicator_model)) {
          "deterministic"
        } else {
          class(indicator_model)[[1]]
        }
      )
    }
  ) |>
    dplyr::bind_rows()

  cat("Bridge model summary\n")
  cat("-----------------------------------\n")
  cat("Target series: ", object$target_name, "\n", sep = "")
  cat(
    "Target frequency: ",
    object$target_frequency$unit[[1]],
    " (step ",
    object$target_frequency$step[[1]],
    ")\n",
    sep = ""
  )
  cat("Forecast horizon: ", object$h, "\n", sep = "")
  cat("Target model: ", class(object$model)[[1]], "\n", sep = "")
  cat("Estimation rows: ", nrow(object$estimation_set), "\n", sep = "")
  cat(
    "Regressors: ",
    paste(object$regressor_names, collapse = ", "),
    "\n",
    sep = ""
  )

  if (identical(unique(object$indic_predict), "direct")) {
    cat("Indicator handling: direct alignment\n")
  }
  cat("-----------------------------------\n")
  cat("Target equation coefficients:\n")
  print(coefficient_table)
  cat("-----------------------------------\n")
  cat("Indicator summary:\n")
  print(indicator_summary)

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
          paste(round(aggregator, 3), collapse = ", "),
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
          round(object$parametric_weights[[indicator_id]], 3),
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
            round(object$parametric_parameters[[indicator_id]], 3),
            collapse = ", "
          ),
          "\n",
          sep = ""
        )
      }
    }
  }

  cat("-----------------------------------\n")
  cat("Uncertainty:\n")
  if (isTRUE(object$bootstrap$enabled)) {
    cat(
      "Method: conditional ",
      object$bootstrap$type,
      " bootstrap with predictive forecast draws\n",
      sep = ""
    )
    cat(
      "Bootstrap draws: ",
      object$bootstrap$valid_N,
      " / ",
      object$bootstrap$N,
      "\n",
      sep = ""
    )
    cat("Block length: ", object$bootstrap$block_length, "\n", sep = "")
  } else {
    cat("Method: none\n")
  }

  if (!is.null(object$parametric_optimization)) {
    cat("-----------------------------------\n")
    cat("Joint parametric aggregation optimization:\n")
    cat("Method: ", object$parametric_optimization$method, "\n", sep = "")
    cat(
      "Objective value: ",
      round(object$parametric_optimization$value, 4),
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
    if (!is.null(object$parametric_optimization$message) &&
      nzchar(object$parametric_optimization$message)) {
      cat("Message: ", object$parametric_optimization$message, "\n", sep = "")
    }
  }
  cat("-----------------------------------\n")

  invisible(object)
}
