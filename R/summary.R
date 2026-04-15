#' Summarize a Bridge Model
#'
#' @param object A `"bridge"` object returned by [bridge()].
#' @param ... Unused.
#'
#' @return `object`, invisibly.
#' @method summary bridge
#' @export
summary.bridge <- function(object, ...) {
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
  cat("Formula: ", deparse(object$formula), "\n", sep = "")
  cat("-----------------------------------\n")
  cat("Main model:\n")
  cat("-----------------------------------\n")
  print(object$model)
  cat("-----------------------------------\n")
  cat("Indicator models:\n")
  cat("-----------------------------------\n")

  for (indicator_id in object$indic_name) {
    indicator_index <- match(indicator_id, object$indicator_frequencies$id)
    indicator_meta <- object$indicator_frequencies[
      indicator_index, ,
      drop = FALSE
    ]
    indicator_model <- object$indic_models[[indicator_id]]
    aggregator <- object$indic_aggregators[[indicator_index]]

    cat("Series: ", indicator_id, "\n", sep = "")
    cat(
      "Frequency: ",
      indicator_meta$unit[[1]],
      " (step ",
      indicator_meta$step[[1]],
      ")\n",
      sep = ""
    )
    cat(
      "Forecast method: ",
      object$indic_predict[[indicator_index]],
      "\n",
      sep = ""
    )

    if (is.null(indicator_model)) {
      cat("Indicator model: deterministic extension\n")
    } else {
      print(indicator_model)
    }

    if (is.character(aggregator)) {
      cat("Aggregation: ", aggregator, "\n", sep = "")
    } else {
      cat("Aggregation: custom weights\n")
      cat(paste(round(aggregator, 3), collapse = ", "), "\n", sep = "")
    }

    if (!is.null(object$parametric_weights[[indicator_id]])) {
      cat("Estimated parametric weights: ")
      cat(
        paste(round(object$parametric_weights[[indicator_id]], 3), collapse = ", ")
      )
      cat("\n")
      if (!is.null(object$parametric_parameters[[indicator_id]])) {
        cat("Estimated parametric parameters: ")
        cat(
          paste(
            round(object$parametric_parameters[[indicator_id]], 3),
            collapse = ", "
          )
        )
        cat("\n")
      }
    }

    cat("-----------------------------------\n")
  }

  if (!is.null(object$parametric_optimization)) {
    cat("Joint parametric aggregation optimization:\n")
    cat("-----------------------------------\n")
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
    cat("-----------------------------------\n")
  }

  invisible(object)
}
