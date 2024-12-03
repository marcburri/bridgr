#' Summary method for bridge_model
#'
#' Implement the `summary` method for `bridge_model` class
#' @param bridge_model A `bridge_model` object.
#' @noRd
summary.bridge_model <- S7::new_external_generic("base", "summary", "bridge_model")

#' Defines the `summary` method for `bridge_model`
#'
#' @keywords internal
#' This method is summarizes the bridge model.
#' @param bridge_model A `bridge_model` object.
#' @name summary
S7::method(summary.bridge_model, bridge_model) <- function(bridge_model, ...) {

  cat("Bridge model summary\n")
  cat("-----------------------------------\n")
  cat("Main model:\n")
  cat("-----------------------------------\n")
  cat("Series: ", bridge_model@target_name, "\n")
  output <- capture.output(print(bridge_model@model))
  # Check if the first line starts with "Series" and modify the output accordingly
  if (startsWith(output[1], "Series")) {
    output <- output[-1]  # Remove the first line if it starts with "Series"
  }
  cat(output, sep = "\n")
  cat("-----------------------------------\n")
  cat("Single indicator models:\n")
  cat("-----------------------------------\n")
  for (i in 1:length(bridge_model@indic_name)) {
    cat("Series: ", bridge_model@indic_name[i], "\n")
    if (bridge_model@indic_predict[i] == "mean") {
      cat("Writing forward the mean over values in last low frequency period.\n")
    } else if (bridge_model@indic_predict[i] == "last") {
      cat("Writing forward the last value in last low frequency period.\n")
    } else {
    output <- capture.output(print(bridge_model@indic_models[[i]]))
    # Check if the first line starts with "Series" and modify the output accordingly
    if (startsWith(output[1], "Series")) {
      output <- output[-1]  # Remove the first line if it starts with "Series"
    }
    cat(output, sep = "\n")
    }
    cat("Aggregation to low frequency:\n")
    if (bridge_model@indic_aggregators[i] == "mean") {
      cat("Using mean over values in corresponding periods.\n")
    } else if (bridge_model@indic_aggregators[i] == "last") {
      cat("Using last values of corresponding periods.\n")
    }
    cat("-----------------------------------\n")
  }

  invisible(bridge_model)
}


