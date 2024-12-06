#' Summarize a `bridge_model` class object
#'
#' This method is summarizes the bridge model.
#' @param object An object of class `bridge_model` obtained from [bridgr::bridge()].
#' @keywords internal
#' @noRd
summary.bridge <- S7::new_external_generic("base", "summary", "bridge_model")


#' Summarize a `bridge_model` class object
#'
#' This method is summarizes the bridge model.
#' @param object An object of class `bridge_model` obtained from [bridgr::bridge()].
#' @param ... Additional arguments to be passed to the summary function.
#' @usage summary(object, ...)
#' @keywords internal
#' @name summary.bridge
S7::method(summary.bridge, bridge_model) <- function(object, ...) {

  cat("Bridge model summary\n")
  cat("-----------------------------------\n")
  cat("Main model:\n")
  cat("-----------------------------------\n")
  cat("Series: ", object@target_name, "\n")
  output <- capture.output(print(object@model))
  # Check if the first line starts with "Series" and modify the output accordingly
  if (startsWith(output[1], "Series")) {
    output <- output[-1]  # Remove the first line if it starts with "Series"
  }
  cat(output, sep = "\n")
  cat("-----------------------------------\n")
  cat("Single indicator models:\n")
  cat("-----------------------------------\n")
  for (i in 1:length(object@indic_name)) {
    cat("Series: ", object@indic_name[i], "\n")
    if (object@indic_predict[i] == "mean") {
      cat("Writing forward the mean over values in last low frequency period.\n")
    } else if (object@indic_predict[i] == "last") {
      cat("Writing forward the last value in last low frequency period.\n")
    } else {
    output <- capture.output(print(object@indic_models[[i]]))
    # Check if the first line starts with "Series" and modify the output accordingly
    if (startsWith(output[1], "Series")) {
      output <- output[-1]  # Remove the first line if it starts with "Series"
    }
    cat(output, sep = "\n")
    }
    cat("Aggregation to low frequency:\n")
    if (object@indic_aggregators[i] == "mean") {
      cat("Using mean over values in corresponding periods.\n")
    } else if (object@indic_aggregators[i] == "last") {
      cat("Using last values of corresponding periods.\n")
    }
    cat("-----------------------------------\n")
  }

  invisible(object)
}
