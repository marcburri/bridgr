#' Summarize a `bridge`  object
#'
#' This method is summarizes the bridge model.
#' @param object A `bridge` object obtained from [bridgr::bridge()].
#' @param ... Additional arguments to be passed to the summary function. Ignored at the moment.
#' @return The function \code{summary} is used to obtain and print a summary of the
#' results.
#' @examples
#' library(bridgr)
#'
#' # Example usage
#' target_series <- suppressMessages(tsbox::ts_tbl(data.frame(
#'   time = seq(as.Date("2020-01-01"), as.Date("2022-12-01"), by = "quarter"),
#'   value = rnorm(12)
#' )))
#'
#' indic_series <- suppressMessages(tsbox::ts_tbl(data.frame(
#'   time = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "month"),
#'   value = rnorm(37)
#' )))
#'
#' bridge_model <- suppressMessages(bridge(
#'   target = target_series,
#'   indic = indic_series,
#'   indic_predict = "mean",
#'   indic_aggregators = "mean",
#'   indic_lags = 2,
#'   target_lags = 1,
#'   h = 1
#' ))
#'
#' # Summarize the information in the bridge model
#' summary(bridge_model)
#' @export
summary.bridge <- function(object, ...) {

  cat("Bridge model summary\n")
  cat("-----------------------------------\n")
  cat("Main model:\n")
  cat("-----------------------------------\n")
  cat("Series: ", object$target_name, "\n")
  output <- capture.output(print(object$model))
  # Check if the first line starts with "Series" and modify the output accordingly
  if (startsWith(output[1], "Series")) {
    output <- output[-1]  # Remove the first line if it starts with "Series"
  }
  cat(output, sep = "\n")
  cat("-----------------------------------\n")
  cat("Single indicator models:\n")
  cat("-----------------------------------\n")
  expa <- 1
  for (i in 1:length(object$indic_name)) {
    cat("Series: ", object$indic_name[i], "\n")
    if (object$indic_predict[i] == "mean") {
      cat("Writing forward the mean over values in last low frequency period.\n")
    } else if (object$indic_predict[i] == "last") {
      cat("Writing forward the last value in last low frequency period.\n")
    } else {
      output <- capture.output(print(object$indic_models[[i]]))
      # Check if the first line starts with "Series" and modify the output accordingly
      if (startsWith(output[1], "Series")) {
        output <- output[-1]  # Remove the first line if it starts with "Series"
      }
      cat(output, sep = "\n")
    }
    cat("Aggregation to low frequency:\n")
    if (object$indic_aggregators[i] == "mean") {
      cat("Using mean over values in corresponding periods.\n")
    } else if (object$indic_aggregators[i] == "last") {
      cat("Using last values of corresponding periods.\n")
    } else if (object$indic_aggregators[i] == "sum") {
      cat("Using sum over values in corresponding periods.\n")
    } else if (object$indic_aggregators[i] == "expalmon") {
      cat("Estimating exponential almon polynomial with the following weights: \n")
      cat(paste0(round(object$expalmon_weights[[expa]],3)))
      cat("\n")
      expa <- expa + 1
    }
    cat("-----------------------------------\n")
  }

  invisible(object)
}
