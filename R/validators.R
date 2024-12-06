#' Validator Function for Bridge Class
#'
#' @param self An S7 class instance to validate.
#' @return Error message if validation fails, otherwise NULL.
#' @keywords internal
#' @noRd
validate_bridge <- function(self) {

  # Validate @target as ts-boxable
  if (!tsbox::ts_boxable(self@target)) {
    stop("@target must be a ts-boxable object, not ", class(self@target))
  }

  # Validate @indic as ts-boxable
  if (!tsbox::ts_boxable(self@indic)) {
    stop("must be a ts-boxable object, not ", class(self@indic))
  }

  # Get frequency of the target and indic variables
  target_summary <-  suppressMessages(tsbox::ts_summary(self@target))
  indic_summaries <- suppressMessages(tsbox::ts_summary(self@indic))

  # Validate @target to be a single time series
  if (nrow(target_summary) > 1) {
    stop("@target must be a single time series.")
  }

  # Validate target frequency: Yearly or higher
  target_freq <- round(target_summary$freq)
  if (!target_freq %in% c(1, 4, 12, 35, 48, 52, 53)) {
    stop("@target frequency must be weekly, monthly, quarterly or yearly.")
  }

  # Validate frequency of each indicator in the list: Same or higher than target
  for (i in 1:nrow(indic_summaries)) {
    indic_freq <- round(indic_summaries[i,]$freq)
    if (!indic_freq %in% c(1, 4, 12, 35, 48, 52, 53, 240, 365)) {
      stop(indic_summaries[i,]$id, " frequency in @indic must be daily, weekly, monthly, quarterly or yearly.")
    }
    else if (round(indic_freq) < round(target_freq)) {
      stop(indic_summaries[i,]$id, " frequency in @indic must be the same as or higher than the @target frequency.")
    }
  }


  # Validate last observation of each indicator in the list: Same date or later than target
  for (i in 1:nrow(indic_summaries)) {
    if (indic_summaries[i,]$end < target_summary$end) {
      stop("Last observation of ", indic_summaries[i,]$id, " in @indic must be the same date or later than the last observation of @target.")
    }
  }


  # Validation for indic_predict
  num_indic_series <- nrow(indic_summaries)
  if (!is.null(self@indic_predict)) {
    len_indic_predict <- length(self@indic_predict)
    if (len_indic_predict != num_indic_series && len_indic_predict != 1) {
      stop("@indic_predict must have length 1 or the same length as the number of time series in @indic.")
    }
  }

  # Validation for indic_aggregate
  if (!is.null(self@indic_aggregators)) {
    len_indic_aggregators <- length(self@indic_aggregators)
    if (len_indic_aggregators != num_indic_series && len_indic_aggregators != 1) {
      stop("@indic_aggregators must have length 1 or the same length as the number of time series in @indic.")
    }
  }

  # Validate @indic_lags and @target_lags
  if (self@indic_lags < 0) {
    stop("@indic_lags must be a non-negative integer.")
  }
  else if (self@target_lags < 0) {
    stop("@target_lags must be a non-negative integer.")
  }
  else if (self@h < 1) {
    stop("@h must be a non-negative integer.")
  }

  # Default frequency conversions
  default_frequency_conversions <- c("dpw" = 5, "wpm" = 4, "mpq" = 3, "qpy" = 4)

  # Check if fewer than 4 values are supplied and ensure they are named
  if (!identical(as.numeric(self@frequency_conversions), as.numeric(default_frequency_conversions))) {
    if (length(self@frequency_conversions) < 4 && is.null(names(self@frequency_conversions))) {
      stop("Custom frequency_conversions must be named and have at least one valid name in 'dpw', 'wpm', 'mpq', or 'qpy'.")
    }

    # Check if all names are valid
    invalid_names <- setdiff(names(self@frequency_conversions), names(default_frequency_conversions))
    if (length(invalid_names) > 0) {
      stop(paste(
        "Invalid names in frequency_conversions:",
        paste(invalid_names, collapse = ", "),
        "\nValid names are 'dpw', 'wpm', 'mpq', and 'qpy'."
      ))
    } else {
      # Update the default values with the user-supplied ones
      default_frequency_conversions[names(self@frequency_conversions)] <- self@frequency_conversions
      self@frequency_conversions <- default_frequency_conversions
    }
    # Check dpw is less than or equal to 7
    if (self@frequency_conversions["dpw"] > 7) {
      stop("dpw must be less than or equal to 7.")
    }
    # check wpm is less than or equal to 5
    if (self@frequency_conversions["wpm"] > 5) {
      stop("wpm must be less than or equal to 5.")
    }
    # check mpq is less than or equal to 3
    if (self@frequency_conversions["mpq"] > 3) {
      stop("mpq must be less than or equal to 3.")
    }
    # check qpy is less than or equal to 4
    if (self@frequency_conversions["qpy"] > 4) {
      stop("qpy must be less than or equal to 4.")
    }

  }



    NULL

}


