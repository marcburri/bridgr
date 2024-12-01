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

  # Validate @target to be a single time series
  else if (is.matrix(self@target)) {
    stop("@target must be a single time series.")
  }

  # Validate target frequency: Yearly or higher
  target_freq <- round(frequency(suppressMessages(tsbox::ts_ts(self@target))))
  if (target_freq < 1 || !target_freq %in% c(1, 4, 12, 48, 52, 53)) {
    stop("@target frequency must be weekly, monthly, quarterly or yearly.")
  }


  # Validate @indic as ts-boxable
  if (!tsbox::ts_boxable(self@indic)) {
    stop("must be a ts-boxable object, not ", class(self@indic))
  }

  # Validate frequency of each indicator in the list: Same or higher than target
  indics <- suppressMessages(tsbox::ts_tslist(self@indic))
    for (i in indics) {
      indic_freq <- round(frequency(tsbox::ts_ts(i)))
      if (!indic_freq %in% c(1, 4, 12, 48, 52, 53, 240, 365)) {
        stop("Frequency of series in @indic must be daily, weekly, monthly, quarterly or yearly.")
      }
      else if (round(indic_freq) < round(target_freq)) {
        stop("Frequency of series in @indic must be the same as or higher than the frequency of @target.")
      }
    }


  # Validation for indic_predict
  num_indic_series <- length(indics)
  len_indic_predict <- length(self@indic_predict)
  if (len_indic_predict != num_indic_series && len_indic_predict != 1) {
    stop("@indic_predict must have length 1 or the same length as the number of time series in @indic.")
    }

  # Validation for indic_aggregate
  len_indic_aggregate <- length(self@indic_aggregate)
  if (len_indic_aggregate != num_indic_series && len_indic_aggregate != 1) {
    stop("@indic_aggregate must have length 1 or the same length as the number of time series in @indic.")
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


  # If all conditions pass
  else {
    NULL
  }
}


