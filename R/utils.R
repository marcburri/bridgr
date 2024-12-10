#' Standardize the time series data frame
#' Value column is renamed to `values`
#' @noRd
standardize_ts_tbl <- function(ts_data) {
  tsbox::ts_tbl(ts_data) %>%
    dplyr::rename(values = dplyr::any_of(c("value", "values"))) %>%
    suppressMessages()
}


#' Define a custom function for frequency labeling
#' @noRd
label_frequency <- function(frequency) {
  dplyr::case_when(
    round(frequency) == 365 ~ "day",
    round(frequency) == 52 ~ "week",
    round(frequency) == 35 ~ "week",
    round(frequency) == 12 ~ "month",
    round(frequency) == 4 ~ "quarter",
    round(frequency) == 1 ~ "year",
    TRUE ~ NA_character_  # Default to NA if no match
  )
}

#' Helper function for generating starts and forecasting date
#' @noRd
get_final_date <- function(freq, target_start, target_end, h) {
  if (freq == 1) {
    target_start <- lubridate::year(target_start)
    final_date <- target_end %m+% lubridate::years(h) %>%
      lubridate::ceiling_date("year") %m-% lubridate::days(1)

  } else if (freq == 4) {
    target_start <- paste0(lubridate::year(target_start), "Q", lubridate::quarter(target_start))
    final_date <- target_end %m+% months(3 * h) %>%
      lubridate::ceiling_date("quarter") %m-% lubridate::days(1)

  } else if (freq == 12) {
    target_start <- paste0(lubridate::year(target_start), "-", lubridate::month(target_start))
    final_date <- target_end %m+% months(h) %>%
      lubridate::ceiling_date("month") %m-% lubridate::days(1)

  } else if (round(freq) %in% c(35, 52)) {
    target_start <- paste0(lubridate::year(target_start), "-", lubridate::week(target_start))
    final_date <- target_end %m+% lubridate::weeks(h) %>%
      lubridate::ceiling_date("week") %m-% lubridate::days(1)

  } else {
    stop("Unsupported frequency.")
  }
  list(target_start = target_start, final_date = final_date)
}

#' Exponential almon polynomial
#' @noRd
exp_almon <- function(p, K) {
  # p: vector of parameters (length corresponds to the order of the polynomial + 1)
  # K: number of lags

  if (length(p) < 2) {
    rlang::abort("The parameter vector 'p' must have at least 2 elements.")
  }

  # Create a sequence of lag indices
  k <- 0:(K - 1)

  # Compute the exponential Almon weights
  polynomial_terms <- sapply(0:(length(p) - 1), function(i) p[i + 1] * k^i)
  numerator <- exp(rowSums(polynomial_terms))
  denominator <- sum(numerator)
  weights <- numerator / denominator

  return(weights)
}

#' Define the objective function for expalmon regression
#' @noRd
expalmon_objective <- function(p, y, x, K, target_freq_label) {
  target_name <- y$id[1]
  indic_name <- x$id[1]

  # Compute the weights
  weights <- exp_almon(p, K)

  # Aggregate high-frequency data into low-frequency predictors
  aggregated_x <- x %>%
    dplyr::mutate(period = lubridate::floor_date(time, rlang::syms(target_freq_label))) %>%
    tsbox::ts_na_omit() %>% suppressMessages() %>%
    dplyr::group_by(period) %>% # Group by the start of each period
    dplyr::slice_tail(n = K) %>% # Select the last K obs of each group
    # calculated weighted sum
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ sum(weights * ., na.rm=T))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      time = period,
      id = indic_name) %>%
    dplyr::select(-period)

  data <- dplyr::bind_rows(aggregated_x, y) %>%
    tsbox::ts_wide() %>% suppressMessages() %>%
    stats::na.omit()

  # fit the model
  model <- stats::lm(stats::as.formula(paste0(target_name, " ~ ", indic_name)), data = data)

  fitted_values <- stats::predict(model)

  # Compute residuals
  residuals <- data[target_name] - fitted_values

  # Return the sum of squared residuals
  return(sum(residuals^2))
}
