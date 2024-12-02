#' Estimate Bridge Model
#'
#' This function estimates a bridge model for nowcasting or forecasting purposes.
#' The bridge model aligns high-frequency indicator variables (`indic`) with a
#' lower-frequency target variable (`target`) and performs nowcasting or forecasting.
#'
#' @param target A time series or data frame representing the target variable (dependent variable).
#' It should be in a format compatible with the [tsbox](https://docs.ropensci.org/tsbox/) package
#' (See [tsbox::ts_boxable()]).
#' @param indic A time series, listof time series, or data frame containing the indicator variables
#' (independent variables). It must be in a format compatible with the [tsbox](https://docs.ropensci.org/tsbox/)
#' package (See [tsbox::ts_boxable()]).
#' @param indic_predict A character string or vector specifying the prediction method(s) for the
#' indicator variables. Defaults to `"mean"`. Future updates will add support for methods
#' from the `forecast` package.
#' @param indic_aggregators A character string or vector specifying the aggregation method(s)
#' for aligning indicator variables with the target variable. Defaults to `"mean"`. Future updates
#' will support custom weighting functions.
#' @param indic_lags An integer or vector of integers specifying the number of lags to include
#' for the indicator variables. Defaults to `0` (no lags).
#' @param target_lags An integer specifying the number of lags to include for the target variable.
#' Defaults to `0` (no lags).
#' @param h An integer specifying the forecast horizon (in terms of the target variable's frequency).
#' Defaults to `1` (next period).
#' @param frequency_conversions A named vector specifying the conversion factors for different
#' time frequencies (e.g., days per week, weeks per month).
#' Defaults to `c("dpw" = 5, "wpm" = 4, "mpq" = 3, "qpy" = 4)`.
#' @param ... Additional arguments to be passed to the model, such as parameters for forecasting
#' methods from the `forecast` package (to be implemented in future versions).
#'
#' @return An object of class `bridge_model`, containing the following elements:
#' - `target`: The standardized target variable.
#' - `indic`: The standardized indicator variables.
#' - `indic_predict`: The prediction methods applied to the indicators.
#' - `indic_aggregators`: The aggregation methods used for indicators.
#' - `final_forecasting_date`: The calculated end date for forecasting.
#' - Additional internal parameters and summary statistics.
#'
#' @details
#' The bridge model aligns time series of different frequencies by slicing and aggregating
#' indicator variables to match the target variable's frequency. It uses predefined rules
#' for frequency conversion and alignment. The function checks for mismatches in start dates
#' and aligns the variables when necessary.
#'
#' Future extensions will include:
#' - Support for more sophisticated forecasting methods from the `forecast` package.
#' - Custom weighting functions for indicator aggregation.
#' - Enhanced validation and flexibility in specifying prediction and aggregation strategies.
#'
#' @export
bridge <- function(
    target,
    indic,
    indic_predict = NULL, # TODO: add support for forecast package methods
    indic_aggregators = NULL, # TODO: add support for custom weighting functions
    indic_lags = 0,
    target_lags = 0,
    h = 1, # Nowcast horizon, always in terms of the target frequency
    frequency_conversions = c("dpw" = 5, "wpm" = 4, "mpq" = 3, "qpy" = 4),
    ... # Additional arguments to be passed to the model (i.e. for forecasting package)
    )
  {
  # Create a new instance of the Bridge class and validate inputs
  bridge_model <- bridge_model(
    target = target,
    indic = indic,
    indic_predict = indic_predict,
    indic_aggregators = indic_aggregators,
    indic_lags = indic_lags,
    target_lags = target_lags,
    h = h,
    frequency_conversions = frequency_conversions
  )

  # Bring target and indicator variables to the same format (ts_tbl)
  # And add corresponding id columns if missing
  # Get the name of the target and indicator variables
  target_name <- deparse(substitute(target))
  bridge_model@target <- suppressMessages(
    tsbox::ts_tbl(target) %>%
      standardize_ts_tbl() %>%
      dplyr::mutate( id = target_name)
   )
  if (length(suppressMessages(tsbox::ts_tslist(indic))) > 1) {
    indic_name <- unique(indic$id)
    bridge_model@indic <- suppressMessages(
      tsbox::ts_tbl(indic) %>%
        standardize_ts_tbl()
    )
  } else {
    indic_name <- deparse(substitute(indic))
    bridge_model@indic <- suppressMessages(
      tsbox::ts_tbl(indic) %>%
        standardize_ts_tbl() %>%
        dplyr::mutate( id = indic_name)
    )
  }

  # Get frequency of the target and indic variables
  target_summary <-  suppressMessages(tsbox::ts_summary(bridge_model@target))
  indic_summaries <- suppressMessages(tsbox::ts_summary(bridge_model@indic))

  # Check if indic_predict is a single value or a list
  num_indic_series <-  nrow(indic_summaries)
  if(!is.null(indic_predict)) {
    if (length(indic_predict) == 1 && num_indic_series > 1) {
      warning("Only one value provided for @indic_predict. Assuming the same value for all time series in @indic.")
      bridge_model@indic_predict <- rep(indic_predict, num_indic_series)
    }
  } else {
    bridge_model@indic_predict <- rep("auto.arima", num_indic_series)
  }

  # Check if indic_aggregators is a single value or a list
  if (!is.null(indic_aggregators)) {
    if (length(indic_aggregators) == 1 && num_indic_series > 1) {
      warning("Only one value provided for @indic_aggregators. Assuming the same value for all time series in @indic.")
      bridge_model@indic_aggregators <- rep(indic_aggregators, num_indic_series)
    }
  } else {
    bridge_model@indic_aggregators <- rep("mean", num_indic_series)
  }

  # Some definitions of frequencies
  dpw <- frequency_conversions[1]             # Days per week
  wpm <- frequency_conversions[2]             # Weeks per month
  mpq <- frequency_conversions[3]             # Months per quarter
  qpy <- frequency_conversions[4]             # Quarters per year
  dpm <- dpw*wpm       # Days per month
  wpq <- wpm*mpq       # Weeks per quarter
  dpq <- dpw*wpq       # Days per quarter
  dpy <- dpw*wpq*qpy   # Days per year
  wpy <- wpm*mpq*qpy   # Weeks per year
  mpy <- mpq*qpy       # Months per year


  target_freq <- target_summary$freq
  if(is.null(names(target_freq))) names(target_freq) <- target_name
  indic_freqs <- indic_summaries$freq
  if(is.null(names(indic_freqs))) names(indic_freqs) <- indic_name

  target_freq <- dplyr::tibble(
    id = names(target_freq),
    frquency = as.numeric(target_freq)
  ) %>% dplyr::mutate(
    freq_label = dplyr::case_when(
      round(frquency) == 365 ~ "day",
      round(frquency) == 52 ~ "week",
      round(frquency) == 35 ~ "week",
      round(frquency) == 12 ~ "month",
      round(frquency) == 4 ~ "quarter",
      round(frquency) == 1 ~ "year"
    ))

  # Make sure the provided time series are aligned
  # And determine final forecasting date for indics
  if (target_freq$frquency == 1) {
    target_start <- lubridate::year(target_summary$start)
    indic_starts <- lubridate::year(indic_summaries$start)

    final_forecasting_date <- target_summary$end %m+%
      lubridate::years(h) %>%
      lubridate::ceiling_date("year") %m-%
      lubridate::days(1)

    # Define rules for slicing
    slicing_rules <- dplyr::tibble(
      ind_freq = c("daily", "weekly", "monthly", "quarterly", "yearly"),
      n_rows = c(dpy, wpy, mpy, qpy, 1)     # Define number of rows to slice
    )

    indics <- suppressMessages(tsbox::ts_tbl(bridge_model@indic)) %>%
      standardize_ts_tbl() %>%
      dplyr::mutate(id = if (!"id" %in% colnames(.)) indic_name else id) %>%
      na.omit() %>%
      dplyr::mutate(
        year = lubridate::year(time),
        ind_freq = dplyr::case_when(
          round(indic_freqs[id]) == 365 ~ "daily",
          round(indic_freqs[id]) == 52 ~ "weekly",
          round(indic_freqs[id]) == 35 ~ "weekly",
          round(indic_freqs[id]) == 12 ~ "monthly",
          round(indic_freqs[id]) == 4 ~ "quarterly",
          round(indic_freqs[id]) == 1 ~ "yearly"
        )
      ) %>%
      dplyr::left_join(slicing_rules, by = "ind_freq") %>%  # Add the slicing rule for each group
      dplyr::group_by(year, ind_freq) %>%
      dplyr::group_map(~ {
        n_rows <- unique(.x$n_rows)  # Get the slicing rule
        n_rows <- min(n_rows, nrow(.x))  # Ensure it doesn't exceed available rows
        dplyr::slice_tail(.x, n = n_rows)
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::ungroup() %>%
      dplyr::select(-n_rows)


  } else if (target_freq$frquency == 4) {
    target_start <- paste0(
      lubridate::year(target_summary$start), "Q",
      lubridate::quarter(target_summary$start)
      )
    indic_starts <- paste0(
      lubridate::year(indic_summaries$start), "Q",
      lubridate::quarter(indic_summaries$start)
      )

    final_forecasting_date <- target_summary$end %m+%
      months(3*h) %>%
      lubridate::ceiling_date("quarter") %m-%
      lubridate::days(1)

    # Define rules for slicing
    slicing_rules <- dplyr::tibble(
      ind_freq = c("daily", "weekly", "monthly", "quarterly"),
      n_rows = c(dpq, wpq, mpq, 1)     # Define number of rows to slice
    )

    indics <- suppressMessages(tsbox::ts_tbl(bridge_model@indic)) %>%
      standardize_ts_tbl() %>%
      dplyr::mutate(id = if (!"id" %in% colnames(.)) indic_name else id) %>%
      na.omit() %>%
      dplyr::mutate(
        year = lubridate::year(time),
        quarter = lubridate::quarter(time),
        ind_freq = dplyr::case_when(
          round(indic_freqs[id]) == 365 ~ "daily",
          round(indic_freqs[id]) == 52 ~ "weekly",
          round(indic_freqs[id]) == 35 ~ "weekly",
          round(indic_freqs[id]) == 12 ~ "monthly",
          round(indic_freqs[id]) == 4 ~ "quarterly"
        )
      ) %>%
      dplyr::left_join(slicing_rules, by = "ind_freq") %>%  # Add the slicing rule for each group
      dplyr::group_by(year, quarter, ind_freq) %>%
      dplyr::group_map(~ {
        n_rows <- unique(.x$n_rows)  # Get the slicing rule
        n_rows <- min(n_rows, nrow(.x))  # Ensure it doesn't exceed available rows
        dplyr::slice_tail(.x, n = n_rows)
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::ungroup() %>%
      dplyr::select(-n_rows)

  } else if (target_freq$frquency == 12) {
    target_start <- paste0(
      lubridate::year(target_summary$start),
      lubridate::month(target_summary$start)
    )
    indic_starts <- paste0(
      lubridate::year(indic_summaries$start),
      lubridate::month(indic_summaries$start)
    )

    final_forecasting_date <- target_summary$end %m+%
      months(h) %>%
      lubridate::ceiling_date("month") %m-%
      lubridate::days(1)

    # Define rules for slicing
    slicing_rules <- dplyr::tibble(
      ind_freq = c("daily", "weekly", "monthly"),
      n_rows = c(dpm, wpm, 1)     # Define number of rows to slice
    )

    indics <- suppressMessages(tsbox::ts_tbl(indic)) %>%
      standardize_ts_tbl() %>%
      dplyr::mutate(id = if (!"id" %in% colnames(.)) indic_name else id) %>%
      na.omit() %>%
      dplyr::mutate(
        year = lubridate::year(time),
        month = lubridate::month(time),
        ind_freq = dplyr::case_when(
          round(indic_freqs[id]) == 365 ~ "daily",
          round(indic_freqs[id]) == 52 ~ "weekly",
          round(indic_freqs[id]) == 35 ~ "weekly",
          round(indic_freqs[id]) == 12 ~ "monthly"
        )
      ) %>%
      dplyr::left_join(slicing_rules, by = "ind_freq") %>%  # Add the slicing rule for each group
      dplyr::group_by(year, month, ind_freq) %>%
      dplyr::group_map(~ {
        n_rows <- unique(.x$n_rows)  # Get the slicing rule
        n_rows <- min(n_rows, nrow(.x))  # Ensure it doesn't exceed available rows
        dplyr::slice_tail(.x, n = n_rows)
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::ungroup() %>%
      dplyr::select(-n_rows)

  } else if(round(target_freq$frquency) %in% c(35, 52)) {
    target_start <- paste0(
      lubridate::year(target_summary$start), "-",
      lubridate::week(target_summary$start)
    )
    indic_starts <- paste0(
      lubridate::year(indic_summaries$start), "-",
      lubridate::week(indic_summaries$start)
    )

    final_forecasting_date <- target_summary$end %m+%
      lubridate::weeks(h) %>%
      lubridate::ceiling_date("week") %m-%
      lubridate::days(1)

    # Define rules for slicing
    slicing_rules <- dplyr::tibble(
      ind_freq = c("daily", "weekly"),
      n_rows = c(dpw, 1)     # Define number of rows to slice
    )

    indics <- suppressMessages(tsbox::ts_tbl(bridge_model@indic)) %>%
      standardize_ts_tbl() %>%
      dplyr::mutate(id = if (!"id" %in% colnames(.)) indic_name else id) %>%
      na.omit() %>%
      dplyr::mutate(
        year = lubridate::year(time),
        month = lubridate::week(time),
        ind_freq = dplyr::case_when(
          round(indic_freqs[id]) == 365 ~ "daily",
          round(indic_freqs[id]) == 52 ~ "weekly",
          round(indic_freqs[id]) == 35 ~ "weekly"
        )
      ) %>%
      dplyr::left_join(slicing_rules, by = "ind_freq") %>%  # Add the slicing rule for each group
      dplyr::group_by(year, week, ind_freq) %>%
      dplyr::group_map(~ {
        n_rows <- unique(.x$n_rows)  # Get the slicing rule
        n_rows <- min(n_rows, nrow(.x))  # Ensure it doesn't exceed available rows
        dplyr::slice_tail(.x, n = n_rows)
      }) %>%
      dplyr::bind_rows() %>%
      dplyr::ungroup() %>%
      dplyr::select(-n_rows)
  }

  if (length(unique(indic_starts)) > 1) {
    warning("The start dates of the target and indicator variables do not match. Trying to align them.")
    # Get earliest common start date
    max_start <- max(c(target_summary$start, indic_summaries$start)) %>%
      lubridate::floor_date(target_freq$freq_label)
    target <- tsbox::ts_span(target, start = max_start) %>%
      dplyr::mutate(id = if (!"id" %in% colnames(.)) target_name else id) %>%
      suppressMessages()
    indics <-  tsbox::ts_span(indics, start = max_start) %>%
      dplyr::mutate(id = if (!"id" %in% colnames(.)) indic_name else id) %>%
      suppressMessages()
    message("The start dates of the target and indicator variables have been aligned to ", max_start)
  }


  indic_freqs <- dplyr::tibble(
    id = names(indic_freqs),
    frquency = as.numeric(indic_freqs)
  ) %>% dplyr::mutate(
    freq_label = dplyr::case_when(
      round(frquency) == 365 ~ "day",
      round(frquency) == 52 ~ "week",
      round(frquency) == 35 ~ "week",
      round(frquency) == 12 ~ "month",
      round(frquency) == 4 ~ "quarter",
      round(frquency) == 1 ~ "year"
    ))

  # Get frequency of the target and indic variables
  target_summary <-  suppressMessages(tsbox::ts_summary(target))
  indic_summaries <- suppressMessages(tsbox::ts_summary(indic))


  message("Dependent variable: ", target_freq$id, " | Frequency: " ,
          target_freq$freq_label, " | Estimation sample: ", target_summary$start, " - ", target_summary$end,
          " | Forecast horizon: ", h, " ", target_freq$freq_label, "(s)")


  # Forecast the indics
  indic_forecasts <- dplyr::tibble()
  indic_models <- list()
  for (ind_id in unique(indics$id)) {
    ind_nr <- 1
    indic_predict <- bridge_model@indic_predict[ind_nr]
    indic_aggregators <- bridge_model@indic_aggregators[ind_nr]

    indic_single <- indics %>%
      dplyr::filter(id == ind_id)

    indic_unit <- indic_freqs[indic_freqs$id == ind_id,]$freq_label

    last_date <- max(indic_single$time)
    last_indic_date <- lubridate::floor_date(final_forecasting_date, indic_unit)

    # Calculate the number of periods to forecast
    if (indic_unit == target_freq$freq_label) {
      indic_horizon <- h
    } else{
    indic_horizon <- lubridate::interval(
      last_date,
      last_indic_date
      ) %>%
      lubridate::time_length(unit = indic_unit) %>%
      as.numeric() %>% round()
    }

    if (last_date >= final_forecasting_date) {
      warning("The indicator variable ", ind_id, " has data until ", last_date, ". No forecast needed.")
      next
    }

    # Forecast the indicator variable
    if (indic_predict == "last") {
      # write last value forward until final_forecasting_date
      last_value <- tail(indic_single$values, 1)
      indic_single <- suppressMessages(tsbox::ts_bind(indic_single, rep(last_value, indic_horizon )))
    } else if (indic_predict == "mean") {
      # write mean value forward until final_forecasting_date
      mean_value <- tsbox::ts_span(indic_single, start = target_summary$end)$values %>%
        mean( na.rm=T) %>% suppressMessages()
      indic_single <- suppressMessages(tsbox::ts_bind(indic_single, rep(mean_value, indic_horizon)))
    } else if (indic_predict == "auto.arima") {
      # fit an ARIMA model to the indicator variable and forecast
      indic_models[[ind_nr]] <- indic_single %>% tsbox::ts_xts() %>%
        forecast::auto.arima() %>% suppressMessages()
      indic_fcst <- forecast::forecast(indic_models[[ind_nr]], h = indic_horizon)
      indic_single <- suppressMessages(tsbox::ts_bind(indic_single, as.numeric(indic_fcst$mean)))
    }

    # Aggregate the indicator to the target frequency
    if (indic_aggregators == "last") {
      indic_single <- indic_single %>% tsbox::ts_frequency(to = target_freq$freq_label, aggregate = "last", na.rm=TRUE) %>%
        suppressMessages()
    } else if (indic_aggregators == "mean") {
      indic_single <- indic_single %>% tsbox::ts_frequency(to = target_freq$freq_label, aggregate = "mean", na.rm=TRUE) %>%
        suppressMessages()
    }

    indic_forecasts <- dplyr::bind_rows(indic_forecasts, indic_single %>% dplyr::mutate(id = ind_id))
    ind_nr <- ind_nr + 1
  }

  # Get the final sample date
  final_sample_date <- target_summary$end %>%
    lubridate::ceiling_date(target_freq$freq_label) %m-%
    lubridate::days(1)

  # Add indicator lags if specified
  indic_forecasts_lagged <- indic_forecasts
  if (bridge_model@indic_lags > 0) {
    for (lag in 1:bridge_model@indic_lags) {
      lagged_indic_tmp <- indic_forecasts %>%
        tsbox::ts_lag(by = lag) %>%
        dplyr::mutate(id = paste0(id, "_lag", lag)) %>%
        suppressMessages()
      indic_forecasts_lagged <- dplyr::bind_rows(indic_forecasts_lagged, lagged_indic_tmp)
    }
  }


  # Add target lags if specified
  target_lagged  <- target %>%
    dplyr::mutate( id = target_name)
  if (bridge_model@target_lags > 0) {
    for (lag in 1:bridge_model@target_lags) {
      lagged_target_tmp <- target %>%
        dplyr::mutate( id = target_name) %>%
        tsbox::ts_lag(by = lag) %>%
        dplyr::mutate(id = paste0(id, "_lag", lag)) %>%
        suppressMessages()
      target_lagged <- dplyr::bind_rows(target_lagged, lagged_target_tmp)
    }
  }


  # Full dataset
  full_data <- dplyr::bind_rows(
    indic_forecasts_lagged,
    #target_lagged
    target %>%
      dplyr::mutate( id = target_name)
    )

  # Split the target into estimation and forecast periods
  estimation_set <- full_data %>%
    dplyr::filter(time <= as.Date(final_sample_date))

  forecast_set <- full_data %>%
    dplyr::filter(time > as.Date(final_sample_date))

  # Estimate the model
  formula <- as.formula(paste(
    target_name,
    " ~ ",
    paste(unique(estimation_set$id)[!unique(estimation_set$id) %in% c("time", target_name)],
          collapse = " + ")
    ))
  bridge_model@formula <- formula

  estimation_set <- tsbox::ts_wide(estimation_set) %>%
    na.omit() %>% suppressMessages()

  forecast_set <- tsbox::ts_wide(forecast_set) %>%
    na.omit() %>% suppressMessages()

  bridge_model@estimation_set <- estimation_set <- tsbox::ts_xts(tsbox::ts_long(estimation_set))
  bridge_model@forecast_set <- forecast_set <- tsbox::ts_xts(tsbox::ts_long(forecast_set))


  # bridge_model@model <- lm(formula, data = estimation_set) # TODO: hac robust standard errors
  bridge_model@model <- forecast::Arima(
    estimation_set[,target_name],
    order = c(target_lags,0,0),
    xreg =  estimation_set[,!colnames(estimation_set) == target_name]
    )


  return(bridge_model)
}

#' Define the Bridge class
#'
#' @include validators.R
#' @keywords internal
#' @noRd
bridge_model <- S7::new_class(
  name = "bridge_model",
  properties = list(
    # User inputs
    target = S7::new_property(class = S7::class_any),
    indic = S7::new_property(class = S7::class_any),
    indic_predict = S7::new_property(class = S7::class_any, default = NULL),
    indic_aggregators = S7::new_property(class = S7::class_any, default = NULL),
    indic_lags = S7::new_property(class = S7::class_numeric, default = 0),
    target_lags = S7::new_property(class = S7::class_numeric, default = 0),
    h = S7::new_property(class = S7::class_numeric, default = 1),
    frequency_conversions = S7::new_property( # TODO: Add validators
      class = S7::class_numeric,
      default = c("dpw" = 5,             # Days per week
                  "wpm" = 4,             # Weeks per month
                  "mpq" = 3,             # Months per quarter
                  "qpy" = 4              # Quarters per year
      )),
    # Internal properties
    model = S7::new_property(class = S7::class_any),
    estimation_set = S7::new_property(class = S7::class_any),
    forecast_set = S7::new_property(class = S7::class_any),
    formula = S7::new_property(class = S7::class_any)
  ),
  validator = validate_bridge
)
