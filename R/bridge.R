#' Estimate a Bridge Model
#'
#' This function estimates a bridge model, aligning high-frequency indicator variables with
#' a lower-frequency target variable to perform nowcasting or forecasting. The bridge model
#' leverages time series alignment, lag structures, and forecasting methods to provide a
#' comprehensive tool for time series analysis.
#'
#' @param target A time series or data frame representing the target variable (dependent variable).
#' Must be in a format compatible with the [tsbox](https://docs.ropensci.org/tsbox/) package
#' (see [tsbox::ts_boxable()]).
#' @param indic A time series, list of time series, or data frame containing the indicator variables
#' (independent variables). Must be in a format compatible with the [tsbox](https://docs.ropensci.org/tsbox/) package
#' (see [tsbox::ts_boxable()]).
#' @param indic_predict A character string or vector specifying the forecasting method(s) for the
#' indicator variables. Supported methods include `"mean"`, `"last"`, `"auto.arima"`, and `"ets"`.
#' Defaults to `"auto.arima"`.
#' @param indic_aggregators A character string or vector specifying the aggregation method(s) for aligning
#' indicator variables with the target variable. Supported methods include `"mean"`, `"last"`, `"expalmon"`, `"sum"`
#' or o custom vector of weights with the same length as the frequency ratio. Defaults to `"mean"`.
#' @param indic_lags An integer or vector of integers specifying the number of lags to include
#' for the indicator variables. Defaults to 0 (no lags).
#' @param target_lags An integer specifying the number of lags to include for the target variable.
#' Defaults to 0 (no lags).
#' @param h An integer specifying the forecast horizon in terms of the target variable's frequency.
#' Defaults to 1 (next period).
#' @param frequency_conversions A named vector specifying the conversion factors between different
#' time frequencies. Defaults to `c("dpw" = 5, "wpm" = 4, "mpq" = 3, "qpy" = 4)` for days per week,
#' weeks per month, months per quarter, and quarters per year, respectively.
#' @param ... Additional arguments for future extension, not used at the moment.
#'
#' @return An object of class `"bridge"` containing:
#' - **target**: The standardized target variable.
#'
#' - **indic**: The standardized indicator variables.
#'
#' - **indic_predict**: The prediction methods applied to the indicators.
#'
#' - **indic_aggregators**: The aggregation methods used for the indicators.
#'
#' - **estimation_set**: A data frame containing the aligned and processed time series
#'   used to estimate the bridge model. This set includes the target variable and all
#'   indicator variables transformed to match the target variable's frequency and alignment.
#'
#' - **forecast_set**: A data frame containing the aligned and processed time series
#'   used for forecasting. This includes the forecasts for the indicator variables as
#'   inputs for the h-step ahead prediction of the target variable.
#'
#' - **model**: The fitted bridge model object for the target variable.
#'
#' - **indic_models**: A list of models used to forecast the indicator variables. Each
#'   element in this list corresponds to the forecasting method (e.g., `auto.arima` or `ets`)
#'   applied to an individual indicator variable.
#'
#' - **Additional components**: Internal parameters, summary statistics, and alignment metadata.
#'
#' @details
#' The bridge model aligns time series of different frequencies by slicing and aggregating
#' indicator variables to match the target variable's frequency. It uses predefined rules
#' for frequency conversion and alignment. The function checks for mismatches in start dates
#' and aligns the variables when necessary.
#'
#' ### Forecasting methods for the indicator variables
#' - **`auto.arima`**: Automatically selects the best ARIMA (AutoRegressive Integrated Moving Average)
#'   model for a given time series based on information criteria (e.g., AIC, AICc, BIC). The method
#'   identifies the orders of the AR (p), differencing (d), and MA (q) components and estimates the model
#'   parameters. It is particularly suitable for time series with seasonality, trends, or other non-stationary patterns.
#'
#' - **`ets`**: Fits an exponential smoothing state-space model to the data. The ETS framework automatically
#'    includes Error (additive or multiplicative), Trend (none, additive, or damped), and Seasonal (none, additive,
#'   or multiplicative) components. This method is effective for capturing underlying patterns in the data
#'   such as level, trend, and seasonality, making it suitable for time series with these features.
#'
#' ### Aggregation methods for the indicator variables
#' - **`mean`**: Calculates the mean of the indicator variable values within each target period.
#' - **`last`**: Takes the last value of the indicator variable within each target period.
#' - **`expalmon`**: Estimates a nonlinear exponential almon lag polynomial for weighting the indicator.
#' - **`sum`**: Calculates the sum of the indicator variable values within each target period.
#' - **Custom weights**: Allows the user to specify custom weights for aggregating the indicator variables.
#'
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
#' @author Marc Burri
#' @references
#' - Baffigi, A., Golinelli, R., & Parigi, G. (2004). Bridge models to forecast the euro area GDP. International Journal of Forecasting, 20(3), 447–460. \doi{doi:10.1016/S0169-2070(03)00067-0}
#' - Burri, M. (2023). Do daily lead texts help nowcasting GDP growth? IRENE Working Papers 23-02. \url{https://www5.unine.ch/RePEc/ftp/irn/pdfs/WP23-02.pdf}
#' - Schumacher, C. (2016). A comparison of MIDAS and bridge equations. International Journal of Forecasting, 32(2), 257–270. \doi{doi:10.1016/j.ijforecast.2015.07.004}
#' @export
bridge <- function( # TODO: fully document
    target,
    indic,
    indic_predict = NULL,
    indic_aggregators = NULL,
    indic_lags = 0,
    target_lags = 0,
    h = 1, # Nowcast horizon, always in terms of the target frequency
    frequency_conversions = c("dpw" = 5, "wpm" = 4, "mpq" = 3, "qpy" = 4),
    ... # TODO: Additional arguments to be passed to the model (i.e. for forecasting package, expalmon control)
    ) # TODO: write a bunch of tests for this function
  {

  # Create a new instance of the Bridge class and validate inputs
  model <- bridge_model(
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
  model$target <-
    tsbox::ts_tbl(target) %>%
      standardize_ts_tbl() %>%
      dplyr::mutate( id = target_name) %>%
      suppressMessages()
  if (length(suppressMessages(tsbox::ts_tslist(indic))) > 1) {
    model$indic <-
      tsbox::ts_tbl(indic) %>%
      standardize_ts_tbl() %>%
      suppressMessages()
    indic_name <- unique(model$indic$id)
  } else {
    indic_name <- deparse(substitute(indic))
    model$indic <- suppressMessages(
      tsbox::ts_tbl(indic) %>%
        standardize_ts_tbl() %>%
        dplyr::mutate( id = indic_name)
    )
  }

  # Get frequency of the target and indic variables
  target_summary <-  suppressMessages(tsbox::ts_summary(model$target))
  indic_summaries <- suppressMessages(tsbox::ts_summary(model$indic))

  # Check if indic_predict is a single value or a list
  num_indic_series <-  nrow(indic_summaries)
  if(!is.null(model$indic_predict)) {
    if (length(model$indic_predict) == 1 && num_indic_series > 1) {
      rlang::warn("Only one value provided for @indic_predict. Assuming the same value for all time series in @indic.")
      model$indic_predict <- rep(indic_predict, num_indic_series)
    }
  } else {
    model$indic_predict <- rep("auto.arima", num_indic_series)
  }

  # Check if indic_aggregators is a single value or a list
  if (!is.null(model$indic_aggregators)) {
    if (length(model$indic_aggregators) == 1 && num_indic_series > 1) {
      rlang::warn("Only one value provided for @indic_aggregators. Assuming the same value for all time series in @indic.")
      model$indic_aggregators <- rep(model$indic_aggregators, num_indic_series)
    }
  } else {
    model$indic_aggregators <- rep("mean", num_indic_series)
  }

  # Some definitions of frequencies
  dpw <- as.numeric(frequency_conversions["dpw"])             # Days per week
  wpm <- as.numeric(frequency_conversions["wpm"])             # Weeks per month
  mpq <- as.numeric(frequency_conversions["mpq"])             # Months per quarter
  qpy <- as.numeric(frequency_conversions["qpy"])             # Quarters per year
  dpm <- dpw*wpm       # Days per month
  wpq <- wpm*mpq       # Weeks per quarter
  dpq <- dpw*wpq       # Days per quarter
  dpy <- dpw*wpq*qpy   # Days per year
  wpy <- wpm*mpq*qpy   # Weeks per year
  mpy <- mpq*qpy       # Months per year

  slicing_rules <- dplyr::tibble(
    targ_freq = c(rep("month", 3),rep("quarter",4), rep("year", 5)),
    ind_freq = c("day", "week", "month", "day", "week", "month", "quarter", "day", "week", "month", "quarter", "year"),
    observations = c(
      dpm, wpm, 1,
      dpq, wpq, mpq, 1,
      dpy, wpy, mpy, qpy, 1
      )     # Define number of rows to slice
  )


  target_freq <- target_summary$freq
  if(is.null(names(target_freq))) names(target_freq) <- target_name
  indic_freqs <- indic_summaries$freq
  if(is.null(names(indic_freqs))) names(indic_freqs) <- indic_name


  target_freq <- dplyr::tibble(
    id = names(target_freq),
    frquency = as.numeric(target_freq)
  ) %>% dplyr::mutate(
    freq_label = label_frequency(frquency)
    )

  indic_freqs <- dplyr::tibble(
    id = names(indic_freqs),
    frquency = as.numeric(indic_freqs)
  ) %>% dplyr::mutate(
    freq_label = label_frequency(frquency)
  ) %>%
    dplyr::left_join(
      slicing_rules %>%
        dplyr::filter(targ_freq == target_freq$freq_label), by = c("freq_label" = "ind_freq")
      )

  # Make sure the number of defined frequencies hold
  indics <- suppressMessages(tsbox::ts_tbl(model$indic)) %>%
    standardize_ts_tbl() %>%
    dplyr::mutate(id = if (!"id" %in% colnames(.)) indic_name else id) %>%
    stats::na.omit() %>%
    dplyr::mutate(
      period = lubridate::floor_date(time, target_freq$freq_label)
    ) %>%
    dplyr::left_join(indic_freqs %>% dplyr::select(id, observations), by = "id") %>%  # Add the slicing rule for each group
    dplyr::group_by(id, period) %>%
    dplyr::mutate(n = dplyr::n():1) %>%
    dplyr::filter(n <= observations) %>%
    dplyr::ungroup() %>%
    dplyr::select(id, time, values)

  # Make sure the provided time series are aligned
  # And determine final forecasting date for indics
  results <- get_final_date(
    freq = target_freq$frquency,
    target_start = target_summary$start,
    target_end = target_summary$end,
    h = h
  )
  target_start <- results$target_start
  indic_starts <- ifelse(target_freq$frquency == 1,
                         lubridate::year(indic_summaries$start),
                         ifelse(target_freq$frquency == 4,
                                paste0(lubridate::year(indic_summaries$start), "Q", lubridate::quarter(indic_summaries$start)),
                                ifelse(target_freq$frquency == 12,
                                       paste0(lubridate::year(indic_summaries$start),"-", lubridate::month(indic_summaries$start)),
                                       paste0(lubridate::year(indic_summaries$start), "-", lubridate::week(indic_summaries$start)))))
  final_forecasting_date <- results$final_date

  if (length(unique(c(indic_starts, target_start))) > 1) {
    # Get earliest common start date
    max_start <- max(c(target_summary$start, indic_summaries$start)) %>%
      lubridate::floor_date(target_freq$freq_label)
    target <- tsbox::ts_span(target, start = max_start) %>%
      dplyr::mutate(id = if (!"id" %in% colnames(.)) target_name else id) %>%
      suppressMessages()
    indics <-  tsbox::ts_span(indics, start = max_start) %>%
      dplyr::mutate(id = if (!"id" %in% colnames(.)) indic_name else id) %>%
      suppressMessages()
    message("The start dates of the target and indicator variables do not match. Aligning them to ", max_start)
  }


  # Get frequency of the target and indic variables
  target_summary <-  suppressMessages(tsbox::ts_summary(target))
  indic_summaries <- suppressMessages(tsbox::ts_summary(indic))


  message("Dependent variable: ", target_freq$id, " | Frequency: " ,
          target_freq$freq_label, " | Estimation sample: ", target_summary$start, " - ", target_summary$end,
          " | Forecast horizon: ", h, " ", target_freq$freq_label, "(s)")


  # Forecast the indics
  indic_forecasts <- dplyr::tibble()
  indic_models <- list()
  ind_nr <- 1
  for (ind_id in unique(indics$id)) {
    indic_predict <- model$indic_predict[ind_nr]
    indic_aggregators <- model$indic_aggregators[[ind_nr]]

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
      rlang::warn("The indicator variable ", ind_id, " has data until ", last_date, ". No forecast needed.")
      next
    }

    # Forecast the indicator variable
    if (indic_predict == "last") {
      # write last value forward until final_forecasting_date
      last_value <- tail(indic_single$values, 1)
      indic_single <- suppressMessages(tsbox::ts_bind(indic_single, rep(last_value, indic_horizon )))
    } else if (indic_predict == "mean") {
      # write mean value forward until final_forecasting_date
      mean_value <- tsbox::ts_span(indic_single, start = target_summary$end)$values %>% suppressMessages() %>%
        mean( na.rm=T)
      indic_single <- suppressMessages(tsbox::ts_bind(indic_single, rep(mean_value, indic_horizon)))
    } else if (indic_predict == "auto.arima") {
      # fit an ARIMA model to the indicator variable and forecast
      indic_models[[ind_nr]] <- indic_single %>% tsbox::ts_xts()  %>% suppressMessages() %>%
        forecast::auto.arima() %>% suppressMessages()
      indic_fcst <- forecast::forecast(indic_models[[ind_nr]], h = indic_horizon)
      indic_single <- suppressMessages(tsbox::ts_bind(indic_single, as.numeric(indic_fcst$mean)))
    } else if (indic_predict == "ets") {
      # fit an ETS model to the indicator variable and forecast
      indic_models[[ind_nr]] <- indic_single %>% tsbox::ts_xts()  %>% suppressMessages() %>%
        forecast::ets() %>% suppressMessages()
      indic_fcst <- forecast::forecast(indic_models[[ind_nr]], h = indic_horizon)
      indic_single <- suppressMessages(tsbox::ts_bind(indic_single, as.numeric(indic_fcst$mean)))
    } else {
      rlang::abort("The indicator prediction method ", indic_predict, " is not supported.")
    }

    # Aggregate the indicator to the target frequency
    if (is.character(indic_aggregators[1])){
    if (indic_aggregators == "last") {
      indic_single <- indic_single %>%
        tsbox::ts_na_omit() %>%
        dplyr::mutate(period = lubridate::floor_date(time, rlang::syms(target_freq$freq_label))) %>%
        dplyr::group_by(period) %>% # Group by the start of each month
        dplyr::slice_tail(n = 1) %>%                           # Select the last row of each group
        dplyr::ungroup() %>%
        dplyr::mutate(time = period) %>%
        dplyr::select(-period) %>%
        suppressMessages()
    } else if (indic_aggregators == "mean") {
      indic_single <- indic_single %>% tsbox::ts_frequency(to = target_freq$freq_label, aggregate = "mean", na.rm=TRUE) %>%
        suppressMessages()
    } else if (indic_aggregators == "sum") {
      indic_single <- indic_single %>% tsbox::ts_frequency(to = target_freq$freq_label, aggregate = "sum", na.rm=TRUE) %>%
        suppressMessages()
    } else if (indic_aggregators == "expalmon") {

      # Set initial parameter guesses
      initial_params <- c(rep(0.01, 3))

      # Number of lags per low-frequency observation
      K <- indic_freqs[indic_freqs$id == ind_id,]$observations

      # Estimate parameters
      result <- stats::optim(
        par = initial_params,
        fn = expalmon_objective,
        y = model$target,
        x = indic_single,
        K = K,
        target_freq_label = target_freq$freq_label,
        control = list(trace = F, maxit = 5000)
      )

      weights <- exp_almon(result$par, K)
      model$expalmon_weights[[ind_id]] <- weights

      indic_single <- indic_single %>%
        dplyr::mutate(period = lubridate::floor_date(time, rlang::syms(target_freq$freq_label))) %>%
        dplyr::group_by(period) %>% # Group by the start of each month
        dplyr::slice_tail(n = K) %>% # Select the last K obs of each group
        # calculated weighted sum
        dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ sum(weights * ., na.rm=T))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          time = period,
          id = ind_id) %>%
        dplyr::select(-period)

    }
      } else if (is.numeric(indic_aggregators)) {

      # Number of lags per low-frequency observation
      K <- indic_freqs[indic_freqs$id == ind_id,]$observations

      if (length(indic_aggregators) != K) {
        rlang::abort("The number of aggregation weights must be equal to the number of observations per low-frequency observation.")
      } else if(sum(indic_aggregators) != 1) {
        rlang::abort("The sum of the aggregation weights must be equal to 1.")
      }

      weights <- indic_aggregators
      indic_single <- indic_single %>%
        dplyr::mutate(period = lubridate::floor_date(time, rlang::syms(target_freq$freq_label))) %>%
        dplyr::group_by(period) %>% # Group by the start of each month
        dplyr::slice_tail(n = K) %>% # Select the last K obs of each group
        # calculated weighted sum
        dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ sum(weights * ., na.rm=T))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          time = period,
          id = ind_id) %>%
        dplyr::select(-period)

    }
    else {
      rlang::abort("The indicator aggregation method ", indic_aggregators, " is not supported.")
    }

    indic_forecasts <- dplyr::bind_rows(indic_forecasts, indic_single %>% dplyr::mutate(id = ind_id))
    ind_nr <- ind_nr + 1
  }

  # Save the indicator models
  model$indic_models <- indic_models

  # Get the final sample date
  final_sample_date <- target_summary$end %>%
    lubridate::ceiling_date(target_freq$freq_label) %m-%
    lubridate::days(1)

  # Add indicator lags if specified
  indic_forecasts_lagged <- indic_forecasts
  if (model$indic_lags > 0) {
    for (lag in 1:model$indic_lags) {
      lagged_indic_tmp <- indic_forecasts %>%
        tsbox::ts_lag(by = lag) %>%
        dplyr::mutate(id = paste0(id, "_lag", lag)) %>%
        suppressMessages()
      indic_forecasts_lagged <- dplyr::bind_rows(indic_forecasts_lagged, lagged_indic_tmp)
    }
  }

  # Add target lags if specified
  # target_lagged  <- model$target %>%
  #   dplyr::mutate( id = target_name)
  # if (model$target_lags > 0) {
  #   for (lag in 1:model$target_lags) {
  #     lagged_target_tmp <- target %>%
  #       dplyr::mutate( id = target_name) %>%
  #       tsbox::ts_lag(by = lag) %>%
  #       dplyr::mutate(id = paste0(id, "_lag", lag)) %>%
  #       suppressMessages()
  #     target_lagged <- dplyr::bind_rows(target_lagged, lagged_target_tmp)
  #   }
  # }

  # Full dataset
  full_data <- dplyr::bind_rows(
    indic_forecasts_lagged,
    #target_lagged
    model$target
    )

  model$target_name <- target_name
  model$indic_name <- indic_name

  # Split the target into estimation and forecast periods
  estimation_set <- full_data %>%
    dplyr::filter(time <= as.Date(final_sample_date))

  forecast_set <- full_data %>%
    dplyr::filter(time > as.Date(final_sample_date))

  # Estimate the model
  formula <- stats::as.formula(paste(
    target_name,
    " ~ ",
    paste(unique(estimation_set$id)[!unique(estimation_set$id) %in% c("time", target_name)],
          collapse = " + ")
    ))
  model$formula <- formula

  model$estimation_set <-  tsbox::ts_wide(estimation_set) %>%
    stats::na.omit() %>% suppressMessages()

  model$forecast_set <- tsbox::ts_wide(forecast_set) %>%
    stats::na.omit() %>% suppressMessages()

  estimation_set <- tsbox::ts_xts(tsbox::ts_long(model$estimation_set))
  forecast_set <- tsbox::ts_xts(tsbox::ts_long(model$forecast_set))

  # Fit the model
  model$model <- forecast::Arima(
    estimation_set[,target_name],
    order = c(target_lags,0,0),
    xreg =  estimation_set[,!colnames(estimation_set) == target_name]
    )


  return(model)
}


#' Define the Bridge class
#'
#' Constructor for the S3 Bridge object
#' @keywords internal
#' @noRd
bridge_model <- function(
    target,
    indic,
    indic_predict,
    indic_aggregators,
    indic_lags,
    target_lags,
    h,
    frequency_conversions)
  {
  obj <- list(
    target = target,
    indic = indic,
    indic_predict = indic_predict,
    indic_aggregators = indic_aggregators,
    indic_lags = indic_lags,
    target_lags = target_lags,
    h = h,
    frequency_conversions = frequency_conversions,
    target_name = NULL,
    estimation_set = NULL,
    forecast_set = NULL,
    indic_name = NULL,
    indic_models = list(),
    expalmon_weights = list()
  )

  # Assign the S3 class
  class(obj) <- "bridge"

  # Validate the inputs
  validate_bridge(obj)

  return(obj)
}


#' Validator Function for Bridge Class
#'
#' @param self An S3 bridge_model class instance to validate.
#' @return Error message if validation fails, otherwise NULL.
#' @keywords internal
#' @noRd
validate_bridge <- function(self) {

  # Validate @target as ts-boxable
  if (!tsbox::ts_boxable(self$target)) {
    rlang::abort("@target must be a ts-boxable object, not ", class(self$target))
  }

  # Validate @indic as ts-boxable
  if (!tsbox::ts_boxable(self$indic)) {
    rlang::abort("must be a ts-boxable object, not ", class(self$indic))
  }

  # Get frequency of the target and indic variables
  target_summary <-  suppressMessages(tsbox::ts_summary(self$target))
  indic_summaries <- suppressMessages(tsbox::ts_summary(self$indic))

  # Validate @target to be a single time series
  if (nrow(target_summary) > 1) {
    rlang::abort("@target must be a single time series.")
  }

  # Validate target frequency: Yearly or higher
  target_freq <- round(target_summary$freq)
  if (!target_freq %in% c(1, 4, 12)) {
    rlang::abort("@target frequency must be  monthly, quarterly or yearly.")
  }

  # Validate frequency of each indicator in the list: Same or higher than target
  for (i in 1:nrow(indic_summaries)) {
    indic_freq <- round(indic_summaries[i,]$freq)
    if (!indic_freq %in% c(1, 4, 12, 35, 48, 52, 53, 240, 365)) {
      rlang::abort(indic_summaries[i,]$id, " frequency in @indic must be daily, weekly, monthly, quarterly or yearly.")
    }
    else if (round(indic_freq) < round(target_freq)) {
      rlang::abort(indic_summaries[i,]$id, " frequency in @indic must be the same as or higher than the @target frequency.")
    }
  }


  # Validate last observation of each indicator in the list: Same date or later than target
  for (i in 1:nrow(indic_summaries)) {
    if (indic_summaries[i,]$end < target_summary$end) {
      rlang::abort("Last observation of ", indic_summaries[i,]$id, " in @indic must be the same date or later than the last observation of @target.")
    }
  }


  # Validation for indic_predict
  num_indic_series <- nrow(indic_summaries)
  if (!is.null(self$indic_predict)) {
    len_indic_predict <- length(self$indic_predict)
    if (len_indic_predict != num_indic_series && len_indic_predict != 1) {
      rlang::abort("@indic_predict must have length 1 or the same length as the number of time series in @indic.")
    }
  }

  # Validation for indic_aggregate
  if (!is.null(self$indic_aggregators)) {
    len_indic_aggregators <- length(self$indic_aggregators)
    if (len_indic_aggregators != num_indic_series && len_indic_aggregators != 1) {
      rlang::abort("@indic_aggregators must have length 1 or the same length as the number of time series in @indic.")
    }
  }

  # Validate @indic_lags and @target_lags
  if (self$indic_lags < 0) {
    rlang::abort("@indic_lags must be a non-negative integer.")
  }
  else if (self$target_lags < 0) {
    rlang::abort("@target_lags must be a non-negative integer.")
  }
  else if (self$h < 1) {
    rlang::abort("@h must be a non-negative integer.")
  }

  # Default frequency conversions
  default_frequency_conversions <- c("dpw" = 5, "wpm" = 4, "mpq" = 3, "qpy" = 4)

  # Check if fewer than 4 values are supplied and ensure they are named
  if (!identical(as.numeric(self$frequency_conversions), as.numeric(default_frequency_conversions))) {
    if (length(self$frequency_conversions) < 4 && is.null(names(self$frequency_conversions))) {
      rlang::abort("Custom frequency_conversions must be named and have at least one valid name in 'dpw', 'wpm', 'mpq', or 'qpy'.")
    }

    # Check if all names are valid
    invalid_names <- setdiff(names(self$frequency_conversions), names(default_frequency_conversions))
    if (length(invalid_names) > 0) {
      rlang::abort(paste(
        "Invalid names in frequency_conversions:",
        paste(invalid_names, collapse = ", "),
        "\nValid names are 'dpw', 'wpm', 'mpq', and 'qpy'."
      ))
    } else {
      # Update the default values with the user-supplied ones
      default_frequency_conversions[names(self$frequency_conversions)] <- self$frequency_conversions
      self$frequency_conversions <- default_frequency_conversions
    }
    # Check dpw is less than or equal to 7
    if (self$frequency_conversions["dpw"] > 7) {
      rlang::abort("dpw must be less than or equal to 7.")
    }
    # check wpm is less than or equal to 5
    if (self$frequency_conversions["wpm"] > 5) {
      rlang::abort("wpm must be less than or equal to 5.")
    }
    # check mpq is less than or equal to 3
    if (self$frequency_conversions["mpq"] > 3) {
      rlang::abort("mpq must be less than or equal to 3.")
    }
    # check qpy is less than or equal to 4
    if (self$frequency_conversions["qpy"] > 4) {
      rlang::abort("qpy must be less than or equal to 4.")
    }

  }
  NULL
}



