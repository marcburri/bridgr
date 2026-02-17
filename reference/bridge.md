# Estimate a Bridge Model

This function estimates a bridge model, aligning high-frequency
indicator variables with a lower-frequency target variable to perform
nowcasting or forecasting. The bridge model leverages time series
alignment, lag structures, and forecasting methods to provide a
comprehensive tool for time series analysis.

## Usage

``` r
bridge(
  target,
  indic,
  indic_predict = NULL,
  indic_aggregators = NULL,
  indic_lags = 0,
  target_lags = 0,
  h = 1,
  frequency_conversions = c(dpw = 5, wpm = 4, mpq = 3, qpy = 4),
  ...
)
```

## Arguments

- target:

  A time series or data frame representing the target variable
  (dependent variable). Must be in a format compatible with the
  [tsbox](https://docs.ropensci.org/tsbox/) package (see
  [`tsbox::ts_boxable()`](https://docs.ropensci.org/tsbox/reference/ts_boxable.html)).

- indic:

  A time series, list of time series, or data frame containing the
  indicator variables (independent variables). Must be in a format
  compatible with the [tsbox](https://docs.ropensci.org/tsbox/) package
  (see
  [`tsbox::ts_boxable()`](https://docs.ropensci.org/tsbox/reference/ts_boxable.html)).

- indic_predict:

  A character string or vector specifying the forecasting method(s) for
  the indicator variables. Supported methods include `"mean"`, `"last"`,
  `"auto.arima"`, and `"ets"`. Defaults to `"auto.arima"`.

- indic_aggregators:

  A character string or vector specifying the aggregation method(s) for
  aligning indicator variables with the target variable. Supported
  methods include `"mean"`, `"last"`, `"expalmon"`, `"sum"` or o custom
  vector of weights (provided in a
  [`list()`](https://rdrr.io/r/base/list.html)) with the same length as
  the frequency ratio. Defaults to `"mean"`.

- indic_lags:

  An integer or vector of integers specifying the number of lags to
  include for the indicator variables. Defaults to 0 (no lags).

- target_lags:

  An integer specifying the number of lags to include for the target
  variable. Defaults to 0 (no lags).

- h:

  An integer specifying the forecast horizon in terms of the target
  variable's frequency. Defaults to 1 (next period).

- frequency_conversions:

  A named vector specifying the conversion factors between different
  time frequencies. Defaults to
  `c("dpw" = 5, "wpm" = 4, "mpq" = 3, "qpy" = 4)` for days per week,
  weeks per month, months per quarter, and quarters per year,
  respectively.

- ...:

  Additional arguments for future extension, not used at the moment.

## Value

An object of class `"bridge"` containing:

- **target**: The standardized target variable.

- **indic**: The standardized indicator variables.

- **indic_predict**: The prediction methods applied to the indicators.

- **indic_aggregators**: The aggregation methods used for the
  indicators.

- **estimation_set**: A data frame containing the aligned and processed
  time series used to estimate the bridge model. This set includes the
  target variable and all indicator variables transformed to match the
  target variable's frequency and alignment.

- **forecast_set**: A data frame containing the aligned and processed
  time series used for forecasting. This includes the forecasts for the
  indicator variables as inputs for the h-step ahead prediction of the
  target variable.

- **model**: The fitted bridge model object for the target variable.

- **indic_models**: A list of models used to forecast the indicator
  variables. Each element in this list corresponds to the forecasting
  method (e.g., `auto.arima` or `ets`) applied to an individual
  indicator variable.

- **Additional components**: Internal parameters, summary statistics,
  and alignment metadata.

## Details

The bridge model aligns time series of different frequencies by slicing
and aggregating indicator variables to match the target variable's
frequency. It uses predefined rules for frequency conversion and
alignment. The function checks for mismatches in start dates and aligns
the variables when necessary.

### Forecasting methods for the indicator variables

- **`auto.arima`**: Automatically selects the best ARIMA (AutoRegressive
  Integrated Moving Average) model for a given time series based on
  information criteria (e.g., AIC, AICc, BIC). The method identifies the
  orders of the AR (p), differencing (d), and MA (q) components and
  estimates the model parameters.

- **`ets`**: Fits an exponential smoothing state-space model to the
  data. The ETS framework automatically includes Error (additive or
  multiplicative), Trend (none, additive, or damped), and Seasonal
  (none, additive, or multiplicative) components. This method is
  effective for capturing underlying patterns in the data such as level,
  trend, and seasonality, making it suitable for time series with these
  features.

### Aggregation methods for the indicator variables

- **`mean`**: Calculates the mean of the indicator variable values
  within each target period.

- **`last`**: Takes the last value of the indicator variable within each
  target period.

- **`expalmon`**: Estimates a nonlinear exponential almon lag polynomial
  for weighting the indicator.

- **`sum`**: Calculates the sum of the indicator variable values within
  each target period.

- **Custom weights**: Allows the user to specify custom weights for
  aggregating the indicator variables.

## References

- Baffigi, A., Golinelli, R., & Parigi, G. (2004). Bridge models to
  forecast the euro area GDP. International Journal of Forecasting,
  20(3), 447–460.
  [doi:10.1016/S0169-2070(03)00067-0](https://doi.org/10.1016/S0169-2070%2803%2900067-0)

- Burri, M. (2023). Do daily lead texts help nowcasting GDP growth?
  IRENE Working Papers 23-02.
  <https://www5.unine.ch/RePEc/ftp/irn/pdfs/WP23-02.pdf>

- Schumacher, C. (2016). A comparison of MIDAS and bridge equations.
  International Journal of Forecasting, 32(2), 257–270.
  [doi:10.1016/j.ijforecast.2015.07.004](https://doi.org/10.1016/j.ijforecast.2015.07.004)

## Author

Marc Burri

## Examples

``` r
library(bridgr)

# Example usage
target_series <- suppressMessages(tsbox::ts_tbl(data.frame(
  time = seq(as.Date("2020-01-01"), as.Date("2022-12-01"), by = "quarter"),
  value = stats::rnorm(12)
)))

indic_series <- suppressMessages(tsbox::ts_tbl(data.frame(
  time = seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "month"),
  value = stats::rnorm(37)
)))

bridge_model <- suppressMessages(bridge(
  target = target_series,
  indic = indic_series,
  indic_predict = "mean",
  indic_aggregators = "mean",
  indic_lags = 2,
  target_lags = 1,
  h = 1
))
```
