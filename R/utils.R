

# Standardize the time series data frame
# Value column is renamed to `values`
standardize_ts_tbl <- function(ts_data) {
  tsbox::ts_tbl(ts_data) %>%
    dplyr::rename(values = dplyr::any_of(c("value", "values"))) %>%
    suppressMessages()
}
