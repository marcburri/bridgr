#' @description
#' To learn more about bridger, start with the vignettes:
#' `browseVignettes(package = "bridgr")`
#' @keywords internal
"_PACKAGE"
utils::globalVariables(c(
  ".", "id", "ind_freq", "month", "week", "quarter", "time", "year",
  "n", "values", "frquency", "period", "observations", "targ_freq"))

#' @importFrom magrittr %>%
#' @importFrom rlang %||%
#' @importFrom lubridate %m+% %m-%
#' @importFrom utils tail capture.output
#' @importFrom generics forecast
NULL

# Objects for re export

#' @export
generics::forecast

