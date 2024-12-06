#' @description
#' To learn more about bridger, start with the vignettes:
#' `browseVignettes(package = "bridgr")`
#' @keywords internal
"_PACKAGE"
utils::globalVariables(c(".", "id", "ind_freq", "month", "week", "n_rows", "quarter", "time", "year", "n", "values"))

#' @importFrom magrittr %>%
#' @importFrom rlang %||%
#' @importFrom lubridate %m+% %m-%
#' @importFrom stats as.formula frequency lm na.omit
#' @importFrom utils tail capture.output
#' @importFrom generics forecast
NULL

# Objects for re export

#' @export
generics::forecast

