#' @description
#' To learn more about bridgr, start with the vignettes:
#' `browseVignettes(package = "bridgr")`
#' @keywords internal
"_PACKAGE"
utils::globalVariables(c(
  ".data", "id", "n", "period", "time", "values"
))

#' @importFrom rlang .data
#' @importFrom rlang %||%
#' @importFrom lubridate %m+% %m-%
#' @importFrom utils tail
#' @importFrom forecast forecast
NULL

# Objects for re export

#' @export
forecast::forecast
