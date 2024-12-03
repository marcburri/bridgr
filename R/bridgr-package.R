#' @description
#' To learn more about bridger, start with the vignettes:
#' `browseVignettes(package = "bridgr")`
#' @keywords internal
"_PACKAGE"
utils::globalVariables(c(".", "id", "ind_freq", "month", "week", "n_rows", "quarter", "time", "year"))

#' @importFrom magrittr %>%
#' @importFrom rlang %||%
#' @importFrom lubridate %m+% %m-%
#' @importFrom stats as.formula frequency lm na.omit
#' @importFrom utils tail
#' @importFrom generics forecast
NULL

#' Generics to re-export

#' @export
generics::forecast

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL
