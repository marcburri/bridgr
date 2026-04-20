#' @description
#' To learn more about bridgr, start with the vignettes:
#' `browseVignettes(package = "bridgr")`
#' @srrstats {G1.1} The package documentation positions `bridgr` as a focused R interface for bridge-style nowcasting relative to existing R packages such as `midasr` and `midasml`, rather than as a first implementation of bridge or MIDAS methods.
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
