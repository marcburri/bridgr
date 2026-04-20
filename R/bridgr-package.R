#' @description
#' To learn more about bridgr, start with the vignettes:
#' `browseVignettes(package = "bridgr")`
#' @details
#' `bridgr` is under active maintenance. The public interface centered on
#' [mf_model()], [forecast()], [summary()], and [plot()] is considered stable
#' for bridge-style nowcasting workflows, while future development is expected
#' to expand diagnostics and mixed-frequency model options without changing the
#' core target-and-indicator workflow.
#' @srrstats {G1.2} Documents the package lifecycle and stable interface.
#' @srrstats {G1.4} Documents package functions with roxygen2.
#' @srrstats {G1.4a} Documents internal helpers with roxygen2 and `@noRd`.
#' @srrstats {G1.1} Positions `bridgr` relative to nearby R packages.
#' @srrstats {G5.2} Tests should cover warning and error behavior.
#' @srrstats {G5.2a} Messages should be unique.
#' @keywords internal
"_PACKAGE"
utils::globalVariables(c(
  ".data", "id", "n", "period", "time", "values"
))

#' @importFrom rlang .data
#' @importFrom rlang %||%
#' @importFrom lubridate %m+% %m-%
#' @importFrom stats nobs
#' @importFrom utils tail
#' @importFrom forecast forecast
NULL

# Objects for re export

#' @export
forecast::forecast
