#' @description
#' To learn more about bridgr, start with the vignettes:
#' `browseVignettes(package = "bridgr")`
#' @details
#' `bridgr` is under active maintenance. The public interface centered on
#' [mf_model()], [forecast()], [summary()], and [plot()] is considered stable
#' for bridge-style nowcasting workflows, while future development is expected
#' to expand diagnostics and mixed-frequency model options without changing the
#' core target-and-indicator workflow.
#' @srrstats {G1.2} Package-level documentation includes an explicit lifecycle statement describing the current stable interface and the anticipated direction of future development.
#' @srrstats {G1.4} Package functions are documented with roxygen2 blocks in `R/`, and those blocks generate the installed reference documentation.
#' @srrstats {G1.4a} Internal helpers in `R/` are also documented with roxygen2 and are suppressed from standalone help pages with `@noRd` and `@keywords internal`.
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
