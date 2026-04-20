#' Accessor Methods for Mixed-Frequency Models
#'
#' Access standard model summaries from a fitted [mf_model()] object.
#'
#' @param object,x A fitted `"mf_model"` object returned by [mf_model()].
#' @param ... Unused.
#'
#' @return The requested model summary, usually delegated from the stored target
#'   regression fit.
#'
#' @examples
#' gdp_growth <- tsbox::ts_pc(gdp)
#' gdp_growth <- tsbox::ts_na_omit(gdp_growth)
#' model <- mf_model(
#'   target = gdp_growth,
#'   indic = baro,
#'   indic_predict = "auto.arima",
#'   indic_aggregators = "mean",
#'   h = 1
#' )
#'
#' coef(model)
#' @name mf_model-accessors
NULL

#' @rdname mf_model-accessors
#' @srrstats {RE4.2} `coef.mf_model()` returns the fitted coefficients of the stored target regression.
#' @method coef mf_model
#' @export
coef.mf_model <- function(object, ...) {
  stats::coef(object$model, ...)
}

#' @rdname mf_model-accessors
#' @srrstats {RE4.4} `formula.mf_model()` returns the assembled target-regression formula stored on the fitted model object.
#' @method formula mf_model
#' @export
formula.mf_model <- function(x, ...) {
  x$formula
}
