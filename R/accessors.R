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
#' @param parm,level Passed to [confint()]. Confidence intervals are computed
#'   from the coefficient covariance matrix returned by [stats::vcov()].
#' @srrstats {RE4.3} `confint.mf_model()` returns coefficient confidence intervals derived from the model's reported covariance matrix.
#' @method confint mf_model
#' @export
confint.mf_model <- function(object, parm = NULL, level = 0.95, ...) {
  coefficients <- stats::coef(object)
  covariance <- stats::vcov(object)

  if (is.null(parm)) {
    parm_names <- names(coefficients)
  } else if (is.numeric(parm)) {
    parm_names <- names(coefficients)[parm]
  } else {
    parm_names <- parm
  }
  parm_names <- parm_names[!is.na(parm_names)]

  standard_errors <- sqrt(diag(covariance))[parm_names]
  degrees_freedom <- tryCatch(
    stats::df.residual(object$model),
    error = function(...) NA_real_
  )
  critical_value <- if (is.finite(degrees_freedom) && degrees_freedom > 0) {
    stats::qt((1 + level) / 2, df = degrees_freedom)
  } else {
    stats::qnorm((1 + level) / 2)
  }

  intervals <- cbind(
    coefficients[parm_names] - critical_value * standard_errors,
    coefficients[parm_names] + critical_value * standard_errors
  )
  colnames(intervals) <- paste0(
    format(100 * c((1 - level) / 2, 1 - (1 - level) / 2), trim = TRUE),
    " %"
  )

  intervals
}

#' @rdname mf_model-accessors
#' @srrstats {RE4.4} `formula.mf_model()` returns the assembled target-regression formula stored on the fitted model object.
#' @method formula mf_model
#' @export
formula.mf_model <- function(x, ...) {
  x$formula
}

#' @rdname mf_model-accessors
#' @srrstats {RE4.5} `nobs.mf_model()` returns the number of rows used in the fitted target regression.
#' @method nobs mf_model
#' @export
nobs.mf_model <- function(object, ...) {
  stats::nobs(object$model, ...)
}

#' @rdname mf_model-accessors
#' @srrstats {RE4.6} `vcov.mf_model()` returns the stored coefficient covariance matrix when available and otherwise falls back to the underlying target regression fit.
#' @method vcov mf_model
#' @export
vcov.mf_model <- function(object, ...) {
  covariance <- object$uncertainty$coefficient_covariance

  if (!is.null(covariance)) {
    return(covariance)
  }

  stats::vcov(object$model, ...)
}

#' @rdname mf_model-accessors
#' @srrstats {RE4.9} `fitted.mf_model()` returns the in-sample fitted values from the stored target regression.
#' @method fitted mf_model
#' @export
fitted.mf_model <- function(object, ...) {
  stats::fitted(object$model, ...)
}

#' @rdname mf_model-accessors
#' @details `residuals.mf_model()` returns target-equation residuals on the same
#'   standardized scale as the fitted target series, so they can be passed
#'   directly to downstream residual diagnostics.
#' @srrstats {RE4.10} `residuals.mf_model()` returns target-equation residuals on the fitted model scale and documents that they can be used directly in downstream residual diagnostics.
#' @method residuals mf_model
#' @export
residuals.mf_model <- function(object, ...) {
  stats::residuals(object$model, ...)
}

#' @rdname mf_model-accessors
#' @return `x`, invisibly.
#' @srrstats {RE4.17} `print.mf_model()` provides an on-screen summary of model inputs, coefficients, and uncertainty settings through the package's standard summary layout.
#' @method print mf_model
#' @export
print.mf_model <- function(x, ...) {
  summary(x, ...)
  invisible(x)
}
