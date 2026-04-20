#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#'
#' @srrstatsNA {G1.5} Publication-performance code is out of package scope.
#' @srrstatsNA {G1.6} Package-comparison code is out of package scope.
#' @srrstatsNA {G2.4d} No factor inputs require `as.factor()` conversion.
#' @srrstatsNA {G2.4e} No factor inputs require conversion from factor.
#' @srrstatsNA {G2.5} No inputs are expected to be factors.
#' @srrstatsNA {G2.9} Type-loss diagnostics are out of current package scope.
#' @srrstatsNA {G2.11} Non-vector column support is out of current scope.
#' @srrstatsNA {G2.16} Non-finite-value handling is not part of the API.
#' @srrstatsNA {G3.0} Floating-point comparison guarantees are not documented.
#' @srrstatsNA {G3.1a} No arbitrary covariance interface is exposed.
#' @srrstatsNA {G4.0} The package does not write analytical output files.
#' @srrstatsNA {G5.0} Standard-dataset testing is outside the test contract.
#' @srrstatsNA {G5.4c} Tests rely on direct references, not paper outputs.
#' @srrstatsNA {G5.7} Runtime benchmarking is treated as external analysis.
#' @srrstatsNA {G5.9} Noise-susceptibility tests are outside current scope.
#' @srrstatsNA {G5.9a} Epsilon-noise robustness is not part of the test suite.
#' @srrstatsNA {G5.11} Extended tests do not need downloaded assets.
#' @srrstatsNA {G5.11a} Extended tests do not download assets.
#' @srrstatsNA {TS1.4} Attribute preservation beyond current preprocessing
#' is out of scope.
#' @srrstatsNA {TS1.7} `units` input support is out of scope.
#' @srrstatsNA {TS2.0} Ragged-edge workflows allow implicit end gaps.
#' @srrstatsNA {TS2.5} The package does not return time-indexed matrices.
#' @srrstatsNA {TS2.6} The package does not return auto-covariance matrices.
#' @srrstatsNA {TS3.1} Counterexample uncertainty tests are not required here.
#' @srrstatsNA {TS3.3b} Trimming is documented, not exposed as a helper.
#' @srrstatsNA {TS4.0a} Returns custom classes instead of input classes.
#' @srrstatsNA {TS4.1} Unit attributes are not propagated to outputs.
#' @srrstatsNA {TS4.4} Upstream transforms are not modeled internally.
#' @srrstatsNA {TS4.5} Stationarity transforms are handled upstream.
#' @srrstatsNA {TS4.5a} No back-transform helper is provided.
#' @srrstatsNA {TS4.5b} No back-transform workflow is documented.
#' @srrstatsNA {TS4.5c} No transform limitations are documented.
#' @srrstatsNA {TS4.6a} No distribution object is returned.
#' @srrstatsNA {TS4.7c} Forecasts are kept separate, not combined.
#' @srrstatsNA {TS5.4} No frequency-domain plots are provided.
#' @srrstatsNA {TS5.5} Plotting raw missing-value line styles is out of scope.
#' @srrstatsNA {RE1.1} No user-facing formula interface is exposed.
#' @srrstatsNA {RE1.3} Extra attribute retention is out of current scope.
#' @srrstatsNA {RE2.2} Separate predictor/response NA policies are out of scope.
#' @srrstatsNA {RE2.3} Centering and offsetting are treated upstream.
#' @srrstatsNA {RE4.1} The package only constructs fitted model objects.
#' @srrstatsNA {RE4.12} No internal transform/inverse-transform helpers exist.
#' @srrstatsNA {RE4.16} No grouped categorical-response models are fit.
#' @srrstatsNA {RE5.0} Scaling-law benchmarks are treated as external analysis.
#' @srrstatsNA {RE7.0a} Noiseless inputs are accepted for exact recovery.
#' @srrstatsNA {RE7.1a} No noisy-vs-noiseless speed contract is tested.
#' @srrstatsNA {RE7.2} Row-name retention is outside current object design.
#' @noRd
NULL
