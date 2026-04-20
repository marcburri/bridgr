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
#'
#' @srrstatsTODO {G1.5} *Software should include all code necessary to reproduce results which form the basis of performance claims made in associated publications.* 
#' @srrstatsTODO {G1.6} *Software should include code necessary to compare performance claims with alternative implementations in other R packages.* 
#' @srrstatsTODO {G2.9} *Software should issue diagnostic messages for type conversion in which information is lost (such as conversion of variables from factor to character; standardisation of variable names; or removal of meta-data such as those associated with [`sf`-format](https://r-spatial.github.io/sf/) data) or added (such as insertion of variable or column names where none were provided).* 
#' @srrstatsTODO {G2.11} *Software should ensure that `data.frame`-like tabular objects which have columns which do not themselves have standard class attributes (typically, `vector`) are appropriately processed, and do not error without reason. This behaviour should be tested. Again, columns created by the [`units` package](https://github.com/r-quantities/units/) provide a good test case.*
#' @srrstatsTODO {G2.15} *Functions should never assume non-missingness, and should never pass data with potential missing values to any base routines with default `na.rm = FALSE`-type parameters (such as [`mean()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/mean.html), [`sd()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/sd.html) or [`cor()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.html)).*
#' @srrstatsTODO {G2.16} *All functions should also provide options to handle undefined values (e.g., `NaN`, `Inf` and `-Inf`), including potentially ignoring or removing such values.* 
#' @srrstatsTODO {G3.0} *Statistical software should never compare floating point numbers for equality. All numeric equality comparisons should either ensure that they are made between integers, or use appropriate tolerances for approximate equality.* 
#' @srrstatsTODO {G5.0} *Where applicable or practicable, tests should use standard data sets with known properties (for example, the [NIST Standard Reference Datasets](https://www.itl.nist.gov/div898/strd/), or data sets provided by other widely-used R packages).*
#' @srrstatsTODO {G5.2} *Appropriate error and warning behaviour of all functions should be explicitly demonstrated through tests. In particular,*
#' @srrstatsTODO {G5.2a} *Every message produced within R code by `stop()`, `warning()`, `message()`, or equivalent should be unique*
#' @srrstatsTODO {G5.2b} *Explicit tests should demonstrate conditions which trigger every one of those messages, and should compare the result with expected values.*
#' 
#' @srrstatsTODO {G5.7} **Algorithm performance tests** *to test that implementation performs as expected as properties of data change. For instance, a test may show that parameters approach correct estimates within tolerance as data size increases, or that convergence times decrease for higher convergence thresholds.*
#' 
#' @srrstatsTODO {G5.9} **Noise susceptibility tests** *Packages should test for expected stochastic behaviour, such as through the following conditions:*
#' @srrstatsTODO {G5.9a} *Adding trivial noise (for example, at the scale of `.Machine$double.eps`) to data does not meaningfully change results*
#' @srrstatsTODO {G5.9b} *Running under different random seeds or initial conditions does not meaningfully change results* 
#' @srrstatsTODO {TS1.4} *The pre-processing function described above should maintain all time- or date-based components or attributes of input data.* 
#' @srrstatsTODO {TS1.7} *Accept inputs defined via the [`units` package](https://github.com/r-quantities/units/) for attributing SI units to R vectors.*
#' @srrstatsTODO {TS3.1} *If possible, provide at least one test which violates TS3.0*
#' @srrstatsTODO {TS4.1} *Any units included as attributes of input data should also be included within return values.*
#' @srrstatsTODO {RE1.3} *Regression Software which passes or otherwise transforms aspects of input data onto output structures should ensure that those output structures retain all relevant aspects of input data, notably including row and column names, and potentially information from other `attributes()`.*
#' @srrstatsTODO {RE2.2} *Regression Software should provide different options for processing missing values in predictor and response data. For example, it should be possible to fit a model with no missing predictor data in order to generate values for all associated response points, even where submitted response values may be missing.*
#' @srrstatsTODO {RE2.3} *Where applicable, Regression Software should enable data to be centred (for example, through converting to zero-mean equivalent values; or to z-scores) or offset (for example, to zero-intercept equivalent values) via additional parameters, with the effects of any such parameters clearly documented and tested.*
#' @srrstatsTODO {RE5.0} *Scaling relationships between sizes of input data (numbers of observations, with potential extension to numbers of variables/columns) and speed of algorithm.* 
#' 
#' @srrstatsTODO {RE7.1a} *In particular, these tests should confirm that model fitting is at least as fast or (preferably) faster than testing with equivalent noisy data (see RE2.4b).* 
#' @srrstatsTODO {RE7.2} Demonstrate that output objects retain aspects of input data such as row or case names (see **RE1.3**).
#' 
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#' @srrstatsNA {G2.4d} Not applicable because `bridgr` does not use factor-valued analytical inputs or require factor coercion in its mixed-frequency workflow. *explicit conversion to factor via `as.factor()`*
#' @srrstatsNA {G2.4e} Not applicable because `bridgr` does not use factor-valued analytical inputs or require factor coercion in its mixed-frequency workflow. *explicit conversion from factor via `as...()` functions*
#' @srrstatsNA {G2.5} Not applicable because `bridgr` does not define any inputs that are expected to be of factor type. *Where inputs are expected to be of `factor` type, secondary documentation should explicitly state whether these should be `ordered` or not, and those inputs should provide appropriate error or other routines to ensure inputs follow these expectations.* 
#' @srrstatsNA {G4.0} Not applicable because `bridgr` does not provide interfaces that write analytical outputs to local files. *Statistical Software which enables outputs to be written to local files should parse parameters specifying file names to ensure appropriate file suffixes are automatically generated where not provided.* 
#' @srrstatsNA {G5.4c} Not applicable here because the package's main correctness checks rely on direct implementations, exact simulated constructions, or closed-form reference calculations rather than stored values from published papers. *Where applicable, stored values may be drawn from published paper outputs when applicable and where code from original implementations is not available*
#' @srrstatsNA {G5.11} Not applicable because the current test suite does not depend on downloaded external assets or large auxiliary data. *Where extended tests require large data sets or other assets, these should be provided for downloading and fetched as part of the testing workflow.*
#' @srrstatsNA {G5.11a} Not applicable because the current test suite does not download external assets for testing. *When any downloads of additional data necessary for extended tests fail, the tests themselves should not fail, rather be skipped and implicitly succeed with an appropriate diagnostic message.*
#' @srrstatsNA {G3.1a} Not applicable because `bridgr` exposes a fixed set of built-in covariance-estimation routes rather than an interface for arbitrarily user-specified covariance estimators. *The ability to use arbitrarily specified covariance methods should be documented (typically in examples or vignettes).* 
#' @srrstatsNA {TS2.0} Not applicable because `bridgr` is explicitly designed for ragged-edge nowcasting workflows that complete implicit end-of-sample gaps in higher-frequency indicators rather than requiring all missingness to be represented explicitly. *Time Series Software which presumes or requires regular data should only allow **explicit** missing values, and should issue appropriate diagnostic messages, potentially including errors, in response to any **implicit** missing values.*
#' @srrstatsNA {TS2.6} Not applicable because `bridgr` does not return auto-covariance matrices or similar matrix objects with unit metadata. *Where applicable, auto-covariance matrices should also include specification of appropriate units.* 
#' @srrstatsNA {TS2.5} Not applicable because `bridgr` does not return square time-indexed matrix objects whose row and column order need to mirror an underlying time-series index; aligned data are kept as long tables ordered directly by `time`. *Incorporate a system to ensure that both row and column orders follow the same ordering as the underlying time series data. This may, for example, be done by including the `index` attribute of the time series data as an attribute of the auto-covariance matrix.*
#' @srrstatsNA {TS4.0a} Not applicable because `bridgr` satisfies the return-value standard through dedicated custom classes (`\"mf_model\"` and `\"mf_model_forecast\"`) rather than by converting outputs back into the original input class. *Be in same class as input data, for example by using the [`tsbox` package](https://www.tsbox.help/) to re-convert from standard internal format (see 1.4, above); or*
#' @srrstatsNA {TS4.5} Not applicable because `bridgr` does not internally transform submitted series to stationary equivalents before fitting or forecasting; any such transformations are expected to happen upstream. *In decreasing order of preference, either:*
#' @srrstatsNA {TS4.5a} Not applicable because `bridgr` does not internally transform submitted series to stationary equivalents before fitting or forecasting. *Provide explicit routines or options to back-transform data commensurate with original, non-stationary input data*
#' @srrstatsNA {TS4.5b} Not applicable because `bridgr` does not internally transform submitted series to stationary equivalents before fitting or forecasting. *Demonstrate how data may be back-transformed to a form commensurate with original, non-stationary input data.*
#' @srrstatsNA {TS4.5c} Not applicable because `bridgr` does not internally transform submitted series to stationary equivalents before fitting or forecasting. *Document associated limitations on forecast values* 
#' @srrstatsNA {TS4.4} Not applicable because `bridgr` does not internally transform submitted series before forecasting; any differences, growth rates, or other stationarity-inducing transforms are expected to happen upstream. *Document the effect of any such transformations on forecast data, including potential effects on both first- and second-order estimates.*
#' @srrstatsNA {TS4.6a} Not applicable because `bridgr` satisfies the forecasting-return standard through point forecasts plus explicit standard errors and interval matrices rather than through a distribution object. *A distribution object, for example via one of the many packages described in the CRAN Task View on [Probability Distributions](https://cran.r-project.org/web/views/Distributions.html) (or the new [`distributional` package](https://pkg.mitchelloharawild.com/distributional/) as used in the [`fable` package](https://fable.tidyverts.org) for time-series forecasting).*
#' @srrstatsNA {TS4.7c} Not applicable because `bridgr` distinguishes observed and forecast values through separate components rather than by returning one combined object with a type column. *Combining model and forecast values into a single return object with an appropriate additional column clearly distinguishing the two kinds of data.* 
#' @srrstatsNA {TS3.3b} Not applicable because `bridgr` satisfies the forecast-trimming standard through documented guidance in the uncertainty vignette rather than through a dedicated trimming helper or primary-function parameter. *Provide an explicit mechanism to trim forecast values to a specified error margin, either via an explicit post-processing function, or via an input parameter to a primary analytic function.* 
#' @srrstatsNA {TS5.5} Not applicable because `plot.mf_model()` only works with fitted-model and forecast outputs after missing data have already been rejected, dropped, or imputed during preprocessing, so it does not expose a raw missing-data line style choice. *Provide options to determine whether plots of data with missing values should generate continuous or broken lines.* 
#' @srrstatsNA {TS5.4} Not applicable because `bridgr` does not provide frequency-domain visualizations such as periodograms or spectral plots. *For frequency visualization, abscissa spanning `[-\\pi, \\pi]` should be avoided in favour of positive units of `[0, 2\\pi]` or `[0, 0.5]`, in all cases with appropriate additional explanation of units.*
#' @srrstatsNA {RE1.1} Not applicable because `bridgr` does not expose a user-facing formula interface; mixed-frequency inputs are supplied as separate `target` and `indic` series and only then assembled into an internal regression formula. *Regression Software should document how formula interfaces are converted to matrix representations of input data.* 
#' @srrstatsNA {RE4.12} Not applicable because `bridgr` does not internally transform response or predictor inputs before estimation; any such transformations are expected to happen upstream. *Where appropriate, functions used to transform input data, and associated inverse transform functions.* 
#' @srrstatsNA {RE4.1} Not applicable because `bridgr` intentionally constructs only fitted bridge-model objects; alignment, indicator completion, and uncertainty metadata are all defined in relation to an actual fitted specification rather than a deferred placeholder object. *Regression Software may enable an ability to generate a model object without actually fitting values. This may be useful for controlling batch processing of computationally intensive fitting algorithms.* 
#' @srrstatsNA {RE4.16} Not applicable because `bridgr` does not fit grouped categorical-response models; it works with numeric mixed-frequency target and indicator series. *Regression Software which models distinct responses for different categorical groups should include the ability to submit new groups to `predict()` methods.* 
#' @srrstatsNA {RE7.0a} Not applicable because the package intentionally accepts noiseless simulated inputs for exact recovery checks instead of rejecting them. In particular, these tests should confirm ability to reject perfectly noiseless input data.
#' @noRd
NULL
