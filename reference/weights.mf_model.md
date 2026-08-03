# Aggregation Weights of a Mixed-Frequency Model

Extracts the within-period aggregation weights applied to each
indicator. For parametric aggregators (`"expalmon"`, `"beta"`) these are
the weights implied by the estimated parameters; for user-supplied
numeric aggregators they are the supplied weights.

## Usage

``` r
# S3 method for class 'mf_model'
weights(object, indicator = NULL, ...)
```

## Arguments

- object:

  A fitted `"mf_model"` object returned by
  [`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md).

- indicator:

  Optional indicator selector, either an indicator name or a position.
  When supplied, the weights for that single indicator are returned
  directly; when `NULL` (default), a named list covering every indicator
  is returned.

- ...:

  Unused.

## Value

When `indicator` is `NULL`, a named list with one element per indicator,
each either a numeric weight vector or `NULL` for indicators whose
aggregator does not imply a fixed weight vector (`"unrestricted"`). When
`indicator` is supplied, that single element.

## Details

The deterministic rules imply the fixed weight vectors of \\\tilde x_t =
\sum_m w_m x\_{t,m}\\: `"mean"` gives \\w_m = 1/M\\, `"last"` gives
\\w_M = 1\\ and zero elsewhere, and `"sum"` gives \\w_m = 1\\. These are
returned alongside the estimated and user-supplied weights, so the
accessor reports the weights actually applied whichever aggregator was
chosen. `"unrestricted"` has no single weight vector, because it
estimates one coefficient per within-period observation instead of
aggregating; it returns `NULL`, as does an indicator aligned with
`indic_predict = "direct"`.

## See also

[`aggregation_parameters()`](https://marcburri.github.io/bridgr/reference/aggregation_parameters.md)
for the underlying parametric parameters, and
[`indicators()`](https://marcburri.github.io/bridgr/reference/indicators.md)
for the available indicator names.

## Examples

``` r
gdp_growth <- tsbox::ts_pc(gdp)
#> [value]: 'values' 
#> [value]: 'values' 
gdp_growth <- tsbox::ts_na_omit(gdp_growth)
#> [value]: 'values' 
model <- mf_model(
  target = gdp_growth,
  indic = baro,
  indic_aggregators = "expalmon",
  h = 1
)

weights(model)
#> $baro
#> [1] 6.142366e-03 9.938573e-01 3.314536e-07
#> 
weights(model, indicator = 1)
#> [1] 6.142366e-03 9.938573e-01 3.314536e-07
```
