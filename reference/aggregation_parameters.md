# Estimated Parametric Aggregation Parameters

Extracts the estimated parameters of parametric aggregation schemes
(`"expalmon"`, `"beta"`) from a fitted
[`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md)
object. These are the parameters whose implied weights are returned by
[`weights.mf_model()`](https://marcburri.github.io/bridgr/reference/weights.mf_model.md).

## Usage

``` r
aggregation_parameters(object, indicator = NULL, ...)

# S3 method for class 'mf_model'
aggregation_parameters(object, indicator = NULL, ...)
```

## Arguments

- object:

  A fitted `"mf_model"` object returned by
  [`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md).

- indicator:

  Optional indicator selector, either an indicator name or a position.
  When supplied, the parameters for that single indicator are returned
  directly; when `NULL` (default), a named list is returned.

- ...:

  Unused.

## Value

When `indicator` is `NULL`, a named list with one element per indicator
that used a parametric aggregator, each a numeric parameter vector. When
`indicator` is supplied, that single element, or `NULL` when the
indicator did not use a parametric aggregator.

## See also

[`weights.mf_model()`](https://marcburri.github.io/bridgr/reference/weights.mf_model.md)
for the implied aggregation weights.

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

aggregation_parameters(model)
#> $baro
#> [1]  -4.913616 -10.000000
#> 
```
