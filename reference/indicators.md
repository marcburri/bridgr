# Indicator Names of a Mixed-Frequency Model

Returns the identifiers of the high-frequency indicators used by a
fitted
[`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md)
object, in the order they enter the bridge equation.

## Usage

``` r
indicators(object, ...)

# S3 method for class 'mf_model'
indicators(object, ...)
```

## Arguments

- object:

  A fitted `"mf_model"` object returned by
  [`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md).

- ...:

  Unused.

## Value

A character vector of indicator identifiers.

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
  indic_predict = "auto.arima",
  indic_aggregators = "mean",
  h = 1
)

indicators(model)
#> [1] "baro"
```
