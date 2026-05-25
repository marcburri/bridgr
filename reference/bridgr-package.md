# bridgr: Bridging Data Frequencies for Timely Economic Forecasts

`bridgr` provides a unified workflow for bridging high-frequency
indicators to lower-frequency target series, the practical job at the
heart of nowcasting and forecasting macroeconomic variables. It covers
classical bridge equations and MIDAS-style mixed-frequency regressions
(`expalmon`, `beta`, unrestricted / U-MIDAS) under one interface, with
automatic frequency alignment, indicator forecasting, and aggregation.

To learn more about bridgr, start with the vignettes:
`browseVignettes(package = "bridgr")`

## Details

`bridgr` is under active maintenance. The public interface centered on
[`mf_model()`](https://marcburri.github.io/bridgr/reference/mf_model.md),
[`forecast()`](https://generics.r-lib.org/reference/forecast.html),
[`summary()`](https://rdrr.io/r/base/summary.html), and
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) is considered
stable for mixed-frequency nowcasting workflows, while future
development is expected to expand diagnostics and mixed-frequency model
options without changing the core target-and-indicator workflow.

## See also

Useful links:

- <https://github.com/marcburri/bridgr>

- <https://marcburri.github.io/bridgr/>

- Report bugs at <https://github.com/marcburri/bridgr/issues>

## Author

**Maintainer**: Marc Burri <marc.burri91@gmail.com>
([ORCID](https://orcid.org/0000-0001-8974-9090)) \[copyright holder\]
