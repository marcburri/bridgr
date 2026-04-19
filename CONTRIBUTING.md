# Contributing to bridgr

Thanks for your interest in improving `bridgr`.

## Getting Started

Clone the repository and install the development dependencies in R:

``` r
install.packages(c("devtools", "testthat", "roxygen2", "pkgdown"))
```

Then load the package from the project root:

``` r
devtools::load_all()
```

## Development Workflow

- Add or update code in `R/`.
- Add or update tests in `tests/testthat/`.
- Regenerate documentation after roxygen changes with
  `devtools::document()`.
- Rebuild `README.md` from `README.Rmd` after README changes.

## Checks

Run targeted tests while developing:

``` r
testthat::test_dir("tests/testthat")
```

Before opening a pull request, please run:

``` r
devtools::test()
devtools::check()
```

## Pull Requests

- Keep changes focused and explain the motivation clearly.
- Include tests for bug fixes or new behavior when possible.
- Update user-facing documentation when the public API changes.
