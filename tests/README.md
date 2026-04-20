# Extended Tests

`bridgr` keeps its extended tests inside the ordinary `testthat` suite and
gates them with the `BRIDGR_EXTENDED_TESTS=true` environment variable.

## How to run them

Use ordinary package test commands with the flag enabled, for example:

```sh
BRIDGR_EXTENDED_TESTS=true Rscript -e 'devtools::test(filter = "extended")'
```

## Current scope

The current extended tier exercises a larger bootstrap forecasting workflow in
`tests/testthat/test-extended-bootstrap.R`. It is intended to take longer than
the default unit tests, but it stays offline and does not require downloaded
assets or platform-specific system dependencies.

