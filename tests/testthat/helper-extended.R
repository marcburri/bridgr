bridgr_extended_tests_enabled <- function() {
  flag <- Sys.getenv("BRIDGR_EXTENDED_TESTS", unset = "false")
  identical(tolower(flag), "true")
}

skip_if_not_bridgr_extended_tests <- function() {
  testthat::skip_if_not(
    bridgr_extended_tests_enabled(),
    message = "Set BRIDGR_EXTENDED_TESTS=true to run extended tests."
  )
}
