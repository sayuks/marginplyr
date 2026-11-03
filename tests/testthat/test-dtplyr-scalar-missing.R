# proxy registers dist methods when sf loads; these contracts need the
# ordinary stats dist behavior in a fresh process, including under test_check().
if (identical(Sys.getenv("MARGINPLYR_SCALAR_CONTRACT_PROCESS"), "true")) {
  source(testthat::test_path("fixtures", "dtplyr-scalar-missing-contracts.R"),
         local = TRUE)
} else {
  test_that("dtplyr scalar missing contracts hold in a fresh process", {
    skip_if_suggest_absent("dtplyr")
    package <- getNamespaceInfo(asNamespace("marginplyr"), "path")
    test <- normalizePath(testthat::test_path("test-dtplyr-scalar-missing.R"))
    helper <- normalizePath(testthat::test_path("helper-optional-backends.R"))
    bootstrap <- paste(
      "args <- commandArgs(trailingOnly = TRUE)",
      paste0("Sys.setenv(TESTTHAT_PARALLEL = 'false', ",
             "MARGINPLYR_SCALAR_CONTRACT_PROCESS = 'true')"),
      "if (file.exists(file.path(args[[1L]], 'Meta', 'package.rds'))) {",
      ".libPaths(c(dirname(args[[1L]]), .libPaths()))",
      "library(marginplyr)",
      "source(args[[3L]])",
      paste0("testthat::test_file(args[[2L]], reporter = 'summary', ",
             "stop_on_failure = TRUE)"),
      "} else {",
      paste0("testthat::test_local(args[[1L]], ",
             "filter = '^dtplyr-scalar-missing$', reporter = 'summary', ",
             "stop_on_failure = TRUE)"),
      "}", sep = "
"
    )
    log <- suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"),
      c("--vanilla", "-e", shQuote(bootstrap), shQuote(package),
        shQuote(test), shQuote(helper)), stdout = TRUE, stderr = TRUE
    ))
    expect_identical(attr(log, "status") %||% 0L, 0L,
                     info = paste(log, collapse = "
"))
  })
}
