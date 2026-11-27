# stringr is a Suggest, so the walk recognizes a bare `str_glue` only as a
# function its loaded namespace holds. A fresh process starts without it, so
# this file reruns itself in one, under `test_local()` where the package is a
# source tree and against the installed package otherwise.
if (identical(Sys.getenv("MARGINPLYR_STR_GLUE_PROCESS"), "true")) {
  test_that("a caller's own str_glue() does not load stringr", {
    before <- isNamespaceLoaded("stringr")
    str_glue <- function(...) "z"
    data <- data.frame(g = c("a", "b"), x = c(1, 3))
    result <- summarize_with_margins(
      data, n = sum(x), s = share_of_total(n), m = nchar(str_glue("a")),
      .grouping = rollup(g)
    )
    expect_false(before)
    expect_false(isNamespaceLoaded("stringr"))
    expect_identical(result$m, rep(1L, 3L))
  })
} else {
  test_that("str_glue() is checked in a fresh process", {
    package <- getNamespaceInfo(asNamespace("marginplyr"), "path")
    test <- normalizePath(testthat::test_path("test-share-str-glue-process.R"))
    bootstrap <- paste(
      "args <- commandArgs(trailingOnly = TRUE)",
      paste0("Sys.setenv(TESTTHAT_PARALLEL = 'false', ",
             "MARGINPLYR_STR_GLUE_PROCESS = 'true')"),
      "if (file.exists(file.path(args[[1L]], 'Meta', 'package.rds'))) {",
      ".libPaths(c(dirname(args[[1L]]), .libPaths()))",
      "library(marginplyr)",
      paste0("testthat::test_file(args[[2L]], reporter = 'summary', ",
             "stop_on_failure = TRUE)"),
      "} else {",
      paste0("testthat::test_local(args[[1L]], ",
             "filter = '^share-str-glue-process$', reporter = 'summary', ",
             "stop_on_failure = TRUE)"),
      "}", sep = "\n"
    )
    log <- suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"),
      c("--vanilla", "-e", shQuote(bootstrap), shQuote(package), shQuote(test)),
      stdout = TRUE, stderr = TRUE
    ))
    expect_identical(attr(log, "status") %||% 0L, 0L,
                     info = paste(log, collapse = "\n"))
  })
}
