#!/usr/bin/env Rscript

# Exercise the verifier's real entry point under setup-r's one-CPU option.
verify_suite_coverage_workers <- function() {
  fixture <- tempfile("marginplyr-suite-workers-")
  dir.create(fixture)
  on.exit(unlink(fixture, recursive = TRUE), add = TRUE)
  scripts <- file.path(fixture, ".github", "scripts")
  tests <- file.path(fixture, "tests", "testthat")
  records <- file.path(fixture, "workers")
  dir.create(scripts, recursive = TRUE)
  dir.create(tests, recursive = TRUE)
  dir.create(records)
  files <- c("verify-suite-coverage.R", "ci-helpers.R")
  stopifnot(all(file.copy(file.path(".github", "scripts", files), scripts)))
  writeLines(c(
    "Package: marginplyrWorkerFixture",
    "Version: 0.0.0",
    "Title: Suite Worker Fixture",
    "Description: Exercises worker selection and result aggregation.",
    "License: MIT",
    "Config/testthat/edition: 3"
  ), file.path(fixture, "DESCRIPTION"))
  writeLines(character(), file.path(fixture, "NAMESPACE"))
  writeLines(c(
    'optional_backends <- function() c("stats", "utils")',
    "suggest_status <- function(package) list(available = TRUE)",
    "suggest_available <- function(package) {",
    '  !package %in% Sys.getenv("MARGINPLYR_HIDE_SUGGESTS")',
    "}"
  ), file.path(tests, "helper-optional-backends.R"))
  for (package in c("stats", "utils")) {
    writeLines(c(
      sprintf('test_that("%s worker inherits absence", {', package),
      '  hidden <- Sys.getenv("MARGINPLYR_HIDE_SUGGESTS")',
      "  expect_true(hidden %in% optional_backends())",
      "  expect_false(suggest_available(hidden))",
      "  present <- setdiff(optional_backends(), hidden)",
      "  expect_true(suggest_available(present))",
      '  file.create(file.path("..", "..", "workers",',
      '                        paste0(hidden, "-", Sys.getpid())))',
      "})",
      sprintf('test_that("%s contract runs", {', package),
      sprintf('  if (!suggest_available("%s")) {', package),
      sprintf('    skip("{%s} is not installed")', package),
      "  }",
      "  expect_true(TRUE)",
      "})"
    ), file.path(tests, paste0("test-", package, ".R")))
  }

  previous <- setwd(fixture)
  on.exit(setwd(previous), add = TRUE, after = FALSE)
  output <- system2(file.path(R.home("bin"), "Rscript"), c(
    "-e", shQuote(paste0(
      "options(Ncpus = 1L); ",
      "source('.github/scripts/verify-suite-coverage.R')"
    ))
  ), stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop(paste(output, collapse = "\n"), call. = FALSE)
  }
  if (!any(grepl("Verified: all 4 tests execute", output, fixed = TRUE))) {
    stop("The verifier did not aggregate all fixture tests.", call. = FALSE)
  }
  for (hidden in c("stats", "utils")) {
    workers <- list.files(records, pattern = paste0("^", hidden, "-"))
    if (length(workers) != 2L) {
      stop(sprintf(
        "Expected two workers with %s hidden, observed %d.\n%s",
        hidden, length(workers), paste(output, collapse = "\n")
      ), call. = FALSE)
    }
  }
  message(paste0(
    "Verified: both configurations used two workers for two files, ",
    "inherited absence settings, and retained all results."
  ))
}

verify_suite_coverage_workers()
