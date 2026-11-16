#!/usr/bin/env Rscript

# Exercise the verifier's real entry points under setup-r's one-CPU option:
# the local run over every configuration, and the per-configuration and
# aggregate modes the `structure` matrix legs and their aggregate job call.
verify_suite_coverage_workers <- function() {
  fixture <- tempfile("marginplyr-suite-workers-")
  dir.create(fixture)
  on.exit(unlink(fixture, recursive = TRUE), add = TRUE)
  scripts <- file.path(fixture, ".github", "scripts")
  tests <- file.path(fixture, "tests", "testthat")
  records <- file.path(fixture, "workers")
  rows <- file.path(fixture, "rows")
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
  helper <- file.path(tests, "helper-optional-backends.R")
  write_helper <- function(backends) {
    writeLines(c(
      sprintf(
        "optional_backends <- function() c(%s)",
        paste(sprintf('"%s"', backends), collapse = ", ")
      ),
      "suggest_status <- function(package) list(available = TRUE)",
      "suggest_available <- function(package) {",
      '  hidden <- strsplit(Sys.getenv("MARGINPLYR_HIDE_SUGGESTS"), ",")[[1]]',
      "  !package %in% hidden",
      "}"
    ), helper)
  }
  write_helper(c("stats", "utils"))
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
  profile <- file.path(fixture, "one-cpu.R")
  writeLines("options(Ncpus = 1L)", profile)

  previous <- setwd(fixture)
  on.exit(setwd(previous), add = TRUE, after = FALSE)
  verifier <- function(...) {
    output <- suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"),
      c(".github/scripts/verify-suite-coverage.R", ...),
      stdout = TRUE, stderr = TRUE,
      env = sprintf("R_PROFILE_USER=%s", shQuote(profile))
    ))
    status <- attr(output, "status")
    list(status = if (is.null(status)) 0L else status, output = output)
  }
  succeeds <- function(result) {
    if (result$status != 0L) {
      stop(paste(result$output, collapse = "\n"), call. = FALSE)
    }
    result
  }
  fails_naming <- function(result, fragment) {
    named <- any(grepl(fragment, result$output, fixed = TRUE))
    if (result$status == 0L || !named) {
      stop(sprintf(
        "Expected a failure naming '%s'.\n%s",
        fragment, paste(result$output, collapse = "\n")
      ), call. = FALSE)
    }
    result
  }
  expect_two_workers_each <- function(output) {
    for (hidden in c("stats", "utils")) {
      workers <- list.files(records, pattern = paste0("^", hidden, "-"))
      if (length(workers) != 2L) {
        stop(sprintf(
          "Expected two workers with %s hidden, observed %d.\n%s",
          hidden, length(workers), paste(output, collapse = "\n")
        ), call. = FALSE)
      }
    }
    unlink(list.files(records, full.names = TRUE))
  }
  # Everything after the suites ran: the summary counts are part of the verdict.
  verdict <- function(result) {
    grep(
      "^Starting [0-9]+ test processes", result$output,
      value = TRUE, invert = TRUE
    )
  }
  aggregate <- function(directory = "rows") {
    verifier("--aggregate", sprintf("--rows=%s", directory))
  }
  expect_same_verdict <- function(local, aggregated) {
    if (!identical(verdict(aggregated), verdict(local))) {
      stop(sprintf(
        "The aggregate verdict differs from the local one.\nLocal: %s\n%s",
        paste(verdict(local), collapse = "\n"),
        paste(verdict(aggregated), collapse = "\n")
      ), call. = FALSE)
    }
  }
  observe_each <- function() {
    unlink(rows, recursive = TRUE)
    for (backend in c("stats", "utils")) {
      succeeds(verifier(sprintf("--observe=%s", backend), "--rows=rows"))
    }
  }

  local <- succeeds(verifier())
  if (!any(grepl("Verified: all 4 tests execute", local$output))) {
    stop("The verifier did not aggregate all fixture tests.", call. = FALSE)
  }
  expect_two_workers_each(local$output)

  # One process per configuration, as the matrix legs run it.
  observe_each()
  expect_two_workers_each("per-configuration runs")
  expect_same_verdict(local, succeeds(aggregate()))

  # A leg whose rows never arrived.
  leg <- file.path(rows, "utils.rds")
  saved <- file.path(fixture, "utils.rds")
  stopifnot(file.copy(leg, saved))
  unlink(leg)
  fails_naming(aggregate(), "No result for the utils")
  fails_naming(aggregate("missing"), "holds no configuration results")

  # Two results for one configuration.
  stopifnot(file.copy(saved, leg))
  again <- file.path(rows, "again.rds")
  stopifnot(file.copy(file.path(rows, "stats.rds"), again))
  fails_naming(aggregate(), "More than one result for the stats")
  unlink(again)

  # A backend added to the table without a leg that observed it.
  write_helper(c("stats", "utils", "tools"))
  fails_naming(aggregate(), "No result for the tools")
  write_helper(c("stats", "utils"))

  # A failing verdict is the same verdict in both modes.
  writeLines(c(
    'test_that("both backends at once", {',
    '  if (!suggest_available("stats")) skip("{stats} is not installed")',
    '  if (!suggest_available("utils")) skip("{utils} is not installed")',
    "  expect_true(TRUE)",
    "})"
  ), file.path(tests, "test-both.R"))
  local <- fails_naming(verifier(), "test-both.R")
  observe_each()
  expect_same_verdict(local, fails_naming(aggregate(), "test-both.R"))

  message(paste0(
    "Verified: every configuration used two workers for two files and ",
    "inherited absence settings, the aggregate verdict matched the local ",
    "one, and missing, duplicate, and unobserved configurations failed."
  ))
}

verify_suite_coverage_workers()
