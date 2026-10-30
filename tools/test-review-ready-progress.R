# Exercises the real reporter and process boundary with a small disposable
# package. Run from the repository root with the review dependencies installed.
source("tools/review-ready-check-lib.R")
source("tools/review-ready-progress.R")

local({
  root <- normalizePath(".", winslash = "/")
  workspace <- tempfile("review-ready-progress-smoke-")
  package <- file.path(workspace, "fixture")
  dir.create(file.path(package, "tests", "testthat"), recursive = TRUE)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  writeLines(c(
    "Package: fixture", "Version: 0.0.1", "Title: Progress Fixture",
    "Description: Exercises live progress during source package checks.",
    'Authors@R: person("A", "B", email="a@example.com", role=c("aut", "cre"))',
    "License: GPL-3", "Suggests: testthat, withr", "Config/testthat/edition: 3"
  ), file.path(package, "DESCRIPTION"))
  writeLines(character(), file.path(package, "NAMESPACE"))
  writeLines(gsub("marginplyr", "fixture", readLines("tests/testthat.R"), fixed = TRUE),
    file.path(package, "tests", "testthat.R"))
  writeLines('test_that("first", expect_true(TRUE))',
    file.path(package, "tests", "testthat", "test-first.R"))
  repos <- review_ready_offline_repositories(workspace)
  for (mode in c("pass", "fail", "worker", "teardown", "startup")) {
    fail <- mode != "pass"
    helper <- file.path(package, "tests", "testthat", "helper-worker-startup.R")
    if (mode == "startup") writeLines('stop("fixture worker startup failure")', helper)
    expression <- switch(mode,
      pass = 'test_that("second", expect_true(TRUE))',
      fail = 'test_that("second", expect_true(FALSE))',
      worker = 'test_that("worker exits", quit(status = 17L, save = "no"))',
      teardown = c(
        'withr::defer(stop("fixture teardown failure"), envir = .GlobalEnv)',
        'test_that("second", expect_true(TRUE))'
      ),
      startup = 'test_that("second", expect_true(TRUE))'
    )
    writeLines(expression, file.path(package, "tests", "testthat", "test-second.R"))
    check_dir <- file.path(workspace, mode)
    output <- capture.output({
      result <- review_ready_progress_check(
        package, args = c("--no-manual", "--no-vignettes"), build_args = character(),
        check_dir = check_dir, error_on = "never", repos = repos,
        env = c(
          TESTTHAT_PARALLEL = "true", TESTTHAT_CPUS = "4", NOT_CRAN = "true",
          MARGINPLYR_REVIEW_PROGRESS_REPORTER = file.path(
            root, "tools", "review-ready-progress-reporter.R"
          )
        )
      )
    })
    # Four requested workers and two files distinguish selection from configuration.
    stopifnot(
      "Starting 2 test processes." %in% trimws(output),
      identical(result$status == 0L, !fail),
      identical(length(result$errors) == 0L, !fail)
    )
    if (mode == "worker") {
      stopifnot(
        any(grepl("testthat subprocess exited in file 'test-second.R'", output, fixed = TRUE)),
        any(grepl("R session crashed with exit code 17", output, fixed = TRUE))
      )
      next
    }
    if (mode == "teardown") {
      stopifnot(
        any(grepl("At least one parallel worker failed to run teardown", output, fixed = TRUE)),
        any(grepl("fixture teardown failure", output, fixed = TRUE))
      )
      next
    }
    if (mode == "startup") {
      stopifnot(
        any(grepl("testthat subprocess failed to start", output, fixed = TRUE)),
        any(grepl("fixture worker startup failure", output, fixed = TRUE))
      )
      next
    }
    events <- readLines(file.path(check_dir, "test-progress.tsv"))
    stopifnot(
      identical(events[[1L]], "total\t2"),
      setequal(events[startsWith(events, "done\t")],
        c("done\ttest-first.R", "done\ttest-second.R")),
      identical(tail(events, 1L), "finished")
    )
    logs <- list.files(result$checkdir, "testthat[.]Rout", recursive = TRUE,
      full.names = TRUE)
    tally <- if (fail) "[ FAIL 1 | WARN 0 | SKIP 0 | PASS 1 ]" else {
      "[ FAIL 0 | WARN 0 | SKIP 0 | PASS 2 ]"
    }
    stopifnot(length(logs) == 1L, tally %in% trimws(readLines(logs)))
  }
  cat("Real progress reporter and process checks passed.\n")
})
