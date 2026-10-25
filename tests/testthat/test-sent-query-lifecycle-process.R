# Independent processes keep lifecycle's session-frequency state out of both
# the unaudited control and the test runner's warning settings.
sent_query_lifecycle_process <- function(case, audited, warning_level,
                                         verbosity = "unset") {
  output <- tempfile(fileext = ".rds")
  on.exit(unlink(output), add = TRUE)
  libraries <- .libPaths()
  installed <- libraries[file.exists(file.path(
    libraries, "marginplyr", "Meta", "package.rds"
  ))][[1L]]
  args <- c(
    "--vanilla",
    shQuote(testthat::test_path("fixtures", "sent-query-lifecycle.R")),
    case, if (audited) "true" else "false", warning_level,
    shQuote(output), shQuote(installed),
    shQuote(testthat::test_path("helper-duckdb.R")), verbosity
  )
  log <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), args, stdout = TRUE, stderr = TRUE
  ))
  expect_identical(attr(log, "status") %||% 0L, 0L,
                   info = paste(log, collapse = "\n"))
  readRDS(output)
}

expect_lifecycle_audit <- function(case) {
  for (warning_level in c(0L, 2L)) {
    off <- sent_query_lifecycle_process(case, FALSE, warning_level)
    on <- sent_query_lifecycle_process(case, TRUE, warning_level)
    expect_length(off$warnings, 2L)
    expect_true(all(vapply(off$warnings, function(cnd) {
      "lifecycle_warning_deprecated" %in% cnd$classes
    }, logical(1))))
    expect_match(off$warnings[[1L]]$message, "ignore_case", fixed = TRUE)
    expect_identical(off$returned, warning_level == 0L)
    expect_identical(on$returned, off$returned)
    expect_identical(on$warnings, off$warnings)
    expect_identical(on$error, off$error)
    expect_true(on$options_restored)
    expect_true(off$options_restored)
    expect_true(on$stable)
    expect_identical(on$accessor_warnings, on$warnings)
    expect_identical(on$plain, on$details[c("purpose", "sql")])
    expect_identical(on$internal, on$details[on$details$purpose != "result", ])
    expect_false(anyNA(on$details$sql))
    expect_match(on$internal$sql, "SELECT", fixed = TRUE)
    expect_identical(on$internal$status,
                     if (warning_level == 0L) "returned" else "errored")
    observed <- on$internal$conditions[[1L]]
    expect_true(all(observed$phase == "invocation"))
    expect_true(all(observed$handling == "propagated"))
    expect_identical(
      observed$classes[1:2], lapply(on$warnings, `[[`, "classes")
    )
    expect_identical(
      observed$message[1:2], vapply(on$warnings, `[[`, "", "message")
    )
    if (warning_level == 2L) {
      expect_true("error" %in% observed$classes[[3L]])
      expect_match(
        observed$message[[3L]], "converted from warning", fixed = TRUE
      )
    }
    expect_length(on$subsequent_warnings, 0L)
    expect_identical(on$subsequent_warnings, off$subsequent_warnings)
    expect_true(on$subsequent_returned)
    expect_identical(on$subsequent_returned, off$subsequent_returned)
  }
}

test_that("audit preserves lifecycle warnings during SQLite label checking", {
  skip_if_suggest_absent("RSQLite", "DBI")
  expect_lifecycle_audit("collision")
})

test_that("audit preserves lifecycle warnings during DuckDB Margin metadata", {
  skip_if_suggest_absent("duckdb", "DBI")
  expect_lifecycle_audit("metadata")
})

test_that("audit preserves lifecycle warnings during DuckDB plan inspection", {
  skip_if_suggest_absent("duckdb", "DBI")
  expect_lifecycle_audit("inspection")
})

test_that("audit restores explicit lifecycle warning settings", {
  skip_if_suggest_absent("RSQLite", "DBI")
  for (verbosity in c("default", "quiet", "warning", "error")) {
    off <- sent_query_lifecycle_process("collision", FALSE, 0L, verbosity)
    on <- sent_query_lifecycle_process("collision", TRUE, 0L, verbosity)
    expect_identical(on$returned, off$returned)
    expect_identical(on$error, off$error)
    expect_identical(on$warnings, off$warnings)
    expect_identical(on$subsequent_warnings, off$subsequent_warnings)
    expect_identical(on$subsequent_returned, off$subsequent_returned)
    expect_true(on$options_restored)
  }
})
