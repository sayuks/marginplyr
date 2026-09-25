# dbplyr suppresses the missing-value warning under TESTTHAT=true. Run each
# option in its own R process with that variable unset, so the warning's
# frequency state starts fresh and the real call can still raise under warn=2.
sent_query_warning_process <- function(case, audited) {
  output <- tempfile(fileext = ".rds")
  on.exit(unlink(output), add = TRUE)
  script <- testthat::test_path("fixtures", "sent-query-warning.R")
  helper <- testthat::test_path("helper-duckdb.R")
  command <- file.path(R.home("bin"), "Rscript")
  libraries <- .libPaths()
  installed <- libraries[file.exists(file.path(
    libraries, "marginplyr", "Meta", "package.rds"
  ))][[1L]]
  args <- c(
    "--vanilla", shQuote(script), case,
    if (audited) "true" else "false",
    shQuote(output), shQuote(installed), shQuote(helper)
  )
  log <- suppressWarnings(system2(command, args, stdout = TRUE, stderr = TRUE))
  expect_identical(
    attr(log, "status") %||% 0L, 0L,
    info = paste(log, collapse = "\n")
  )
  readRDS(output)
}

test_that("audit preserves a deferred result warning failure", {
  skip_if_suggest_absent("RSQLite", "DBI")

  off <- sent_query_warning_process("result", FALSE)
  on <- sent_query_warning_process("result", TRUE)

  expect_match(
    off$result$error, "Missing values are always removed", fixed = TRUE
  )
  expect_identical(on$result$error, off$result$error)
  expect_identical(on$record$purpose, "result")
  expect_false(is.na(on$record$sql))
  expect_match(on$record$sql, "SELECT", fixed = TRUE)
  expect_true(on$warning_verbosity_restored)
})

test_that("audit preserves an internal selection-proxy warning failure", {
  skip_if_suggest_absent("duckdb", "DBI")

  off <- sent_query_warning_process("selection_proxy", FALSE)
  on <- sent_query_warning_process("selection_proxy", TRUE)

  expect_match(
    off$result$error, "Missing values are always removed", fixed = TRUE
  )
  expect_identical(on$result$error, off$result$error)
  expect_identical(on$record$purpose, "selection_proxy")
  expect_false(is.na(on$record$sql))
  expect_match(on$record$sql, "SELECT", fixed = TRUE)
  expect_true(on$warning_verbosity_restored)
})

expect_explicit_audit <- function(case) {
  off <- sent_query_warning_process(case, FALSE)
  on <- sent_query_warning_process(case, TRUE)

  expect_null(off$result$error)
  expect_identical(
    on$result$value[order(on$result$value$g), ],
    off$result$value[order(off$result$value$g), ]
  )
  expect_identical(tail(on$record$purpose, 1L), "result")
  expect_false(anyNA(on$record$sql))
  expect_true(on$warning_verbosity_restored)
  on
}

test_that("explicit NA removal succeeds for a deferred result under audit", {
  skip_if_suggest_absent("RSQLite", "DBI")
  expect_explicit_audit("result_explicit")
})

test_that("explicit NA removal preserves internally sent SQL under audit", {
  skip_if_suggest_absent("duckdb", "DBI")
  on <- expect_explicit_audit("selection_proxy_explicit")
  internal_sql <- on$record$sql[on$record$purpose != "result"]
  expect_true(all(internal_sql %in% on$sent_sql))
})
