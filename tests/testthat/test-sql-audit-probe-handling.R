test_that("probe warnings without a restart are recorded as propagated", {
  skip_if_suggest_absent("RSQLite", "DBI")
  saved <- share_dialect_snapshot()
  on.exit(restore_share_dialect_verdicts(saved), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(1, 2)), "probe_handling"
  )
  leaf <- simpleWarning("restartless probe warning")
  collect <- dplyr::collect
  collections <- 0L
  testthat::local_mocked_bindings(collect = function(x, ...) {
    collections <<- collections + 1L
    withRestarts(signalCondition(leaf), continue_probe = function() NULL)
    collect(x, ...)
  }, .package = "dplyr")

  observed <- list()
  run <- function(audited) {
    options(marginplyr.audit_sql = audited)
    empty_share_dialect_verdicts()
    withCallingHandlers(
      tryCatch(
        summarize_with_margins(
          source,
          amount = sum(.data$v, na.rm = TRUE),
          share = share_of_total(amount),
          .grouping = rollup(dplyr::all_of("g"))
        ),
        error = identity
      ),
      warning = function(cnd) {
        observed[[length(observed) + 1L]] <<- cnd
        invokeRestart("continue_probe")
      }
    )
  }

  off <- run(FALSE)
  on <- run(TRUE)
  expect_s3_class(off, "marginplyr_error")
  expect_identical(class(on), class(off))
  expect_identical(conditionMessage(on), conditionMessage(off))
  expect_identical(collections, 2L)
  expect_identical(observed, list(leaf, leaf))

  rows <- last_sent_queries(include_result = FALSE, details = TRUE)
  expect_identical(rows$purpose, "share_dialect")
  expect_identical(rows$status, "returned")
  expect_identical(rows$conditions[[1L]]$message, conditionMessage(leaf))
  expect_identical(rows$conditions[[1L]]$phase, "invocation")
  expect_identical(rows$conditions[[1L]]$handling, "propagated")
})
