# Driver-boundary doubles exercise failure/cleanup paths without a server in
# CRAN checks. tools/postgres-share-transactions.R owns live transaction values.
postgres_probe_fixture <- function(active = TRUE, probe = "raises", control = 1,
                                   fail_command = NULL, probe_warning = FALSE) {
  skip_if_suggest_absent("RPostgres")
  RPostgres::Postgres()
  con <- methods::new("PqConnection")
  scope <- parent.frame()
  state <- new.env(parent = emptyenv())
  state$statements <- character()
  state$aborted <- FALSE
  local_mocked_bindings(postgresIsTransacting = function(conn) {
    if (is.function(active)) active() else active
  }, .package = "RPostgres", .env = scope)
  local_mocked_bindings(
    dbIsValid = function(db_obj, ...) TRUE,
    dbGetInfo = function(db_obj, ...) list(serverVersion = "17.11"),
    dbQuoteIdentifier = function(conn, x, ...) DBI::SQL(paste0('"', x, '"')),
    dbExecute = function(conn, statement, ...) {
      state$statements <- c(state$statements, as.character(statement))
      if (!is.null(fail_command) && startsWith(statement, fail_command)) {
        stop("transaction control failed")
      }
      if (startsWith(statement, "ROLLBACK TO")) state$aborted <- FALSE
      0L
    },
    .package = "DBI", .env = scope
  )
  local_mocked_bindings(collect = function(x, ...) {
    if (grepl("SUM('x')", dbplyr::sql_render(x), fixed = TRUE)) {
      if (probe_warning) warning("probe warning", call. = FALSE)
      if (probe == "raises") {
        state$aborted <- isTRUE(active)
        stop("function sum(unknown) is not unique")
      }
      return(data.frame(p = if (probe == "converts") 0 else "unexpected"))
    }
    if (state$aborted) stop("current transaction is aborted")
    if (is.function(control)) return(control())
    data.frame(p = control)
  }, .package = "dplyr", .env = scope)
  empty_share_dialect_verdicts()
  state$source <- dbplyr::tbl_lazy(data.frame(g = "a", x = 2L), con = con)
  state
}

# The symbols are columns and contextual-summary names in the public data mask.
# nolint start: object_usage_linter.
postgres_probe_summary <- function(source) {
  summarize_with_margins(
    source, .grouping = rollup(g), amount = sum(x, na.rm = TRUE),
    parent = share_of_parent(amount), total = share_of_total(amount)
  )
}
# nolint end

test_that("PostgreSQL shares recover their probe before the control", {
  saved <- share_dialect_snapshot()
  on.exit(restore_share_dialect_verdicts(saved), add = TRUE)
  fixture <- postgres_probe_fixture()
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  out <- postgres_probe_summary(fixture$source)
  expect_s3_class(out, "tbl_lazy")
  expect_false(fixture$aborted)
  expect_length(fixture$statements, 4L)
  expect_match(fixture$statements[[1L]], "^SAVEPOINT ")
  expect_match(fixture$statements[[2L]], "^ROLLBACK TO SAVEPOINT ")
  expect_match(fixture$statements[[4L]], "^RELEASE SAVEPOINT ")
  record <- last_sent_queries()
  expect_identical(record$purpose, c(
    "share_dialect_transaction", "share_dialect",
    "share_dialect_transaction", "share_dialect_control",
    "share_dialect_transaction", "share_dialect_transaction", "result"
  ))
  expect_identical(record$sql[record$purpose == "share_dialect_transaction"],
                   fixture$statements)
  postgres_probe_summary(fixture$source)
  expect_length(fixture$statements, 4L)
  expect_identical(last_sent_queries()$purpose, "result")
})

test_that("PostgreSQL autocommit needs no savepoint controls", {
  saved <- share_dialect_snapshot()
  on.exit(restore_share_dialect_verdicts(saved), add = TRUE)
  fixture <- postgres_probe_fixture(active = FALSE)
  expect_s3_class(postgres_probe_summary(fixture$source), "tbl_lazy")
  expect_length(fixture$statements, 0L)
})

test_that("PostgreSQL probe outcomes release isolation", {
  saved <- share_dialect_snapshot()
  on.exit(restore_share_dialect_verdicts(saved), add = TRUE)
  fixture <- postgres_probe_fixture(probe = "converts")
  expect_error(
    postgres_probe_summary(fixture$source), class = "marginplyr_error"
  )
  expect_length(fixture$statements, 3L)
  fixture <- postgres_probe_fixture(probe = "unexpected")
  expect_error(
    postgres_probe_summary(fixture$source), class = "marginplyr_error"
  )
  expect_length(fixture$statements, 3L)
  expect_length(ls(share_dialect_verdicts), 0L)
  fixture <- postgres_probe_fixture(control = function() stop("control failed"))
  expect_error(
    postgres_probe_summary(fixture$source), class = "marginplyr_error"
  )
  expect_length(fixture$statements, 4L)
  expect_false(fixture$aborted)
  expect_length(ls(share_dialect_verdicts), 0L)
})

test_that("PostgreSQL cannot probe without establishing its isolation", {
  saved <- share_dialect_snapshot()
  on.exit(restore_share_dialect_verdicts(saved), add = TRUE)
  fixture <- postgres_probe_fixture(fail_command = "SAVEPOINT")
  expect_error(
    postgres_probe_summary(fixture$source), class = "marginplyr_error"
  )
  expect_length(fixture$statements, 1L)
  expect_length(ls(share_dialect_verdicts), 0L)
  fixture <- postgres_probe_fixture(
    active = function() stop("state unavailable")
  )
  expect_error(
    postgres_probe_summary(fixture$source), class = "marginplyr_error"
  )
  expect_length(fixture$statements, 0L)
})

test_that("a PostgreSQL cleanup failure cannot cache a measured answer", {
  saved <- share_dialect_snapshot()
  on.exit(restore_share_dialect_verdicts(saved), add = TRUE)
  fixture <- postgres_probe_fixture(
    probe = "converts", fail_command = "RELEASE"
  )
  expect_error(
    postgres_probe_summary(fixture$source), "transaction control failed"
  )
  expect_length(ls(share_dialect_verdicts), 0L)
})


test_that("PostgreSQL controls retain their owned completion and conditions", {
  saved <- share_dialect_snapshot()
  on.exit(restore_share_dialect_verdicts(saved), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  fixture <- postgres_probe_fixture()
  postgres_probe_summary(fixture$source)
  rows <- last_sent_queries(FALSE, TRUE)
  expect_identical(rows$status, c(
    "returned", "errored", "returned", "returned", "returned", "returned"
  ))
  expect_identical(rows$source, c(
    "literal", "render", "literal", "render", "literal", "literal"
  ))
  expect_identical(rows$conditions[[2L]]$handling, "handled")
  expect_identical(rows$sql[rows$source == "literal"], fixture$statements)
  for (command in c("SAVEPOINT", "RELEASE")) {
    fixture <- postgres_probe_fixture(fail_command = command)
    if (command == "SAVEPOINT") {
      expect_error(postgres_probe_summary(fixture$source),
                   class = "marginplyr_error")
    } else {
      expect_error(postgres_probe_summary(fixture$source),
                   "transaction control failed")
    }
    rows <- last_sent_queries(FALSE, TRUE)
    failed <- rows[nrow(rows), ]
    expect_identical(failed$status, "errored")
    expect_identical(failed$source, "literal")
    expect_identical(failed$conditions[[1L]]$handling,
                     if (command == "SAVEPOINT") "handled" else "propagated")
  }
})


test_that("probe warning suppression retains conditions without delivery", {
  saved <- share_dialect_snapshot()
  on.exit(restore_share_dialect_verdicts(saved), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE, warn = 2)
  on.exit(options(old), add = TRUE)
  fixture <- postgres_probe_fixture(probe_warning = TRUE, control = function() {
    warning("control warning", call. = FALSE)
    data.frame(p = 1)
  })
  expect_s3_class(postgres_probe_summary(fixture$source), "tbl_lazy")
  rows <- last_sent_queries(FALSE, TRUE)
  probe <- rows$conditions[[2L]]
  expect_identical(probe$message, c(
    "probe warning", "function sum(unknown) is not unique"
  ))
  expect_identical(probe$handling, c("suppressed", "handled"))
  expect_identical(probe$phase, c("invocation", "invocation"))
  control <- rows$conditions[[4L]]
  expect_identical(control$message, "control warning")
  expect_identical(control$handling, "suppressed")
  expect_identical(control$phase, "invocation")
  expect_identical(rows$status[c(2L, 4L)], c("errored", "returned"))
  expect_identical(getOption("warn"), 2L)
})

test_that("cleanup interrupts retain the interrupted control", {
  saved <- share_dialect_snapshot()
  on.exit(restore_share_dialect_verdicts(saved), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  fixture <- postgres_probe_fixture(probe = "converts")
  leaf <- structure(list(message = "cleanup interrupted", call = NULL),
                    class = c("interrupt", "condition"))
  execute <- DBI::dbExecute
  testthat::local_mocked_bindings(dbExecute = function(conn, statement, ...) {
    if (startsWith(statement, "RELEASE")) stop(leaf)
    execute(conn, statement, ...)
  }, .package = "DBI")
  caught <- tryCatch(
    postgres_probe_summary(fixture$source), interrupt = identity
  )
  expect_identical(caught, leaf)
  rows <- last_sent_queries(FALSE, TRUE)
  expect_identical(rows$status, c(
    "returned", "returned", "returned", "interrupted"
  ))
  expect_identical(rows$source[[4L]], "literal")
  expect_identical(rows$conditions[[4L]], dplyr::tibble(
    classes = list(class(leaf)), message = "cleanup interrupted",
    phase = "invocation", handling = "propagated"
  ))
})
