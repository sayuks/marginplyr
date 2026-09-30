# These public compute regressions inspect DBI state because a successful retry
# alone cannot show whether SQLite retained an unexpected transaction (#753).
sqlite_acquisition_fixture <- function(code) {
  path <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  observer <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit({
    DBI::dbDisconnect(observer)
    if (DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
    unlink(path)
  }, add = TRUE)
  input <- data.frame(g = c("a", "b"), v = c(2, 5))
  DBI::dbWriteTable(con, "source", input)
  DBI::dbExecute(con, "CREATE TABLE sentinel (value TEXT)")
  DBI::dbExecute(con, "INSERT INTO sentinel VALUES ('baseline')")
  DBI::dbExecute(con, "CREATE TABLE report (old INT)")
  DBI::dbExecute(con, "INSERT INTO report VALUES (42)")
  DBI::dbExecute(con, "CREATE INDEX report_old ON report(old)")
  query <- summarize_with_margins(
    dplyr::tbl(con, "source"), z = sum(.data$v, na.rm = TRUE),
    .grouping = rollup("g"), .sort = "last"
  )
  code(list(
    con = con, observer = observer, query = query, input = input,
    schema = DBI::dbGetQuery(
      con, "SELECT * FROM sqlite_master ORDER BY type, name"
    ),
    expected = data.frame(g = c("a", "b", "Total"), z = c(2, 5, 7))
  ))
}

# Inject only after the real SAVEPOINT statement returned successfully, while
# dbBegin() is still completing. Real rollback and release remain installed.
sqlite_fail_after_acquisition <- function(invalidate = FALSE) {
  injection <- new.env(parent = emptyenv())
  injection$active <- TRUE
  injection$savepoint <- NULL
  injection$acquired <- FALSE
  injection$failure <- structure(
    list(message = "ordinary failure after SAVEPOINT", call = NULL),
    class = c("sqlite_acquisition_failure", "error", "condition")
  )
  suppressMessages(trace(
    "dbExecute", where = asNamespace("DBI"), print = FALSE,
    exit = function() {
      frame <- parent.frame()
      statement <- as.character(get("statement", envir = frame))
      if (injection$active && grepl("^SAVEPOINT ", statement) &&
            !is.null(evalq(returnValue(NULL), envir = frame))) {
        injection$active <- FALSE
        injection$savepoint <- sub("^SAVEPOINT ", "", statement)
        con <- get("conn", envir = frame)
        # A successful named rollback-to proves that this savepoint exists.
        # No destination write has begun, so this probe changes no caller data.
        DBI::dbExecute(con, paste("ROLLBACK TO", injection$savepoint))
        injection$acquired <- RSQLite::sqliteIsTransacting(con)
        if (invalidate) {
          DBI::dbDisconnect(con)
        }
        stop(injection$failure)
      }
    }
  ))
  injection
}

test_that("SQLite acquisition failure releases work before persistent retry", {
  skip_if_suggest_absent("RSQLite", "DBI")
  for (flag in c(FALSE, TRUE)) {
    sqlite_acquisition_fixture(function(fixture) {
      con <- fixture$con
      expect_equal(as.data.frame(dplyr::collect(fixture$query)),
                   fixture$expected)
      injection <- sqlite_fail_after_acquisition()
      on.exit(
        suppressMessages(untrace("dbExecute", where = asNamespace("DBI"))),
        add = TRUE
      )
      err <- tryCatch(
        dplyr::compute(fixture$query, name = "report", temporary = FALSE,
                       overwrite = TRUE, in_transaction = flag),
        error = identity
      )
      expect_true(injection$acquired)
      expect_identical(err, injection$failure)
      expect_identical(DBI::dbReadTable(con, "source"), fixture$input)
      expect_identical(DBI::dbReadTable(con, "report"), data.frame(old = 42L))
      expect_identical(DBI::dbReadTable(con, "sentinel")$value, "baseline")
      expect_identical(DBI::dbGetQuery(
        con, "SELECT * FROM sqlite_master ORDER BY type, name"
      ), fixture$schema)
      expect_false(RSQLite::sqliteIsTransacting(con))
      expect_error(DBI::dbExecute(
        con, paste("ROLLBACK TO", injection$savepoint)
      ), "no such savepoint")

      # All residue checks precede any caller transaction or test cleanup.
      expect_true(DBI::dbBegin(con))
      DBI::dbExecute(con, "INSERT INTO sentinel VALUES ('later caller')")
      expect_true(DBI::dbCommit(con))
      out <- dplyr::compute(
        fixture$query, name = "recovered", temporary = FALSE,
        in_transaction = flag
      )
      expect_equal(as.data.frame(dplyr::collect(out)), fixture$expected)
      expect_false(RSQLite::sqliteIsTransacting(con))
      expect_equal(DBI::dbReadTable(fixture$observer, "recovered"),
                   fixture$expected)
      expect_identical(DBI::dbReadTable(fixture$observer, "sentinel")$value,
                       c("baseline", "later caller"))
      expect_identical(DBI::dbReadTable(fixture$observer, "report"),
                       data.frame(old = 42L))
    })
  }
})

test_that("SQLite acquisition failure preserves caller commit and rollback", {
  skip_if_suggest_absent("RSQLite", "DBI")
  for (flag in c(FALSE, TRUE)) {
    for (commit in c(FALSE, TRUE)) {
      sqlite_acquisition_fixture(function(fixture) {
        con <- fixture$con
        expect_true(DBI::dbBegin(con))
        DBI::dbExecute(con, "INSERT INTO sentinel VALUES ('caller')")
        injection <- sqlite_fail_after_acquisition()
        on.exit(
          suppressMessages(untrace("dbExecute", where = asNamespace("DBI"))),
          add = TRUE
        )
        err <- tryCatch(
          dplyr::compute(fixture$query, name = "report", temporary = FALSE,
                         overwrite = TRUE, in_transaction = flag),
          error = identity
        )
        expect_true(injection$acquired)
        expect_identical(err, injection$failure)
        expect_true(RSQLite::sqliteIsTransacting(con))
        expect_identical(DBI::dbReadTable(con, "source"), fixture$input)
        expect_identical(DBI::dbReadTable(con, "report"), data.frame(old = 42L))
        expect_identical(DBI::dbReadTable(con, "sentinel")$value,
                         c("baseline", "caller"))
        expect_identical(DBI::dbGetQuery(
          con, "SELECT * FROM sqlite_master ORDER BY type, name"
        ), fixture$schema)
        expect_error(DBI::dbExecute(
          con, paste("ROLLBACK TO", injection$savepoint)
        ), "no such savepoint")
        expect_identical(DBI::dbReadTable(fixture$observer, "sentinel")$value,
                         "baseline")

        # The caller decides without a retry overwriting the failed destination.
        if (commit) {
          expect_true(DBI::dbCommit(con))
        } else {
          expect_true(DBI::dbRollback(con))
        }
        expect_false(RSQLite::sqliteIsTransacting(con))
        expect_identical(DBI::dbReadTable(fixture$observer, "sentinel")$value,
                         if (commit) c("baseline", "caller") else "baseline")
        expect_identical(DBI::dbReadTable(fixture$observer, "source"),
                         fixture$input)
        expect_identical(DBI::dbReadTable(fixture$observer, "report"),
                         data.frame(old = 42L))
        expect_identical(DBI::dbGetQuery(
          fixture$observer, "SELECT * FROM sqlite_master ORDER BY type, name"
        ), fixture$schema)
      })
    }
  }
})

test_that("unavailable SQLite cleanup retains the ordinary acquisition cause", {
  skip_if_suggest_absent("RSQLite", "DBI")
  control <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbBegin(control, name = "dependency_control")
  DBI::dbDisconnect(control)
  dependency_error <- tryCatch(
    DBI::dbRollback(control, name = "dependency_control"), error = identity
  )
  expect_s3_class(dependency_error, "error")

  sqlite_acquisition_fixture(function(fixture) {
    injection <- sqlite_fail_after_acquisition(invalidate = TRUE)
    on.exit(suppressMessages(untrace("dbExecute", where = asNamespace("DBI"))),
            add = TRUE)
    err <- tryCatch(
      dplyr::compute(fixture$query, name = "report", temporary = FALSE,
                     overwrite = TRUE),
      error = identity
    )
    expect_true(injection$acquired)
    expect_match(conditionMessage(err), "savepoint rollback failed")
    expect_match(conditionMessage(err), conditionMessage(dependency_error),
                 fixed = TRUE)
    expect_identical(err$parent, injection$failure)
    expect_false(DBI::dbIsValid(fixture$con))
    # The injected disconnect cannot establish that package cleanup succeeded.
    expect_identical(DBI::dbReadTable(fixture$observer, "source"),
                     fixture$input)
    expect_identical(DBI::dbReadTable(fixture$observer, "report"),
                     data.frame(old = 42L))
    expect_identical(DBI::dbReadTable(fixture$observer, "sentinel")$value,
                     "baseline")
  })
})
