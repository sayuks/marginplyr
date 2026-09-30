# Disposable public-compute fixture for interruption and transaction ownership.
# Snapshots precede the injected failure; observers never share the transaction.
sqlite_interrupt_fixture <- function(code, schema = "main", overwrite = TRUE,
                                     sorted = TRUE, expansion = FALSE) {
  paths <- c(tempfile(fileext = ".sqlite"), tempfile(fileext = ".sqlite"))
  con <- DBI::dbConnect(RSQLite::SQLite(), paths[[1L]])
  observer <- DBI::dbConnect(RSQLite::SQLite(), paths[[1L]])
  on.exit({
    DBI::dbDisconnect(observer)
    if (DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
    unlink(paths)
  }, add = TRUE)
  for (connection in list(con, observer)) {
    DBI::dbExecute(connection, paste(
      "ATTACH DATABASE", DBI::dbQuoteString(connection, paths[[2L]]), "AS other"
    ))
  }
  input <- data.frame(g = c("a", "b"), v = c(2, 5))
  DBI::dbWriteTable(con, "source", input)
  DBI::dbExecute(con, "CREATE TABLE sentinel (value TEXT)")
  DBI::dbExecute(con, "INSERT INTO sentinel VALUES ('baseline')")
  if (overwrite) {
    DBI::dbExecute(con, paste0("CREATE TABLE ", schema, ".report (old INT)"))
    DBI::dbExecute(con, paste0("INSERT INTO ", schema, ".report VALUES (42)"))
    DBI::dbExecute(con, paste0(
      "CREATE INDEX ", schema, ".report_old ON report(old)"
    ))
  }
  for (one in c("main", "temp", "other")) {
    DBI::dbExecute(con, paste("ANALYZE", one))
  }
  source <- dplyr::tbl(con, "source")
  query <- if (expansion) {
    expand_with_margins(source, .grouping = rollup("g"),
                        .id = "sid", .sort = if (sorted) "last" else "none")
  } else {
    summarize_with_margins(
      source, z = sum(.data$v, na.rm = TRUE), .grouping = rollup("g"),
      .id = "sid", .sort = if (sorted) "last" else "none"
    )
  }
  fixture <- list(
    con = con, observer = observer, input = input, query = query,
    schema = schema, overwrite = overwrite,
    destination = DBI::Id(schema = schema, table = "report"),
    before = sqlite_interrupt_state(con),
    expected = if (expansion) {
      data.frame(g = c("a", "b", "Total", "Total"),
                 sid = c(1L, 1L, 2L, 2L), v = c(2, 5, 2, 5))
    } else {
      data.frame(g = c("a", "b", "Total"), sid = c(1L, 1L, 2L),
                 z = c(2, 5, 7))
    }
  )
  code(fixture)
}

# Read physical schema, indexes and optimizer statistics before any recovery
# probe, retry, caller decision or disposal can hide an incomplete restoration.
sqlite_interrupt_state <- function(con) {
  stats::setNames(lapply(c("main", "temp", "other"), function(schema) {
    list(
      schema = DBI::dbGetQuery(con, paste0(
        "SELECT * FROM ", schema, ".sqlite_master ORDER BY type, name"
      )),
      statistics = DBI::dbGetQuery(con, paste0(
        "SELECT * FROM ", schema, ".sqlite_stat1 ORDER BY tbl, idx"
      )),
      samples = if (DBI::dbExistsTable(
        con, DBI::Id(schema = schema, table = "sqlite_stat4")
      )) {
        DBI::dbGetQuery(con, paste0(
          "SELECT tbl, idx, neq, nlt, ndlt, hex(sample) AS sample FROM ",
          schema, ".sqlite_stat4 ORDER BY tbl, idx, sample"
        ))
      }
    )
  }), c("main", "temp", "other"))
}

# Trace DBI/dplyr system boundaries after real statements or result preparation.
# `deliver` permits a separate SIGINT supervisor to use these same checkpoints.
sqlite_interrupt_hook <- function(checkpoint, deliver = rlang::interrupt,
                                  cleanup_failure = NULL) {
  state <- new.env(parent = emptyenv())
  state$hooked <- TRUE
  state$hit <- FALSE
  state$inserted <- FALSE
  state$savepoint <- NULL
  state$cleanup_failure <- NULL
  patterns <- c(acquire = "^SAVEPOINT ", drop = "^DROP TABLE",
                create = "^CREATE (TEMPORARY )?TABLE", index = "^CREATE INDEX",
                insert = "^INSERT INTO", analyze = "^ANALYZE",
                rollback = "^ROLLBACK TO", release = "^RELEASE SAVEPOINT",
                cleanup_release = "^RELEASE SAVEPOINT")
  fault <- function() {
    state$hit <- TRUE
    deliver()
  }
  suppressMessages(trace(
    "dbExecute", where = asNamespace("DBI"), print = FALSE,
    tracer = function() {
      frame <- parent.frame()
      statement <- as.character(get("statement", envir = frame))
      if (state$hit && !is.null(cleanup_failure)) {
        pattern <- if (cleanup_failure == "rollback") {
          "^ROLLBACK TO"
        } else {
          "^RELEASE SAVEPOINT"
        }
        if (grepl(pattern, statement)) {
          state$cleanup_failure <- simpleError(
            paste("injected", cleanup_failure)
          )
          stop(state$cleanup_failure)
        }
      }
    },
    exit = function() {
      frame <- parent.frame()
      statement <- as.character(get("statement", envir = frame))
      if (is.null(evalq(returnValue(NULL), envir = frame))) {
        return(invisible(NULL))
      }
      if (grepl("^SAVEPOINT ", statement)) {
        state$savepoint <- sub("^SAVEPOINT ", "", statement)
      }
      if (grepl("^INSERT INTO", statement)) {
        state$inserted <- TRUE
        if (checkpoint %in% c("rollback", "cleanup_release")) {
          stop("ordinary INSERT checkpoint failure")
        }
      }
      if (!state$hit && checkpoint %in% names(patterns) &&
            grepl(patterns[[checkpoint]], statement)) {
        fault()
      }
    }
  ))
  suppressMessages(trace(
    "tbl", where = asNamespace("dplyr"), print = FALSE,
    exit = function() {
      if (!state$hit && state$inserted && checkpoint == "result") {
        fault()
      }
    }
  ))
  suppressMessages(trace(
    "dbExistsTable", where = asNamespace("DBI"), print = FALSE,
    exit = function() {
      if (!state$hit && is.null(state$savepoint) && checkpoint == "before") {
        fault()
      }
    }
  ))
  state
}

# Remove system-boundary instrumentation before a caller retry or decision.
sqlite_interrupt_unhook <- function(state) {
  if (!state$hooked) {
    return(invisible(NULL))
  }
  state$hooked <- FALSE
  suppressMessages(untrace("dbExecute", where = asNamespace("DBI")))
  suppressMessages(untrace("tbl", where = asNamespace("dplyr")))
  suppressMessages(untrace("dbExistsTable", where = asNamespace("DBI")))
}

# Acceptance assertions shared by deterministic tests and the SIGINT worker.
# Every immediate state observation precedes any write by the test itself.
expect_sqlite_restored <- function(fixture, state, outer = FALSE) {
  con <- fixture$con
  expect_true(state$hit)
  expect_identical(sqlite_interrupt_state(con), fixture$before)
  expect_identical(DBI::dbReadTable(con, "source"), fixture$input)
  expect_identical(DBI::dbReadTable(con, "sentinel")$value,
                   if (outer) c("baseline", "caller") else "baseline")
  if (fixture$overwrite) {
    expect_identical(DBI::dbReadTable(con, fixture$destination),
                     data.frame(old = 42L))
    expect_identical(DBI::dbGetQuery(con, paste0(
      "SELECT name, type FROM pragma_table_info('report', '",
      fixture$schema, "')"
    )), data.frame(name = "old", type = "INT"))
    expect_identical(DBI::dbGetQuery(con, paste0(
      "SELECT name FROM ", fixture$schema,
      ".sqlite_master WHERE type = 'index' AND tbl_name = 'report'"
    ))$name, "report_old")
    expect_identical(DBI::dbGetQuery(con, paste0(
      "SELECT stat FROM ", fixture$schema,
      ".sqlite_stat1 WHERE tbl = 'report'"
    ))$stat, "1 1")
  } else {
    expect_false(DBI::dbExistsTable(con, fixture$destination))
  }
  expect_identical(RSQLite::sqliteIsTransacting(con), outer)
  expect_identical(DBI::dbReadTable(fixture$observer, "sentinel")$value,
                   "baseline")
  if (fixture$schema != "temp") {
    if (fixture$overwrite) {
      expect_identical(DBI::dbReadTable(fixture$observer, fixture$destination),
                       data.frame(old = 42L))
    } else {
      expect_false(DBI::dbExistsTable(fixture$observer, fixture$destination))
    }
  }
  if (!is.null(state$savepoint)) {
    expect_error(DBI::dbExecute(con, paste("ROLLBACK TO", state$savepoint)),
                 "no such savepoint")
  }
}
