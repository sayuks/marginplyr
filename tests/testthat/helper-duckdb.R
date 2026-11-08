# The one place this suite opens a DuckDB connection.
#
# `shared_home = FALSE` is the whole reason the helper exists. DuckDB resolves a
# home directory for its extension cache and stored secrets each time a driver
# is created, and it picks the shared `~/.duckdb` whenever that directory is
# already present -- which it is on any machine that has ever run DuckDB. A
# connection opened without the argument therefore writes outside the R
# session's temporary directory during `R CMD check`, which CRAN Repository
# Policy forbids. The argument redirects both locations into `tempdir()`; it
# needs duckdb (>= 1.5.5), which is what DESCRIPTION states.
#
# Passing it at one site rather than at each of the suite's connections is what
# makes the rule checkable: `test-duckdb-storage.R` reads the resolved
# directories back out of a connection, and its source scan holds every other
# file to going through here.
#
# Callers keep their own `on.exit(DBI::dbDisconnect(con, shutdown = TRUE))`.
# Deferring the disconnect from inside would need withr, which this package does
# not depend on, and the tests that open a connection already tear it down.
duckdb_test_connection <- function(...) {
  DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), ...)
}

# Live JSON tests need the extension already available; never install it here.
# A supplied path permits an isolated extension without a shared home cache.
duckdb_test_json_extension <- function(con) {
  path <- Sys.getenv("MARGINPLYR_DUCKDB_JSON_EXTENSION", "")
  sql <- if (nzchar(path)) {
    paste("LOAD", DBI::dbQuoteString(con, path))
  } else {
    "LOAD json"
  }
  tryCatch(DBI::dbExecute(con, sql), error = function(cnd) {
    if (nzchar(path)) {
      stop(cnd)
    }
    testthat::skip(paste(
      "DuckDB json extension unavailable; install it before testing or set",
      "MARGINPLYR_DUCKDB_JSON_EXTENSION to an isolated extension path."
    ))
  })
  invisible(NULL)
}

# A replacement-scan refusal may read catalog names, never bind its source.
# The public audit exposes that complete, successful internal operation.
expect_duckdb_catalog_only <- function() {
  audit <- marginplyr::last_sent_queries(details = TRUE)
  testthat::expect_identical(audit$purpose, "relation_catalog")
  testthat::expect_identical(audit$source, "literal")
  testthat::expect_identical(audit$status, "returned")
  testthat::expect_identical(audit$sql, paste(
    "SELECT database_name, schema_name, table_name AS relation_name,",
    "system.main.current_database() AS default_database,",
    "system.main.current_setting('search_path') AS search_path",
    "FROM system.main.duckdb_tables() UNION ALL",
    "SELECT database_name, schema_name, view_name AS relation_name,",
    "system.main.current_database(),",
    "system.main.current_setting('search_path')",
    "FROM system.main.duckdb_views()"
  ))
  testthat::expect_equal(nrow(audit$conditions[[1L]]), 0L)
}

# Count actual query sends. The caller registers `counter$untrace()` on exit.
duckdb_query_counter <- function() {
  counter <- new.env(parent = emptyenv())
  counter$queries <- 0L
  signature <- c("duckdb_connection", "character")
  # do.call retains the tracer's closure when S4 tracing wraps .local lazily.
  suppressMessages(do.call(trace, list(
    what = "dbSendQuery", signature = signature, where = asNamespace("DBI"),
    print = FALSE,
    tracer = function() {
      counter$queries <- counter$queries + 1L
    }
  )))
  counter$untrace <- function() {
    suppressMessages(untrace(
      "dbSendQuery", signature = signature, where = asNamespace("DBI")
    ))
  }
  counter
}
