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
duckdb_test_connection <- function(...) {
  DBI::dbConnect(duckdb::duckdb(shared_home = FALSE, config = list(
    autoinstall_known_extensions = "false"
  )), ...)
}

# Which JSON coverage this execution promises. Direct local tests require it;
# check and explicitly reduced-backend runs retain their declared optionality.
duckdb_json_test_mode <- function(required = required_suggests(),
                                  hidden = hidden_suggests(),
                                  checking = testthat::is_checking()) {
  explicit <- Sys.getenv("MARGINPLYR_REQUIRED_DUCKDB_JSON", "")
  if (!explicit %in% c("", "true", "false")) {
    stop("MARGINPLYR_REQUIRED_DUCKDB_JSON must be true or false.",
         call. = FALSE)
  }
  if (identical(explicit, "true") || "duckdb" %in% required) {
    return("required")
  }
  if (identical(explicit, "false") || "duckdb" %in% hidden ||
        identical(Sys.getenv("_R_CHECK_DEPENDS_ONLY_"), "true") ||
        length(required) > 0L || checking) {
    return("optional")
  }
  "required"
}

# Prepares a selected JSON file before its cases. Source checkouts may acquire
# the extension; installed-package checks only validate what their entry passed.
# The file teardown owns any managed-cache lease and temporary environment.
prepare_duckdb_json_tests <- function() {
  if (identical(duckdb_json_test_mode(), "optional")) {
    return(invisible(NULL))
  }
  tryCatch({
    for (package in c("duckdb", "DBI")) {
      if (!suggest_available(package)) {
        stop(suggest_absence_reason(package), call. = FALSE)
      }
    }
    path <- Sys.getenv("MARGINPLYR_DUCKDB_JSON_EXTENSION", "")
    module <- NULL
    if (!testthat::is_checking()) {
      script <- repository_file("tools/duckdb-json.R")
      if (!is.na(script)) {
        module <- new.env(parent = baseenv())
        sys.source(script, envir = module)
      }
    }
    managed <- FALSE
    if (!is.null(module) && nzchar(path)) {
      cache <- normalizePath(module$duckdb_json_default_cache(),
                             winslash = "/", mustWork = FALSE)
      candidate <- normalizePath(path, winslash = "/", mustWork = FALSE)
      managed <- startsWith(candidate, paste0(cache, "/"))
    }
    lease <- NULL
    registered <- FALSE
    on.exit({
      if (!registered && !is.null(lease)) module$duckdb_json_release(lease)
    }, add = TRUE)
    if (!is.null(module) && (!nzchar(path) || managed)) {
      lease <- module$duckdb_json_acquire(install = TRUE, path = path)
      path <- lease$path
    } else if (nzchar(path)) {
      path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    } else if (!testthat::is_checking()) {
      stop(paste(
        "Local preparation needs tools/duckdb-json.R or an explicit",
        "MARGINPLYR_DUCKDB_JSON_EXTENSION path."
      ), call. = FALSE)
    }
    teardown <- testthat::teardown_env()
    suspendInterrupts({
      if (!is.null(lease)) {
        withr::defer(module$duckdb_json_release(lease), envir = teardown)
      }
      registered <- TRUE
      withr::local_envvar(c(
        MARGINPLYR_DUCKDB_JSON_EXTENSION = path,
        MARGINPLYR_REQUIRED_DUCKDB_JSON = "true"
      ), .local_envir = teardown)
    })
    con <- duckdb_test_connection()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    duckdb_test_json_extension(con)
    value <- DBI::dbGetQuery(con, paste(
      "SELECT json_extract_string('{\"answer\":42}', '$.answer') AS answer"
    ))$answer
    if (!identical(value, "42")) {
      stop("The prepared extension did not answer the JSON control query.",
           call. = FALSE)
    }
    invisible(NULL)
  }, error = function(cnd) {
    stop("DuckDB JSON preparation failed: ", conditionMessage(cnd),
         call. = FALSE)
  })
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
    if (nzchar(path) || identical(duckdb_json_test_mode(), "required")) {
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
  # Compare the complete catalog-only query shape without fixing whitespace.
  tokens <- function(sql) {
    regmatches(sql, gregexpr("'(?:[^']|'')*'|[[:alnum:]_]+|[^[:space:]]",
                             sql, perl = TRUE))
  }
  testthat::expect_identical(tokens(audit$sql), tokens(paste(
    "SELECT database_name, schema_name, table_name AS relation_name,",
    "'table' AS kind, NULL::VARCHAR AS definition,",
    "system.main.current_database() AS default_database,",
    "system.main.current_setting('search_path') AS search_path",
    "FROM system.main.duckdb_tables() UNION ALL",
    "SELECT database_name, schema_name, view_name, 'view', sql,",
    "system.main.current_database(),",
    "system.main.current_setting('search_path')",
    "FROM system.main.duckdb_views() UNION ALL",
    "SELECT database_name, schema_name, function_name, function_type,",
    "macro_definition,",
    "system.main.current_database(),",
    "system.main.current_setting('search_path')",
    "FROM system.main.duckdb_functions()",
    "WHERE function_type IN ('macro', 'table_macro') UNION ALL",
    "SELECT NULL, NULL, keyword_name, 'keyword', keyword_category,",
    "system.main.current_database(),",
    "system.main.current_setting('search_path')",
    "FROM system.main.duckdb_keywords()"
  )))
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
