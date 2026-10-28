# Shared setup for the dated SQL-audit investigation, not package test helpers.
backend_harness_config <- function(args) {
  if (length(args) != 3L || !nzchar(args[[1L]]) ||
      !grepl("^[0-9]+$", args[[2L]])) {
    stop("Usage: Rscript <script.R> <postgres-socket-host> <port> <output-dir>")
  }
  port <- as.integer(args[[2L]])
  stopifnot(!is.na(port), port >= 1L, port <= 65535L)
  dir.create(args[[3L]], recursive = TRUE, showWarnings = FALSE)
  list(host = args[[1L]], port = port,
       output = normalizePath(args[[3L]], mustWork = TRUE))
}

backend_harness_metadata <- function() {
  packages <- c("marginplyr", "DBI", "dbplyr", "dplyr", "RSQLite",
                "duckdb", "RPostgres", "pkgload")
  list(
    captured_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    commit = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
    R = R.version.string,
    packages = stats::setNames(vapply(packages, function(package) {
      as.character(utils::packageVersion(package))
    }, character(1)), packages)
  )
}

# Register cleanup before opening the first connection; partial setup is covered.
with_backend_harness <- function(args, action) {
  config <- backend_harness_config(args)
  pkgload::load_all(".", quiet = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(suspendInterrupts(options(old)), add = TRUE)
  connections <- list()
  on.exit(suspendInterrupts({
    for (backend in rev(names(connections))) {
      con <- connections[[backend]]
      tryCatch({
        if (identical(backend, "duck")) {
          DBI::dbDisconnect(con, shutdown = TRUE)
        } else {
          DBI::dbDisconnect(con)
        }
      }, error = function(cnd) message("Disconnect cleanup: ",
                                      conditionMessage(cnd)))
    }
  }), add = TRUE)
  suspendInterrupts({
    connections$sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    connections$duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
    connections$pg <- DBI::dbConnect(
      RPostgres::Postgres(), host = config$host, port = config$port,
      dbname = "postgres"
    )
  })
  metadata <- backend_harness_metadata()
  metadata$postgres <- DBI::dbGetQuery(connections$pg, "SELECT version()")[[1L]]
  action(connections, config, metadata)
}

# Capture only dbSendQuery, including controls delegated through it. Capturing
# dbSendStatement too would count the same control twice on these drivers.
start_backend_query_capture <- function() {
  namespace <- asNamespace("DBI")
  if (inherits(get("dbSendQuery", namespace), "functionWithTrace")) {
    stop("Run this harness in a fresh R process without an existing DBI trace.")
  }
  capture <- new.env(parent = emptyenv())
  capture$sql <- character()
  capture$active <- FALSE
  done <- FALSE
  on.exit({
    if (!done) stop_backend_query_capture(capture)
  }, add = TRUE)
  callback <- function(statement) {
    capture$sql <- c(capture$sql, as.character(statement))
  }
  suspendInterrupts({
    capture$active <- TRUE
    trace("dbSendQuery", where = namespace,
          tracer = substitute(CAPTURE(statement), list(CAPTURE = callback)),
          print = FALSE)
    done <- TRUE
  })
  capture
}

stop_backend_query_capture <- function(capture) {
  suspendInterrupts({
    if (isTRUE(capture$active)) {
      untrace("dbSendQuery", where = asNamespace("DBI"))
      capture$active <- FALSE
    }
  })
  invisible(NULL)
}

public_backend_outcome <- function(value) {
  list(classes = class(value),
       error = if (inherits(value, "condition")) conditionMessage(value) else NULL)
}
