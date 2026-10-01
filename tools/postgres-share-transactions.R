#!/usr/bin/env Rscript

# Fresh-process public acceptance for #774, against a disposable PostgreSQL.
# PGHOST/PGPORT/PGUSER/PGDATABASE identify an administrator connection; the
# runner creates and removes its own tables, sequence, function and reader role.
# Install the working tree first; every worker loads the installed package.
# Rscript tools/postgres-share-transactions.R [evidence-directory]
args <- commandArgs(TRUE)
worker <- length(args) > 0L && args[[1L]] == "--worker"

run_case <- function(mode, source_kind, heat, audit, termination, access, output) {
  library(marginplyr)
  admin <- DBI::dbConnect(RPostgres::Postgres())
  on.exit(DBI::dbDisconnect(admin), add = TRUE)
  stem <- paste0("margin774_", Sys.getpid())
  tables <- paste0(stem, c("_source", "_marker", "_sentinel"))
  sequence <- paste0(stem, "_reads")
  fun <- paste0(stem, "_value")
  reader <- paste0(stem, "_reader")
  execute <- function(sql) invisible(DBI::dbExecute(admin, sql))
  execute(paste("CREATE TABLE", tables[[1L]], "(g text, x integer)"))
  on.exit({
    for (table in tables) execute(paste("DROP TABLE IF EXISTS", table))
    execute(paste0("DROP FUNCTION IF EXISTS ", fun, "(integer)"))
    execute(paste("DROP SEQUENCE IF EXISTS", sequence))
    if (access == "select-only") execute(paste("DROP ROLE IF EXISTS", reader))
  }, add = TRUE, after = FALSE)
  execute(paste("INSERT INTO", tables[[1L]], "VALUES ('a', 2), ('b', 6)"))
  execute(paste("CREATE TABLE", tables[[2L]], "(x integer)"))
  execute(paste("INSERT INTO", tables[[2L]], "VALUES (0)"))
  execute(paste("CREATE TABLE", tables[[3L]], "(x text)"))
  execute(paste("INSERT INTO", tables[[3L]], "VALUES ('untouched')"))
  execute(paste("CREATE SEQUENCE", sequence))
  execute(paste0("CREATE FUNCTION ", fun, "(v integer) RETURNS integer ",
                 "LANGUAGE plpgsql VOLATILE AS $$ BEGIN PERFORM nextval('",
                 sequence, "'); RETURN v; END $$"))
  if (access == "select-only") {
    execute(paste("CREATE ROLE", reader, "LOGIN PASSWORD 'margin774_reader'"))
    execute(paste("GRANT SELECT ON", paste(tables, collapse = ","), "TO", reader))
    execute(paste("GRANT USAGE, SELECT ON SEQUENCE", sequence, "TO", reader))
  }
  con <- if (access == "select-only") {
    DBI::dbConnect(RPostgres::Postgres(), user = reader, password = "margin774_reader")
  } else {
    DBI::dbConnect(RPostgres::Postgres())
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)
  lazy <- function(connection) {
    if (access != "write") return(dplyr::tbl(connection, tables[[1L]]))
    dplyr::tbl(connection, dbplyr::sql(paste0(
      "SELECT g, ", fun, "(x) AS x FROM ", tables[[1L]],
      " WHERE ", fun, "(1) = 1"
    )))
  }
  source <- lazy(con)
  summary <- function(input) {
    amount <- if (source_kind == "sum") quote(sum(x, na.rm = TRUE)) else quote(dplyr::n())
    shares <- switch(mode,
      parent = list(parent = quote(share_of_parent(amount))),
      total = list(total = quote(share_of_total(amount))),
      combined = list(parent = quote(share_of_parent(amount)),
                      total = quote(share_of_total(amount)))
    )
    marginplyr::summarize_with_margins(input, amount = !!amount, !!!shares,
                                      .grouping = marginplyr::rollup(g), .sort = "last")
  }
  options(marginplyr.audit_sql = audit == "on")
  if (heat %in% c("same", "other")) {
    warmer <- if (heat == "same") con else DBI::dbConnect(RPostgres::Postgres())
    summary(lazy(warmer))
    if (heat == "other") DBI::dbDisconnect(warmer)
  }
  pid <- DBI::dbGetQuery(con, "SELECT pg_backend_pid() AS pid")$pid
  DBI::dbBegin(con)
  on.exit({
    if (RPostgres::postgresIsTransacting(con)) DBI::dbRollback(con)
  }, add = TRUE, after = FALSE)
  if (access != "write") {
    DBI::dbExecute(con, "SET TRANSACTION READ ONLY")
  } else {
    DBI::dbExecute(con, paste("UPDATE", tables[[2L]], "SET x = 1"))
  }
  if (heat == "retry") {
    tryCatch(DBI::dbGetQuery(con, "SELECT SUM('x')"), error = function(cnd) NULL)
    unanswered <- tryCatch(summary(source), error = identity)
    stopifnot(inherits(unanswered, "marginplyr_error"))
    DBI::dbRollback(con)
    summary(source)
    DBI::dbBegin(con)
    DBI::dbExecute(con, paste("UPDATE", tables[[2L]], "SET x = 1"))
  }
  reads <- function() DBI::dbGetQuery(admin, paste(
    "SELECT is_called FROM", sequence
  ))$is_called
  if (access == "write") stopifnot(!reads())
  query <- tryCatch(summary(source), error = identity)
  # Independent observations precede target diagnostics, recovery and cleanup.
  state <- DBI::dbGetQuery(admin, paste0(
    "SELECT state FROM pg_stat_activity WHERE pid = ", pid
  ))$state
  before <- DBI::dbGetQuery(admin, paste("SELECT x FROM", tables[[2L]]))$x
  stopifnot(identical(state, "idle in transaction"), before == 0L)
  if (access == "write") stopifnot(!reads())
  if (inherits(query, "error")) stop(query)
  record <- if (audit == "on") marginplyr::last_sent_queries() else NULL
  if (!is.null(record)) {
    purposes <- record$purpose
    stopifnot(sum(purposes == "share_dialect") <= 1L,
              sum(purposes == "share_dialect_control") <= 1L,
              sum(purposes == "share_dialect_transaction") <= 4L)
    if (heat == "cold") stopifnot(sum(purposes == "share_dialect_control") == 1L)
    if (heat != "cold") stopifnot(!any(grepl("^share_dialect", purposes)))
  }
  values <- dplyr::collect(query)
  after_collect <- DBI::dbGetQuery(admin, paste0(
    "SELECT state FROM pg_stat_activity WHERE pid = ", pid
  ))$state
  stopifnot(identical(after_collect, "idle in transaction"))
  if (access == "write") stopifnot(reads())
  expected <- if (source_kind == "sum") c(2, 6, 8) else c(1, 1, 2)
  ratio <- if (source_kind == "sum") c(0.25, 0.75, 1) else c(0.5, 0.5, 1)
  stopifnot(identical(as.numeric(values$amount), expected))
  for (column in intersect(c("parent", "total"), names(values))) {
    stopifnot(identical(values[[column]], ratio))
  }
  stopifnot(DBI::dbGetQuery(con, "SELECT 1 AS ok")$ok == 1L)
  stopifnot(DBI::dbGetQuery(admin, paste("SELECT x FROM", tables[[2L]]))$x == 0L)
  # A second public construction confirms measured answers are reused.
  summary(source)
  if (audit == "on") stopifnot(identical(marginplyr::last_sent_queries()$purpose, "result"))
  if (termination == "commit") DBI::dbCommit(con) else DBI::dbRollback(con)
  marker <- DBI::dbGetQuery(admin, paste("SELECT x FROM", tables[[2L]]))$x
  stopifnot(marker == as.integer(access == "write" && termination == "commit"))
  stopifnot(identical(DBI::dbGetQuery(admin, paste(
    "SELECT g, x FROM", tables[[1L]], "ORDER BY g"
  )), data.frame(g = c("a", "b"), x = c(2L, 6L))))
  stopifnot(identical(DBI::dbGetQuery(admin, paste(
    "SELECT x FROM", tables[[3L]]
  ))$x, "untouched"))
  saveRDS(list(state = state, after_collect = after_collect, before = before,
               marker = marker, values = values, audit = record,
               server = DBI::dbGetQuery(admin, "SELECT version()")[[1L]],
               driver = as.character(utils::packageVersion("RPostgres"))), output)
}

if (worker) {
  do.call(run_case, as.list(args[-1L]))
} else {
  directory <- if (length(args)) args[[1L]] else tempfile("margin774-evidence-")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  cases <- expand.grid(mode = c("parent", "total", "combined"),
                       source = c("sum", "count"), heat = c("cold", "same", "other"),
                       audit = c("on", "off"), termination = c("commit", "rollback"),
                       access = c("write", "read-only", "select-only"),
                       stringsAsFactors = FALSE)
  cases <- rbind(cases, data.frame(mode = "combined", source = "sum", heat = "retry",
                                   audit = "on", termination = "commit", access = "write"))
  path <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  for (i in seq_len(nrow(cases))) {
    label <- paste(unlist(cases[i, ]), collapse = "-")
    status <- system2(file.path(R.home("bin"), "Rscript"),
                       c(shQuote(path), "--worker", unlist(cases[i, ]),
                         shQuote(file.path(directory, paste0(label, ".rds")))),
                       stdout = file.path(directory, paste0(label, ".log")), stderr = file.path(directory, paste0(label, ".log")))
    if (status != 0L) stop("PostgreSQL acceptance failed: ", label, "; see ", directory)
    if (i %% 18L == 0L) cat(i, "/", nrow(cases), "cases passed\n")
  }
  cat(nrow(cases), "fresh-process PostgreSQL cases passed; evidence:", directory, "\n")
}
