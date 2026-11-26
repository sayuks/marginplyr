# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A time limit that expires while a share request asks its dialect question is consumed by the package's own handler: the Margin verb returns after the limit, and on SQLite "refuses" is recorded, so a later share over a character source is calculated.
# A 0.5 s round trip is simulated for the one statement by tracing the driver's dbSendQuery method, a dependency. Each part runs in its own process; "remedy" replays one candidate answer in memory.
# The PostgreSQL part runs only when PGHOST names a reachable server; it creates no object.
args <- commandArgs(TRUE)
if (length(args) == 0L) {
  self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  for (part in c("control", "limit", "remedy", "postgres")) system2(file.path(R.home("bin"), "Rscript"), c(shQuote(self), part))
  quit(status = 0)
}
part <- args[[1]]
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
slow <- function(pkg, cls, pattern) {
  assign(".slow_pattern", pattern, envir = globalenv())
  suppressMessages(trace("dbSendQuery", signature = c(cls, "character"),
                         tracer = quote(if (grepl(.slow_pattern, statement, fixed = TRUE)) Sys.sleep(0.5)),
                         where = asNamespace(pkg), print = FALSE))
}
with_limit <- function(expr, secs) {
  t0 <- Sys.time()
  took <- function() as.numeric(Sys.time() - t0, units = "secs")
  tryCatch({
    setTimeLimit(elapsed = secs, transient = TRUE)
    force(expr)
    setTimeLimit(elapsed = Inf)
    sprintf("RETURNED after %.1f s", took())
  }, error = function(e) {
    setTimeLimit(elapsed = Inf)
    sprintf("ERROR <%s> after %.1f s", class(e)[1], took())
  })
}
show_record <- function() {
  rec <- last_sent_queries(details = TRUE, include_result = FALSE)
  for (i in seq_len(nrow(rec))) cat(sprintf("    %-26s %-9s %s\n", rec$purpose[i], rec$status[i], paste(rec$conditions[[i]]$handling, collapse = ",")))
}
sqlite_input <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "a", "b"), code = c("10", "20", "30"), v = c(1, 2, 5)))
  tbl(con, "t")
}
numeric_share <- function(t) summarize_with_margins(t, s = sum(v, na.rm = TRUE), p = share_of_total(s), .grouping = rollup(g))
character_share <- function(t) tryCatch({
  x <- collect(summarize_with_margins(t, s = max(code, na.rm = TRUE), p = share_of_total(s), .grouping = rollup(g)))
  paste("RETURNED", paste0(x$g, " ", x$s, " -> ", round(x$p, 3), collapse = ", "))
}, error = function(e) paste0("ERROR <", class(e)[1], ">"))
options(marginplyr.audit_sql = TRUE)
sqlite_part <- function() {
  t <- sqlite_input()
  slow("RSQLite", "SQLiteConnection", "SUM('x'")
  cat("  native control, a statement of the same latency through DBI under a 0.25 s limit:",
      with_limit(DBI::dbGetQuery(dbplyr::remote_con(t), "SELECT 1 AS p -- SUM('x')"), 0.25), "\n")
  cat("  numeric share under a 0.25 s limit:", with_limit(numeric_share(t), 0.25), "\n")
  show_record()
  suppressMessages(untrace("dbSendQuery", signature = c("SQLiteConnection", "character"), where = asNamespace("RSQLite")))
  cat("  later character-source share, no limit:", character_share(t), "\n")
}
if (part == "control") {
  cat("== SQLite, no time limit: both shares are refused, since SQLite converts a string it is asked to sum\n")
  t <- sqlite_input()
  cat("  numeric share  :", with_limit(numeric_share(t), Inf), "\n")
  cat("  character share:", character_share(t), "\n")
}
if (part == "limit") {
  cat("== SQLite, the limit expires during the dialect question\n")
  sqlite_part()
}
if (part == "remedy") {
  cat("== SQLite, the same, with the question's handler run under suspendInterrupts() (in memory)\n")
  ns <- asNamespace("marginplyr")
  patched <- function(con, expr, purpose, control = FALSE) {
    scaffold <- share_probe_scaffold()$sql
    query <- tryCatch(dplyr::summarize(dplyr::tbl(con, scaffold, vars = "z"), p = !!expr), error = function(cnd) NULL)
    if (is.null(query)) return("unanswerable")
    answer <- suspendInterrupts(tryCatch(
      list(value = suppressMessages(suppressWarnings(with_sent_query(
        purpose, query, dplyr::collect(query), error_handling = "handled", warning_handling = "suppressed"
      )))),
      error = function(cnd) NULL
    ))
    if (is.null(answer)) return("raised")
    value <- answer$value
    if (!is.data.frame(value) || nrow(value) != 1L || ncol(value) != 1L) return("unanswerable")
    if (!probe_share_dialect_holds(value[[1L]], control = control)) return("unanswerable")
    "answered"
  }
  environment(patched) <- ns
  unlockBinding("probe_share_dialect_answer", ns)
  assign("probe_share_dialect_answer", patched, ns)
  sqlite_part()
}
if (part == "postgres") {
  cat("== PostgreSQL inside dbBegin(), the limit expires during the dialect question\n")
  pg <- if (nzchar(Sys.getenv("PGHOST")) && requireNamespace("RPostgres", quietly = TRUE)) {
    tryCatch(DBI::dbConnect(RPostgres::Postgres()), error = function(e) NULL)
  }
  if (is.null(pg)) {
    cat("  skipped: no PostgreSQL server reachable through PGHOST\n")
  } else {
    pt <- tbl(pg, dbplyr::sql("SELECT * FROM (VALUES ('a', 1.0), ('a', 2.0), ('b', 5.0)) AS v(g, v)"), vars = c("g", "v"))
    DBI::dbBegin(pg)
    slow("RPostgres", "PqConnection", "SUM('x'")
    cat("  numeric share under a 0.25 s limit:", with_limit(numeric_share(pt), 0.25), "\n")
    show_record()
    cat("  caller transaction usable afterwards:", tryCatch({
      DBI::dbGetQuery(pg, "SELECT 1 AS one")
      TRUE
    }, error = function(e) FALSE), "\n")
    DBI::dbRollback(pg)
    DBI::dbDisconnect(pg)
  }
}
