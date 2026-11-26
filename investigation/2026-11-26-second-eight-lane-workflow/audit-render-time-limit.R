# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# With the SQL audit on, a time limit that expires during a diagnostic render is recorded as a handled render condition and the call runs on; with the audit off the same call ends with the time-limit error.
# Every dbplyr::sql_render() call is delayed by 0.5 s through a trace of that dependency. Each part runs in its own process; "remedy" replays one candidate answer in memory.
args <- commandArgs(TRUE)
if (length(args) == 0L) {
  self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  for (part in c("off", "on", "remedy")) system2(file.path(R.home("bin"), "Rscript"), c(shQuote(self), part))
  quit(status = 0)
}
part <- args[[1]]
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
DBI::dbWriteTable(con, "t", data.frame(d1 = c("a", "b"), d2 = c("a", "b"), v = c(1, 2)))
t <- tbl(con, "t")
if (part == "remedy") {
  ns <- asNamespace("marginplyr")
  render <- get("render_sent_query_sql", ns)
  swap <- function(e) {
    if (!is.call(e)) return(e)
    if (identical(e, quote(as.character(dbplyr::sql_render(query))))) {
      return(quote(suspendInterrupts(as.character(dbplyr::sql_render(query)))))
    }
    as.call(lapply(e, swap))
  }
  patched <- render
  body(patched) <- swap(body(render))
  stopifnot(!identical(body(patched), body(render)))
  unlockBinding("render_sent_query_sql", ns)
  assign("render_sent_query_sql", patched, ns)
}
options(marginplyr.audit_sql = part != "off")
invisible(suppressMessages(trace("sql_render", tracer = quote(Sys.sleep(0.5)), where = asNamespace("dbplyr"), print = FALSE)))
t0 <- Sys.time()
took <- function() as.numeric(Sys.time() - t0, units = "secs")
out <- tryCatch({
  setTimeLimit(elapsed = 0.2, transient = TRUE)
  summarize_with_margins(t, s = sum(v, na.rm = TRUE), .grouping = cube(where(is.character)))
  setTimeLimit(elapsed = Inf)
  "RETURNED"
}, error = function(e) {
  setTimeLimit(elapsed = Inf)
  paste0("ERROR <", class(e)[1], ">")
})
label <- c(off = "audit off              ", on = "audit on               ", remedy = "audit on, render under suspendInterrupts() (in memory)")
cat(sprintf("%s: %s after %.1f s under a 0.2 s limit\n", label[[part]], out, took()))
if (part != "off") {
  rec <- last_sent_queries(details = TRUE)
  for (i in seq_len(nrow(rec))) {
    cnd <- rec$conditions[[i]]
    cat(sprintf("    %-18s %-9s sql %-7s %s\n", rec$purpose[i], rec$status[i], if (is.na(rec$sql[i])) "NA" else "present",
                paste(cnd$phase, cnd$handling, collapse = ", ")))
  }
}
DBI::dbDisconnect(con, shutdown = TRUE)
