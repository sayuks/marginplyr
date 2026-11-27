# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# SQLite compute()'s destination-name probe takes a time limit as its conservative answer; the delay is injected by a trace (#1805).
# Candidate 2: compute() of a direct SQLite Margin result consumes a time limit
# that expires while it asks SQLite whether the destination name resolves
# (`EXPLAIN SELECT 1 FROM <name>`), and returns.
# A 0.5 s round trip is simulated for that one statement by tracing RSQLite's
# dbSendQuery method (a dependency). Each part runs in its own fresh process.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
Sys.setenv(LANGUAGE = "en")
args <- commandArgs(TRUE)
if (length(args) == 0L) {
  self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  for (part in c("control", "explain", "insert", "native")) {
    system2(file.path(R.home("bin"), "Rscript"), c(shQuote(self), part))
  }
  quit(status = 0)
}
part <- args[[1]]
slow <- function(pattern) {
  assign(".slow_pattern", pattern, envir = globalenv())
  suppressMessages(trace("dbSendQuery", signature = c("SQLiteConnection", "character"),
    tracer = quote(if (startsWith(statement, .slow_pattern)) {
      # A busy wait, so R checks the limit while the statement is "running".
      .t0 <- proc.time()[[3L]]; while (proc.time()[[3L]] - .t0 < 0.5) NULL
    }),
    where = asNamespace("RSQLite"), print = FALSE))
}
with_limit <- function(expr, secs) {
  t0 <- Sys.time()
  took <- function() as.numeric(Sys.time() - t0, units = "secs")
  tryCatch({
    setTimeLimit(elapsed = secs, transient = TRUE)
    value <- force(expr)
    setTimeLimit(elapsed = Inf)
    list(msg = sprintf("RETURNED after %.2f s", took()), value = value)
  }, error = function(e) {
    setTimeLimit(elapsed = Inf)
    list(msg = sprintf("ERROR <%s> \"%s\" after %.2f s", class(e)[1],
                       strsplit(conditionMessage(e), "\n")[[1]][[1]], took()))
  })
}
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbWriteTable(con, "t", data.frame(g = c("a", "a", "b"), v = c(1, 2, 5)))
t <- tbl(con, "t")
r <- summarize_with_margins(t, s = sum(v, na.rm = TRUE), .grouping = rollup(g),
                            .sort = "last")
temp_tables <- function() {
  paste(DBI::dbGetQuery(con, "SELECT name FROM temp.sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%'")$name,
        collapse = ", ")
}
cat("== part:", part, "\n")
if (part == "control") {
  out <- with_limit(compute(r, name = "res", temporary = TRUE), Inf)
  cat("  compute(r, name = 'res'), no limit:", out$msg, "\n")
}
if (part == "explain") {
  # Observe (not alter) marginplyr's helper to attribute the consumed limit.
  seen <- new.env(); seen$calls <- character()
  suppressMessages(trace("sqlite_compute_name_resolves", where = asNamespace("marginplyr"),
    print = FALSE, exit = quote(seen$calls <- c(seen$calls, sprintf(
      "table=%s conservative=%s returned=%s", table, conservative, returnValue())))))
  slow("EXPLAIN SELECT 1 FROM")
  out <- with_limit(compute(r, name = "res", temporary = TRUE), 0.25)
  cat("  expected: an error (the limit ends the call), as in the 'insert' and 'native' parts\n")
  cat("  compute under a 0.25 s limit, name probe slowed to 0.5 s:", out$msg, "\n")
  cat("  name probes:", paste(seen$calls, collapse = "; "), "\n")
  cat("  (no relation 'res' existed, so the probe's own answer is FALSE)\n")
  if (!is.null(out$value)) print(collect(out$value))
  cat("  temporary tables now:", temp_tables(), "\n")
}
if (part == "insert") {
  slow("INSERT INTO")
  out <- with_limit(compute(r, name = "res", temporary = TRUE), 0.25)
  cat("  same compute, the INSERT slowed instead:", out$msg, "\n")
  cat("  temporary tables now:", temp_tables(), "\n")
}
if (part == "native") {
  slow("EXPLAIN SELECT 1 FROM")
  out <- with_limit(DBI::dbGetQuery(con, "EXPLAIN SELECT 1 FROM \"res\"", n = 1L), 0.25)
  cat("  native: the same statement through DBI under 0.25 s:", out$msg, "\n")
  suppressMessages(untrace("dbSendQuery", signature = c("SQLiteConnection", "character"),
                           where = asNamespace("RSQLite")))
  slow("CREATE TEMPORARY TABLE")
  n <- arrange(summarise(group_by(t, g), s = sum(v, na.rm = TRUE)), g)
  out <- with_limit(compute(n, name = "res2", temporary = TRUE), 0.25)
  cat("  native compute(arrange()) with its CREATE slowed, under 0.25 s:", out$msg, "\n")
}
