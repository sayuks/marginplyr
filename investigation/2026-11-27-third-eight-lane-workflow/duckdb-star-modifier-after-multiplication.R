# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Second round: exclude(), replace() and rename() after a multiplication are dropped as star modifiers by the scan (#1836).
# Candidate 3: validate_duckdb_authored_calls() drops any `exclude(`, `replace(`
# or `rename(` head whose previous token is `*`, treating it as a star
# modifier. After a multiplication (`v * exclude(v)`) DuckDB binds a call, so a
# caller macro of that name is expanded by the zero-row metadata query.
args <- commandArgs(TRUE)
old <- length(args) > 0 && args[[1]] == "old"
if (old) {
  local({ c0 <- path.expand("~/Library/Caches/org.R-project.R/R/marginplyr/duckdb-2-compatibility"); .libPaths(c(file.path(c0, "1.5.6"), file.path(c0, "shared"), .libPaths())) }) # hunt-duckdb-old
}
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
cat("duckdb", as.character(packageVersion("duckdb")), "\n")
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
csv <- tempfile(fileext = ".csv"); qs <- DBI::dbQuoteString(con, csv); writeLines(c("k", "101"), csv)
invisible(DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = 1:2)))
for (m in c("exclude", "replace", "rename", "excl")) invisible(DBI::dbExecute(con, paste0(
  "CREATE MACRO ", DBI::dbQuoteIdentifier(con, m), "(a) AS (SELECT first(k) FROM read_csv_auto(", qs, "))")))
outcome <- function(x) tryCatch(
  paste0("plan ", paste(inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included, collapse = " ")),
  marginplyr_error = function(e) paste0("Package refusal: ", strsplit(conditionMessage(e), "\n")[[1]][1]),
  error = function(e) { while (!is.null(e$parent)) e <- e$parent; paste0("External: ", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 90)) })
native <- function(x) tryCatch(class(collect(head(x, 0))$y)[1], error = function(e) { while (!is.null(e$parent)) e <- e$parent; paste("ERR", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 60)) })
lz <- function(sql) tbl(con, dbplyr::sql(sql), vars = c("g", "v", "y"))
forms <- list(
  "v * exclude(v)" = lz("SELECT g, v, v * exclude(v) AS y FROM t"),
  "v * replace(v)" = lz("SELECT g, v, v * replace(v) AS y FROM t"),
  "v * rename(v)"  = lz("SELECT g, v, v * rename(v) AS y FROM t"),
  "dplyr mutate(y = v * exclude(v))" = mutate(tbl(con, "t"), y = v * exclude(v)),
  "control: exclude(v) without *" = lz("SELECT g, v, exclude(v) AS y FROM t"),
  "control: v * excl(v)" = lz("SELECT g, v, v * excl(v) AS y FROM t"),
  "control: real star modifier, SELECT * EXCLUDE (v)" = tbl(con, dbplyr::sql("SELECT * EXCLUDE (v), 1 AS v, 2 AS y FROM t"), vars = c("g", "v", "y"))
)
for (nm in names(forms)) {
  cat("\n==", nm, "\n")
  for (val in c("101", "source_tail")) {
    writeLines(c("k", val), csv)
    cat(sprintf("  file row %-12s native zero-row y: %-45s marginplyr typed: %s\n", val, native(forms[[nm]]), outcome(forms[[nm]])))
  }
  unlink(csv); cat(sprintf("  file absent        marginplyr typed: %s\n", outcome(forms[[nm]]))); writeLines(c("k", "101"), csv)
}
options(marginplyr.audit_sql = TRUE)
s <- tryCatch(summarize_with_margins(forms[[1]], n = n(), .grouping = rollup(where(is.character))), error = function(e) e)
cat("\nsummarize_with_margins(rollup(where(is.character))):", if (inherits(s, "error")) conditionMessage(s) else "returned", "\n")
print(as.data.frame(last_sent_queries(details = TRUE))[, c("purpose", "status")])
cat("expected: Package refusal 'Can't safely determine column metadata for this DuckDB call.' for the first four forms, after relation_catalog only\n")
DBI::dbDisconnect(con, shutdown = TRUE)
