# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Second round: a by(...) call reaches a caller macro on duckdb 2.x and is dropped as SQL syntax by the scan (#1835).
# Candidate 1: on duckdb 2.x an unquoted `by(` call head reaches a caller macro
# named "by", but validate_duckdb_authored_calls() drops `by` as SQL syntax, so
# the typed-metadata preflight admits the statement and the zero-row metadata
# query expands the macro and samples the CSV it reads.
args <- commandArgs(TRUE)
old <- length(args) > 0 && args[[1]] == "old"
if (old) {
  local({ c0 <- path.expand("~/Library/Caches/org.R-project.R/R/marginplyr/duckdb-2-compatibility"); .libPaths(c(file.path(c0, "1.5.6"), file.path(c0, "shared"), .libPaths())) }) # hunt-duckdb-old
}
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
cat("duckdb", as.character(packageVersion("duckdb")), "\n")
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
csv <- tempfile(fileext = ".csv"); qs <- DBI::dbQuoteString(con, csv)
writeLines(c("k", "101"), csv)
invisible(DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = 1:2)))
for (m in c("by", "byx")) invisible(DBI::dbExecute(con, paste0(
  "CREATE MACRO ", DBI::dbQuoteIdentifier(con, m), "(a) AS (SELECT first(k) FROM read_csv_auto(", qs, "))")))
outcome <- function(x) tryCatch(
  paste0("plan ", paste(inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included, collapse = " ")),
  marginplyr_error = function(e) paste0("Package refusal: ", conditionMessage(e)),
  error = function(e) { while (!is.null(e$parent)) e <- e$parent; paste0("External: ", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 70)) })
native <- function(x) tryCatch(class(collect(head(x, 0))$y)[1], error = function(e) paste("ERR", substr(conditionMessage(e), 1, 50)))
forms <- list(
  "authored by(v)"        = tbl(con, dbplyr::sql("SELECT g, v, by(v) AS y FROM t"), vars = c("g", "v", "y")),
  "dplyr mutate(by(v))"   = mutate(tbl(con, "t"), y = by(v)),
  "control: quoted \"by\"(v)" = tbl(con, dbplyr::sql('SELECT g, v, "by"(v) AS y FROM t'), vars = c("g", "v", "y")),
  "control: macro byx(v)" = tbl(con, dbplyr::sql("SELECT g, v, byx(v) AS y FROM t"), vars = c("g", "v", "y"))
)
for (nm in names(forms)) {
  cat("\n==", nm, "\n")
  for (val in c("101", "source_tail")) {
    writeLines(c("k", val), csv)
    cat(sprintf("  file row %-12s native zero-row y: %-10s marginplyr typed: %s\n", val, native(forms[[nm]]), outcome(forms[[nm]])))
  }
  unlink(csv)
  cat(sprintf("  file absent        marginplyr typed: %s\n", outcome(forms[[nm]])))
}
writeLines(c("k", "101"), csv)
options(marginplyr.audit_sql = TRUE)
s <- tryCatch(summarize_with_margins(forms[[1]], n = n(), .grouping = rollup(where(is.character))), error = function(e) e)
cat("\nsummarize_with_margins(rollup(where(is.character))):", if (inherits(s, "error")) conditionMessage(s) else "returned", "\n")
print(as.data.frame(last_sent_queries(details = TRUE))[, c("purpose", "status")])
cat("expected: Package refusal 'Can't safely determine column metadata for this DuckDB call.' for every form, after relation_catalog only\n")
DBI::dbDisconnect(con, shutdown = TRUE)
