# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Caller macros named factorial or regexp_matches are bound from syntax on duckdb 2.0 (#1818).
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({library(dplyr)})
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
cat("duckdb", as.character(packageVersion("duckdb")), DBI::dbGetQuery(con, "select version() v")$v, "\n")
dir <- tempfile("cand1"); dir.create(dir); path <- file.path(dir, "p.csv")
qs <- DBI::dbQuoteString(con, path)
DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = 1:2))
plan <- function(x) tryCatch(
  paste0("(", paste(inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included, collapse = ", "), ")"),
  marginplyr_error = function(e) "Package refusal",
  error = function(e) { while (!is.null(e$parent)) e <- e$parent; paste("External:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 40)) })
both <- function(mk) {
  writeLines(c("k", "101"), path); a <- plan(mk())
  writeLines(c("k", "source_tail"), path); b <- plan(mk())
  sprintf("plan %-18s -> %-18s", a, b)
}
macro <- function(name, args) invisible(DBI::dbExecute(con, paste0(
  "CREATE OR REPLACE MACRO ", DBI::dbQuoteIdentifier(con, name), "(", args,
  ") AS (SELECT first(k) FROM read_csv_auto(", qs, "))")))
drop <- function(name) invisible(DBI::dbExecute(con, paste0("DROP MACRO ", DBI::dbQuoteIdentifier(con, name))))
src <- function(sql, vars = c("g", "v", "y")) function() tbl(con, dbplyr::sql(sql), vars = vars)
cat("Expected (ADR 0020, #1780): 'Package refusal' for both files whenever a caller macro the syntax binds exists.\n")
cat("Observed plan changing between the files means the zero-row metadata query read p.csv.\n")
cat("\n-- baseline: no caller macro (one plan for both files expected)\n")
cat(sprintf("%-48s %s\n", "authored: 5! AS y", both(src("SELECT g, v, 5! AS y FROM t"))))
cat(sprintf("%-48s %s\n", "authored: 'a' ~ 'a' AS y", both(src("SELECT g, v, 'a' ~ 'a' AS y FROM t"))))
for (spec in list(
  list(name = "factorial", args = "a", syntax = "5! AS y", call = "factorial(5) AS y"),
  list(name = "regexp_matches", args = "a, b", syntax = "'a' ~ 'a' AS y", call = "regexp_matches('a', 'a') AS y"),
  list(name = "regexp_matches", args = "a, b", syntax = "'a' ~* 'a' AS y", call = "regexp_matches('a', 'a', 'i') AS y"))) {
  if (spec$call == "regexp_matches('a', 'a', 'i') AS y") next
  macro(spec$name, spec$args)
  cat(sprintf("\n-- caller macro named %s\n", spec$name))
  cat(sprintf("%-48s %s\n", paste("authored (no call head):", spec$syntax), both(src(paste("SELECT g, v,", spec$syntax, "FROM t")))))
  cat(sprintf("%-48s %s\n", paste("control (call spelling):", spec$call), both(src(paste("SELECT g, v,", spec$call, "FROM t")))))
  drop(spec$name)
}
macro("+", "a, b")
cat("\n-- control: caller macro named \"+\" (a listed name)\n")
cat(sprintf("%-48s %s\n", "authored: v + 1 AS y", both(src("SELECT g, v, v + 1 AS y FROM t"))))
drop("+")
DBI::dbDisconnect(con, shutdown = TRUE); unlink(dir, recursive = TRUE)
