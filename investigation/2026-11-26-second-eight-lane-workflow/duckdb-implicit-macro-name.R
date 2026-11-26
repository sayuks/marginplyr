# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A caller macro named after an operator or a function DuckDB binds without a call head is reached by the zero-row metadata query with no head for the scan to inspect.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({library(dplyr)})
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
cat("duckdb", as.character(packageVersion("duckdb")), DBI::dbGetQuery(con, "select version() v")$v, "\n")
dir <- tempfile("cand3"); dir.create(dir); path <- file.path(dir, "p.csv")
qs <- DBI::dbQuoteString(con, path)
DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = 1:2))
writeLines(c("k", "101"), path)
plan <- function(x) tryCatch(
  paste0("(", paste(inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included, collapse = ", "), ")"),
  marginplyr_error = function(e) "Package refusal",
  error = function(e) { while (!is.null(e$parent)) e <- e$parent; paste("External:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 60)) })
native <- function(x) tryCatch(paste(vapply(collect(head(x, 0)), function(c) class(c)[1], ""), collapse = ","),
  error = function(e) "native error")
both <- function(mk) {
  writeLines(c("k", "101"), path); a <- c(plan(mk()), native(mk()))
  writeLines(c("k", "source_tail"), path); b <- c(plan(mk()), native(mk()))
  sprintf("plan %-16s -> %-16s | native %s -> %s", a[1], b[1], a[2], b[2])
}
macro <- function(name, args) invisible(DBI::dbExecute(con, paste0(
  "CREATE OR REPLACE MACRO ", DBI::dbQuoteIdentifier(con, name), "(", args,
  ") AS (SELECT first(k) FROM read_csv_auto(", qs, "))")))
drop <- function(name) invisible(DBI::dbExecute(con, paste0("DROP MACRO ", DBI::dbQuoteIdentifier(con, name))))
src <- function(sql, vars = c("g", "v", "y")) function() tbl(con, dbplyr::sql(sql), vars = vars)
view <- function(body) function() { DBI::dbExecute(con, paste0("CREATE OR REPLACE VIEW mv AS ", body)); tbl(con, "mv") }
t <- function() tbl(con, "t")
cat("\nExpected (ADR 0020 #1650): 'Package refusal' for both files whenever a caller macro is reached.\n")
macro("+", "a, b")
cat("\n-- caller macro named \"+\"\n")
cat(sprintf("%-50s %s\n", "dplyr: mutate(t, y = v + 1L)", both(function() mutate(t(), y = v + 1L))))
cat(sprintf("%-50s %s\n", "authored: SELECT g, v, v + 1 AS y FROM t", both(src("SELECT g, v, v + 1 AS y FROM t"))))
cat(sprintf("%-50s %s\n", "stored view: SELECT g, v, v + 1 AS y FROM t", both(view("SELECT g, v, v + 1 AS y FROM t"))))
cat(sprintf("%-50s %s\n", "control: authored \"+\"(v, 1) (call spelling)", both(src("SELECT g, v, \"+\"(v, 1) AS y FROM t"))))
drop("+")
macro("count_star", "")
cat("\n-- caller macro named count_star\n")
cat(sprintf("%-50s %s\n", "dplyr: count(t, g)", both(function() count(t(), g))))
cat(sprintf("%-50s %s\n", "dplyr: summarise(group_by(t, g), n = n())", both(function() summarise(group_by(t(), g), n = n()))))
cat(sprintf("%-50s %s\n", "control: authored count_star() call", both(src("SELECT g, count_star() AS n FROM t GROUP BY g", c("g", "n")))))
drop("count_star")
macro("struct_pack", "a")
cat("\n-- caller macro named struct_pack\n")
cat(sprintf("%-50s %s\n", "authored: {'a': v}", both(src("SELECT g, v, {'a': v} AS y FROM t"))))
cat(sprintf("%-50s %s\n", "control: authored struct_pack(v)", both(src("SELECT g, v, struct_pack(v) AS y FROM t"))))
drop("struct_pack")
macro("date_part", "a, b")
cat("\n-- caller macro named date_part\n")
cat(sprintf("%-50s %s\n", "authored: EXTRACT(year FROM DATE '2020-01-01')", both(src("SELECT g, v, EXTRACT(year FROM DATE '2020-01-01') AS y FROM t"))))
drop("date_part")
cat("\n-- controls with no caller macro (one plan for both files expected and observed)\n")
cat(sprintf("%-50s %s\n", "mutate(t, y = v + 1L)", both(function() mutate(t(), y = v + 1L))))
cat(sprintf("%-50s %s\n", "count(t, g)", both(function() count(t(), g))))
cat("\nAudit of summarize_with_margins() over count(t, g) with macro count_star:\n")
macro("count_star", "")
writeLines(c("k", "101"), path)
options(marginplyr.audit_sql = TRUE)
res <- summarize_with_margins(count(t(), g), n = sum(n), .grouping = rollup(g))
print(last_sent_queries(include_result = FALSE))
DBI::dbDisconnect(con, shutdown = TRUE); unlink(dir, recursive = TRUE)
