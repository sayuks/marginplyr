# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# DuckDB 2.0 reads a backtick span as a quoted identifier and the preflight tokenizer does not, so a CSV relation, a sniffing reader or a caller macro written with backticks is bound by the zero-row metadata query.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({library(dplyr)})
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
cat("duckdb", as.character(packageVersion("duckdb")), DBI::dbGetQuery(con, "select version() v")$v, "\n")
dir <- tempfile("cand1"); dir.create(dir); path <- file.path(dir, "p.csv")
qs <- DBI::dbQuoteString(con, path); qi <- DBI::dbQuoteIdentifier(con, path)
DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = 1:2))
writeLines(c("k", "101"), path)
invisible(DBI::dbExecute(con, paste0("CREATE MACRO hidden_first(x) AS (SELECT first(k) FROM read_csv_auto(", qs, "))")))
plan <- function(x) tryCatch(
  paste0("(", paste(inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included, collapse = ", "), ")"),
  marginplyr_error = function(e) "Package refusal",
  error = function(e) { while (!is.null(e$parent)) e <- e$parent; paste("External:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 60)) })
native <- function(x) tryCatch(paste(vapply(collect(head(x, 0)), function(c) class(c)[1], ""), collapse = ","),
  error = function(e) "native error")
both <- function(mk) {
  writeLines(c("k", "101"), path); a <- c(plan(mk()), native(mk()))
  writeLines(c("k", "source_tail"), path); b <- c(plan(mk()), native(mk()))
  sprintf("plan %-18s -> %-18s | native types %s -> %s", a[1], b[1], a[2], b[2])
}
src <- function(sql, vars = "k") function() tbl(con, dbplyr::sql(sql), vars = vars)
cat("Expected for every row below: 'Package refusal' for both files (or one plan for both).\n\n")
rows <- list(
  "FROM `p.csv`" = src(paste0("SELECT * FROM `", path, "`")),
  "TABLE `p.csv`" = src(paste0("TABLE `", path, "`")),
  "FROM `read_csv_auto`('p.csv')" = src(paste0("SELECT * FROM `read_csv_auto`(", qs, ")")),
  "SELECT `hidden_first`(v) (caller macro)" = src("SELECT g, v, `hidden_first`(v) AS y FROM t", c("g", "v", "y")),
  "mutate(y = sql('`hidden_first`(v)'))" = function() mutate(tbl(con, "t"), y = sql("`hidden_first`(v)")),
  "control: FROM \"p.csv\"" = src(paste0("SELECT * FROM ", qi)),
  "control: FROM read_csv_auto('p.csv')" = src(paste0("SELECT * FROM read_csv_auto(", qs, ")")),
  "control: SELECT hidden_first(v)" = src("SELECT g, v, hidden_first(v) AS y FROM t", c("g", "v", "y")),
  "control: explicit-schema reader" = src(paste0("SELECT * FROM read_csv(", qs, ", columns = {'k': 'VARCHAR'}, auto_detect = false, header = true)"))
)
for (n in names(rows)) cat(sprintf("%-42s %s\n", n, both(rows[[n]])))
cat("\nSent queries for summarize_with_margins() over FROM `p.csv` (audit on):\n")
writeLines(c("g,k", "a,101"), path)
options(marginplyr.audit_sql = TRUE)
x <- tbl(con, dbplyr::sql(paste0("SELECT * FROM `", path, "`")), vars = c("g", "k"))
res <- summarize_with_margins(x, k = sum(k), .grouping = rollup(g))
print(last_sent_queries(include_result = FALSE))
cat("\nAbsent file: native lazy table builds; typed inspection reaches:\n")
unlink(path)
x <- tbl(con, dbplyr::sql(paste0("SELECT * FROM `", path, "`")), vars = "k")
cat("  backtick:", plan(x), "\n  double quote:", plan(tbl(con, dbplyr::sql(paste0("SELECT * FROM ", qi)), vars = "k")), "\n")
cat("\nOver-refusal neighbour (CTE named with backticks, referenced with double quotes):\n")
cat("  WITH `p.csv` AS (SELECT 1 AS k) SELECT * FROM \"p.csv\":", plan(src(paste0("WITH `", path, "` AS (SELECT 1 AS k) SELECT * FROM ", qi))()), "\n")
cat("  WITH \"p.csv\" AS (SELECT 1 AS k) SELECT * FROM \"p.csv\":", plan(src(paste0("WITH ", qi, " AS (SELECT 1 AS k) SELECT * FROM ", qi))()), "\n")
DBI::dbDisconnect(con, shutdown = TRUE); unlink(dir, recursive = TRUE)
