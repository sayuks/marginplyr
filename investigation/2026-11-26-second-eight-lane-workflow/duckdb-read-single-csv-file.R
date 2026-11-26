# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# DuckDB 2.0's read_single_csv_file() infers CSV types at bind and is not on the preflight's reader list.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({library(dplyr)})
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
cat("duckdb", as.character(packageVersion("duckdb")), DBI::dbGetQuery(con, "select version() v")$v, "\n")
print(DBI::dbGetQuery(con, "select distinct function_name, function_type, internal from duckdb_functions() where function_name like 'read_%csv%'"))
dir <- tempfile("cand2"); dir.create(dir); path <- file.path(dir, "p.csv")
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
  sprintf("plan %-16s -> %-16s | native types %s -> %s", a[1], b[1], a[2], b[2])
}
src <- function(sql, vars = "k") function() tbl(con, dbplyr::sql(sql), vars = vars)
view <- function(body) function() { DBI::dbExecute(con, paste0("CREATE OR REPLACE VIEW rv AS ", body)); tbl(con, "rv") }
cat("\nExpected: 'Package refusal' for both files, as read_csv()/read_csv_auto() get.\n")
rows <- list(
  "FROM read_single_csv_file('p.csv')" = src(paste0("SELECT * FROM read_single_csv_file(", qs, ")")),
  "FROM system.main.read_single_csv_file(...)" = src(paste0("SELECT * FROM system.main.read_single_csv_file(", qs, ")")),
  "stored view over read_single_csv_file" = view(paste0("SELECT * FROM read_single_csv_file(", qs, ")")),
  "mutate(y = sql('(SELECT max(k) FROM read_single_csv_file(..))'))" = function() mutate(tbl(con, "t"), y = sql(paste0("(SELECT max(k) FROM read_single_csv_file(", qs, "))"))),
  "control: FROM read_csv('p.csv')" = src(paste0("SELECT * FROM read_csv(", qs, ")")),
  "control: FROM read_csv_auto('p.csv')" = src(paste0("SELECT * FROM read_csv_auto(", qs, ")")),
  "control: read_single_csv_file(auto_detect=false, columns)" = src(paste0("SELECT * FROM read_single_csv_file(", qs, ", auto_detect = false, header = true, columns = {'k': 'VARCHAR'})"))
)
for (n in names(rows)) cat(sprintf("%-62s %s\n", n, both(rows[[n]])))
cat("\nSent queries for summarize_with_margins() (audit on):\n")
writeLines(c("g,k", "a,101"), path)
options(marginplyr.audit_sql = TRUE)
x <- tbl(con, dbplyr::sql(paste0("SELECT * FROM read_single_csv_file(", qs, ")")), vars = c("g", "k"))
res <- summarize_with_margins(x, k = sum(k), .grouping = rollup(g))
print(last_sent_queries(include_result = FALSE))
unlink(path)
cat("\nAbsent file: native lazy table builds; typed inspection reaches:", plan(tbl(con, dbplyr::sql(paste0("SELECT * FROM read_single_csv_file(", qs, ")")), vars = "k")), "\n")
DBI::dbDisconnect(con, shutdown = TRUE); unlink(dir, recursive = TRUE)
