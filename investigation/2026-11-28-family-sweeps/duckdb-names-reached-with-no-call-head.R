# Investigated SHA: 1680498dbfc37296d0d20ea4352606c48441a16e
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Caller DuckDB macros reached from statements that write no call head for them (#1843).
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
if (!requireNamespace("duckdb", quietly = TRUE)) {
  cat("duckdb is not installed; nothing was run.\n")
  quit(status = 0)
}
# The preflight as it stood at the investigated SHA, whatever the tree holds.
namespace <- asNamespace("marginplyr")
assignInNamespace("duckdb_implicit_functions", c(
  "count_star", "list_value", "struct_pack", "row", "map", "date_part",
  "timezone", "ltrim", "rtrim", "regexp_full_match", "like_escape",
  "ilike_escape", "list_apply", "contains", "to_millennia", "to_centuries",
  "to_decades", "to_years", "to_quarters", "to_months", "to_weeks", "to_days",
  "to_hours", "to_minutes", "to_seconds", "to_milliseconds", "to_microseconds",
  "trunc", "factorial", "regexp_matches"
), namespace)
if (exists("duckdb_rewritten_statements", namespace, inherits = FALSE)) {
  assignInNamespace("duckdb_rewritten_statements", character(), namespace)
}
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
dir <- tempfile()
dir.create(dir)
path <- file.path(dir, "p.csv")
qs <- DBI::dbQuoteString(con, path)
pq <- DBI::dbQuoteString(con, file.path(dir, "u.parquet"))
invisible(DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = 1:2, w = 3:4)))
invisible(DBI::dbExecute(con, paste("COPY t TO", pq, "(FORMAT parquet)")))
plan <- function(input) {
  tryCatch(paste0("plan ", paste(inspect_grouping(
    input, .grouping = grouping_set(where(is.numeric))
  )$included, collapse = " ")), marginplyr_error = function(e) "Package refusal",
  error = function(e) {
    while (!is.null(e$parent)) e <- e$parent
    paste("External:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 42))
  })
}
cases <- list(
  list("list_filter", "(a, b) AS (SELECT first(k) FROM read_csv_auto(%s))",
       "SELECT g, v, [e FOR e IN [v] IF e > 0] AS y FROM t", FALSE),
  list("array_agg", "(a) AS (SELECT first(k) FROM read_csv_auto(%s))",
       "SELECT g, v, ARRAY(SELECT w FROM t) AS y FROM t", FALSE),
  list("unpivot_list", "(a, b) AS (SELECT first(k) FROM read_csv_auto(%s))",
       "UNPIVOT t ON v, w INTO NAME n VALUE e", FALSE),
  list("parquet_scan", "(a) AS TABLE SELECT g, k AS v FROM (SELECT 'a' AS g), read_csv_auto(%s)",
       paste("SELECT g, v FROM", pq), TRUE),
  list("duckdb_tables", "() AS TABLE SELECT k AS name FROM read_csv_auto(%s)", "SHOW TABLES", TRUE),
  list("min", "(a) AS (SELECT first(k) FROM read_csv_auto(%s))", "SUMMARIZE t", FALSE)
)
cat("duckdb", as.character(packageVersion("duckdb")), "\n")
cat("Expected (ADR 0020): 'Package refusal' in every column.\n\n")
cat(sprintf("%-14s %-34s %s\n", "caller macro", "file holds 101 | text", "file absent"))
for (case in cases) {
  # dbplyr binds a statement to learn its columns, so the input is captured first.
  input <- tbl(con, dbplyr::sql(case[[3]]))
  writeLines(c("k", "101"), path)
  invisible(DBI::dbExecute(con, paste0("CREATE MACRO ", case[[1]], sprintf(case[[2]], qs))))
  number <- plan(input)
  writeLines(c("k", "source_tail"), path)
  text <- plan(input)
  unlink(path)
  cat(sprintf("%-14s %-34s %s\n", case[[1]], paste(number, "|", text), plan(input)))
  invisible(DBI::dbExecute(con, paste("DROP MACRO", if (case[[4]]) "TABLE", case[[1]])))
}
DBI::dbDisconnect(con, shutdown = TRUE)
unlink(dir, recursive = TRUE)
