# Investigated SHA: 6165f4768400a28520cc8977e056da0db49a32ed
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Authored SQL and stored views calling one of DuckDB's own macros (nullif, date_add, list_sum) are refused as macro calls, also when the catalog holds no caller macro.
# The last block shows the two cases in which such a call can reach source work, which a repair has to keep refusing.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
typed_plan <- function(x) tryCatch(
  paste(inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included, collapse = " "),
  marginplyr_error = function(e) paste("Package refusal:", sub(".*this DuckDB ", "DuckDB ", strsplit(conditionMessage(e), "\n")[[1]][1])),
  error = function(e) {
    while (!is.null(e$parent)) e <- e$parent
    paste("DuckDB error:", sub(":.*", "", gsub("\\s+", " ", conditionMessage(e))))
  }
)
fresh <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = c(1L, 2L), d = as.Date(c("2026-01-01", "2026-01-02"))))
  con
}
zero_row_types <- function(con, sql) tryCatch(
  paste(vapply(DBI::dbGetQuery(con, paste0("SELECT * FROM (", sql, ") LIMIT 0")), function(column) class(column)[1], ""), collapse = ","),
  error = function(e) paste("DuckDB error:", sub(":.*", "", gsub("\\s+", " ", conditionMessage(e))))
)
con <- fresh()
macros <- DBI::dbGetQuery(con, "select function_type, internal, database_name, count(distinct function_name) as names from duckdb_functions() where function_type in ('macro', 'table_macro') group by all order by 1")
cat("macros in a new database:\n"); print(macros)
cat("\n-- no caller macro in the catalog\n")
for (sql in c(
  "SELECT g, nullif(v, 0) AS v FROM t",
  "SELECT g, date_add(d, INTERVAL 1 DAY) AS d2, v FROM t",
  "SELECT g, list_sum([v, v]) AS v FROM t",
  "SELECT g, array_to_string([g, g], '-') AS s, v FROM t",
  "SELECT g, current_database() AS db, v FROM t",
  "SELECT g, coalesce(v, 0) AS v FROM t",
  "SELECT g, CASE WHEN v = 0 THEN NULL ELSE v END AS v FROM t"
)) cat(sprintf("%-58s %-32s native zero-row types: %s\n", sql, typed_plan(tbl(con, dbplyr::sql(sql))), zero_row_types(con, sql)))
invisible(DBI::dbExecute(con, "CREATE VIEW vn AS SELECT g, nullif(v, 0) AS v FROM t"))
cat(sprintf("%-58s %s\n", "stored view over nullif(v, 0)", typed_plan(tbl(con, "vn"))))
t <- tbl(con, "t")
cat(sprintf("%-58s %s\n", "mutate(y = sql('nullif(v, 0)'))", typed_plan(mutate(t, y = dbplyr::sql("nullif(v, 0)")))))
cat(sprintf("%-58s %s   rendered: %s\n", "mutate(y = na_if(v, 0L)), translated", typed_plan(mutate(t, y = na_if(v, 0L))), gsub("\\s+", " ", as.character(dbplyr::sql_render(mutate(t, y = na_if(v, 0L)))))))
DBI::dbDisconnect(con, shutdown = TRUE)

cat("\n-- what a repair has to keep refusing\n")
path <- tempfile(fileext = ".csv")
flip <- function(con, sql) {
  writeLines(c("k", "101"), path)
  first <- zero_row_types(con, sql)
  writeLines(c("k", "source_tail"), path)
  paste(first, "|", zero_row_types(con, sql))
}
con <- fresh()
quoted <- DBI::dbQuoteString(con, path)
histogram <- paste0("SELECT * FROM histogram_values(", quoted, ", k)")
cat("native zero-row types, FROM histogram_values('<csv>', k), numeric row | text row:", flip(con, histogram), "\n")
writeLines(c("k", "101"), path)
cat("Margin over it:", typed_plan(tbl(con, dbplyr::sql(histogram), vars = c("bin", "count"))), "\n")
cat("definition of histogram_values reads:", grepl("query_table", DBI::dbGetQuery(con, "select macro_definition from duckdb_functions() where function_name = 'histogram_values' limit 1")[[1]], fixed = TRUE), "(query_table)\n")
invisible(DBI::dbExecute(con, paste0("CREATE MACRO list_aggr(l, f) AS (SELECT k FROM read_csv_auto(", quoted, ") LIMIT 1)")))
cat("caller macro list_aggr(); native zero-row types of list_sum([v]), numeric row | text row:", flip(con, "SELECT list_sum([v]) AS s FROM t"), "\n")
writeLines(c("k", "101"), path)
cat("Margin over SELECT list_sum([v]) AS s FROM t:", typed_plan(tbl(con, dbplyr::sql("SELECT list_sum([v]) AS s FROM t"))), "\n")
invisible(DBI::dbExecute(con, "CREATE MACRO nullif(a, b) AS 'shadow'"))
cat("caller macro nullif(); SELECT nullif(1, 2):", DBI::dbGetQuery(con, "SELECT nullif(1, 2) AS x")$x, "; system.main.nullif(1, 2):", DBI::dbGetQuery(con, "SELECT system.main.nullif(1, 2) AS x")$x, "\n")
unlink(path)
DBI::dbDisconnect(con, shutdown = TRUE)
