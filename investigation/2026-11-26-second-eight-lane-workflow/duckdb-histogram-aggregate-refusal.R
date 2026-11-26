# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# The histogram() aggregate of DuckDB is refused as a macro call because a system table macro shares its name.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({library(dplyr)})
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
cat("duckdb", as.character(packageVersion("duckdb")), DBI::dbGetQuery(con, "select version() v")$v, "\n")
print(DBI::dbGetQuery(con, "select function_name, function_type, database_name from duckdb_functions() where function_name = 'histogram'"))
cat("caller macros:", nrow(DBI::dbGetQuery(con, "select 1 from duckdb_functions() where function_type in ('macro','table_macro') and database_name <> 'system'")), "\n")
DBI::dbWriteTable(con, "t", data.frame(g = c("a", "a", "b"), v = c(1L, 1L, 2L)))
plan <- function(x) tryCatch(
  paste0("(", paste(inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included, collapse = ", "), ")"),
  marginplyr_error = function(e) paste("Package refusal:", strsplit(conditionMessage(e), "\n")[[1]][1]))
native <- function(x) paste(vapply(collect(head(x, 0)), function(c) class(c)[1], ""), collapse = ",")
sql <- "SELECT g, histogram(v) AS h, sum(v) AS v FROM t GROUP BY g"
authored <- tbl(con, dbplyr::sql(sql), vars = c("g", "h", "v"))
verb <- summarise(group_by(tbl(con, "t"), g), h = histogram(v), v = sum(v))
ctl <- summarise(group_by(tbl(con, "t"), g), h = list(v), v = sum(v))
cat("\nExpected for each: (v)\n")
cat(sprintf("%-55s plan: %-80s native zero-row types: %s\n", "authored histogram(v) aggregate", plan(authored), native(authored)))
cat(sprintf("%-55s plan: %-80s native zero-row types: %s\n", "summarise(h = histogram(v))", plan(verb), native(verb)))
cat(sprintf("%-55s plan: %-80s native zero-row types: %s\n", "control: summarise(h = list(v))", plan(ctl), native(ctl)))
collected <- collect(verb)
cat(sprintf("%-55s plan: %s\n", "collected data frame of the same pipeline", plan(collected)))
cat("\nA typed Margin summary over the aggregate pipeline:\n")
r <- tryCatch(collect(summarize_with_margins(verb, v = sum(v), .grouping = rollup(g))), error = function(e) paste("refused:", conditionMessage(e)))
print(r)
cat("\nOracle (native per-grain summaries of the collected input): \n")
print(bind_rows(summarise(group_by(collected, g), v = sum(v)), summarise(collected, v = sum(v))))
cat("\nThe table-macro form stays refused (pinned by test-duckdb-authored-calls.R):",
    plan(tbl(con, dbplyr::sql("SELECT * FROM histogram('t', v)"), vars = c("bin", "count", "bar"))), "\n")
DBI::dbDisconnect(con, shutdown = TRUE)
