# Investigated SHA: 6165f4768400a28520cc8977e056da0db49a32ed
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A DuckDB macro called through a dbplyr-translated expression is not checked, so the zero-row metadata query binds it and its read_csv_auto() sniffs the file.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = c(1L, 2L)))
path <- tempfile(fileext = ".csv")
writeLines(c("k", "101"), path)
quoted <- DBI::dbQuoteString(con, path)
invisible(DBI::dbExecute(con, paste0("CREATE MACRO hidden_first(x) AS (SELECT first(k) FROM read_csv_auto(", quoted, "))")))
invisible(DBI::dbExecute(con, paste0("CREATE MACRO fixed_first(x) AS (SELECT first(k) FROM read_csv(", quoted, ", columns = {'k': 'VARCHAR'}, auto_detect = false, header = true))")))
invisible(DBI::dbExecute(con, "CREATE MACRO add_one(x) AS x + 1"))
typed_plan <- function(x) tryCatch(
  paste(inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included, collapse = " "),
  marginplyr_error = function(e) "Package refusal",
  error = function(e) {
    while (!is.null(e$parent)) e <- e$parent
    paste("DuckDB error from the metadata query:", sub(":.*", "", gsub("\\s+", " ", conditionMessage(e))))
  }
)
both <- function(x) {
  writeLines(c("k", "101"), path)
  first <- typed_plan(x)
  writeLines(c("k", "source_tail"), path)
  sprintf("numeric row: %-16s text row: %s", first, typed_plan(x))
}
t <- tbl(con, "t")
translated <- mutate(t, y = hidden_first(v))
cat("rendered:", gsub("\\s+", " ", as.character(dbplyr::sql_render(translated))), "\n")
cat("mutate(y = hidden_first(v))        ", both(translated), "\n")
cat("mutate(y = sql('hidden_first(v)')) ", both(mutate(t, y = dbplyr::sql("hidden_first(v)"))), "\n")
cat("tbl(sql('SELECT ... hidden_first'))", both(tbl(con, dbplyr::sql("SELECT g, v, hidden_first(v) AS y FROM t"))), "\n")
cat("summarise(y = max(hidden_first(v)))", both(summarise(t, y = max(hidden_first(v), na.rm = TRUE), .by = g)), "\n")
cat("filter(hidden_first(v) > 0)        ", both(filter(t, hidden_first(v) > 0)), "\n")
cat("control: mutate(y = fixed_first(v))", both(mutate(t, y = fixed_first(v))), "\n")
cat("control: its sql() spelling        ", both(mutate(t, y = dbplyr::sql("fixed_first(v)"))), "\n")
cat("control: mutate(y = add_one(v))    ", both(mutate(t, y = add_one(v))), "\n")
cat("control: its sql() spelling        ", both(mutate(t, y = dbplyr::sql("add_one(v)"))), "\n")
writeLines(c("k", "101"), path)
options(marginplyr.audit_sql = TRUE)
invisible(summarize_with_margins(translated, n = n(), .grouping = rollup(g)))
cat("summarize_with_margins over the translated call built - Sent queries:", paste(last_sent_queries()$purpose, collapse = ", "), "\n")
unlink(path)
cat("file absent: native mutate() builds and renders:", inherits(dbplyr::sql_render(mutate(t, y = hidden_first(v))), "sql"), "- typed inspection:", typed_plan(translated), "\n")
DBI::dbDisconnect(con, shutdown = TRUE)
