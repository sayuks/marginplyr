# Investigated SHA: 6165f4768400a28520cc8977e056da0db49a32ed
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Second-round spellings that pass the DuckDB preflight although the zero-row metadata query makes DuckDB infer a type from file contents or a directory name.
# Each line gives the typed plan for a numeric data row, then for the same row changed to text; a control follows where one applies.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
dir <- tempfile("preflight")
dir.create(dir)
path <- file.path(dir, "p.csv")
qs <- DBI::dbQuoteString(con, path)
qi <- DBI::dbQuoteIdentifier(con, path)
typed_plan <- function(x) tryCatch(
  paste(inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included, collapse = " "),
  marginplyr_error = function(e) "Package refusal",
  error = function(e) {
    while (!is.null(e$parent)) e <- e$parent
    paste("DuckDB error:", sub(":.*", "", gsub("\\s+", " ", conditionMessage(e))))
  }
)
both <- function(x) {
  writeLines(c("k", "101"), path)
  first <- typed_plan(x)
  writeLines(c("k", "source_tail"), path)
  sprintf("%-16s | %s", first, typed_plan(x))
}
authored <- function(sql, vars = "k") tbl(con, dbplyr::sql(sql), vars = vars)
show <- function(label, x) cat(sprintf("%-62s %s\n", label, both(x)))

cat("-- reader arguments\n")
show("read_csv(auto_detect = false, union_by_name = true)", authored(paste0("SELECT * FROM read_csv([", qs, "], auto_detect = false, union_by_name = true)")))
show("  control: + header = true, columns = {'k': 'BIGINT'}", authored(paste0("SELECT * FROM read_csv([", qs, "], auto_detect = false, union_by_name = true, header = true, columns = {'k': 'BIGINT'})")))
show("  context: union_by_name = true alone", authored(paste0("SELECT * FROM read_csv([", qs, "], union_by_name = true)")))
show("read_csv(auto_detect = false, auto_detect = true)", authored(paste0("SELECT * FROM read_csv(", qs, ", auto_detect = false, auto_detect = true)")))
show("  control: auto_detect = true, auto_detect = false, columns", authored(paste0("SELECT * FROM read_csv(", qs, ", auto_detect = true, auto_detect = false, header = true, columns = {'k': 'VARCHAR'})")))

cat("-- relation positions and CTE scope\n")
show("SELECT ... UNION ALL BY NAME TABLE \"p.csv\"", authored(paste0("SELECT 'x' AS g WHERE false UNION ALL BY NAME TABLE ", qi), vars = c("g", "k")))
show("  context: SELECT ... UNION ALL TABLE \"p.csv\"", authored(paste0("SELECT 101 AS k WHERE false UNION ALL TABLE ", qi)))
invisible(DBI::dbExecute(con, "CREATE TABLE safe_t AS SELECT 'x' AS g"))
show("SELECT * FROM (FROM safe_t UNION ALL BY NAME FROM 'p.csv')", authored(paste0("SELECT * FROM (FROM safe_t UNION ALL BY NAME FROM ", qs, ") AS u"), vars = c("g", "k")))
split <- paste0(DBI::dbQuoteString(con, sub("sv$", "", path)), "\n'sv'")
show("FROM 'p.c' <newline> 'sv' (adjacent literals)", authored(paste0("SELECT * FROM ", split)))
show("  context: FROM 'p.csv'", authored(paste0("SELECT * FROM ", qs)))
show("WITH RECURSIVE \"p.csv\" AS (SELECT * FROM \"p.csv\") ...", authored(paste0("WITH RECURSIVE ", qi, " AS (SELECT * FROM ", qi, ") SELECT * FROM ", qi)))
show("  context: the same CTE without RECURSIVE", authored(paste0("WITH ", qi, " AS (SELECT * FROM ", qi, ") SELECT * FROM ", qi)))
show("WITH RECURSIVE r(z) USING KEY (z) AS (...) TABLE \"p.csv\"", authored(paste0("WITH RECURSIVE r(z) USING KEY (z) AS (SELECT 1 AS z UNION SELECT z + 1 FROM r WHERE z < 1) TABLE ", qi)))
show("  context: the same CTE without USING KEY", authored(paste0("WITH RECURSIVE r(z) AS (SELECT 1 AS z UNION SELECT z + 1 FROM r WHERE z < 1) TABLE ", qi)))

cat("-- call heads and verbatim SQL carriers\n")
DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = 1:2))
writeLines(c("k", "101"), path)
invisible(DBI::dbExecute(con, paste0("CREATE MACRO hidden_first(x) AS (SELECT first(k) FROM read_csv_auto(", qs, "))")))
t <- tbl(con, "t")
show("authored SELECT g, v, v.hidden_first() AS y FROM t", authored("SELECT g, v, v.hidden_first() AS y FROM t", vars = c("g", "v", "y")))
show("  context: authored hidden_first(v)", authored("SELECT g, v, hidden_first(v) AS y FROM t", vars = c("g", "v", "y")))
fragment <- paste0("(SELECT max(k) FROM ", qs, ")")
show("mutate(y = !!DBI::SQL('(SELECT max(k) FROM <csv>)'))", mutate(t, y = !!DBI::SQL(fragment)))
show("mutate(y = !!dbplyr::ident_q('(SELECT max(k) FROM <csv>)'))", mutate(t, y = !!dbplyr::ident_q(fragment)))
show("  context: mutate(y = !!dbplyr::sql(<the same>))", mutate(t, y = !!dbplyr::sql(fragment)))

cat("-- hive partition names (the plan follows the directory name, not a data row)\n")
hive_plan <- function(partition, extra = "") {
  root <- file.path(dir, paste0("hive_", partition))
  dir.create(file.path(root, paste0("yr=", partition)), recursive = TRUE)
  writeLines(c("k", "1"), file.path(root, paste0("yr=", partition), "f.csv"))
  typed_plan(authored(paste0(
    "SELECT * FROM read_csv(", DBI::dbQuoteString(con, file.path(root, "*", "*.csv")),
    ", auto_detect = false, header = true, columns = {'k': 'VARCHAR'}", extra, ")"
  ), vars = c("k", "yr")))
}
cat(sprintf("%-62s %-16s | %s\n", "read_csv(<glob>, auto_detect = false, columns): yr=2024 | yr=abc", hive_plan("2024"), hive_plan("abc")))
cat(sprintf("%-62s %-16s | %s\n", "  control: + hive_types = {'yr': 'VARCHAR'}", hive_plan("2025", ", hive_types = {'yr': 'VARCHAR'}"), hive_plan("abd", ", hive_types = {'yr': 'VARCHAR'}")))
unlink(dir, recursive = TRUE)
DBI::dbDisconnect(con, shutdown = TRUE)
