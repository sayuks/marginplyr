# Investigated SHA: 6165f4768400a28520cc8977e056da0db49a32ed
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# PIVOT_WIDER and PIVOT_LONGER over a CSV file pass the DuckDB CSV guard that refuses PIVOT, UNPIVOT and FROM, so the zero-row metadata query sniffs the file.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
path <- tempfile(fileext = ".csv")
quoted <- DBI::dbQuoteString(con, path)
numeric_file <- c("k,g,v", "x,a,1", "x,b,2")
text_file <- c("k,g,v", "x,a,1", "x,b,source_tail")
typed_plan <- function(x) tryCatch(
  paste(inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included, collapse = " "),
  marginplyr_error = function(e) "Package refusal",
  error = function(e) {
    while (!is.null(e$parent)) e <- e$parent
    paste("DuckDB error from the metadata query:", sub(":.*", "", gsub("\\s+", " ", conditionMessage(e))))
  }
)
both <- function(x) {
  writeLines(numeric_file, path)
  first <- typed_plan(x)
  writeLines(text_file, path)
  sprintf("numeric file: %-16s one row changed to text: %s", first, typed_plan(x))
}
wide <- function(keyword, source = quoted) dplyr::tbl(con, dbplyr::sql(sprintf(
  "%s %s ON g IN ('a', 'b') USING first(v) GROUP BY k", keyword, source
)), vars = c("k", "a", "b"))
long <- function(keyword) dplyr::tbl(con, dbplyr::sql(sprintf(
  "%s %s ON v INTO NAME name VALUE value", keyword, quoted
)), vars = c("k", "g", "name", "value"))
cat("keywords in duckdb_keywords():", paste(DBI::dbGetQuery(con, "select keyword_name from duckdb_keywords() where keyword_name like 'pivot%' or keyword_name like 'unpivot%' order by 1")[[1]], collapse = ", "), "\n")
cat("PIVOT 'f.csv'        ", both(wide("PIVOT")), "\n")
cat("PIVOT_WIDER 'f.csv'  ", both(wide("PIVOT_WIDER")), "\n")
cat("UNPIVOT 'f.csv'      ", both(long("UNPIVOT")), "\n")
cat("PIVOT_LONGER 'f.csv' ", both(long("PIVOT_LONGER")), "\n")
explicit <- sprintf("read_csv(%s, columns = {'k': 'VARCHAR', 'g': 'VARCHAR', 'v': 'VARCHAR'}, auto_detect = false, header = true)", quoted)
cat("control: PIVOT_WIDER over an explicit schema:", both(wide("PIVOT_WIDER", explicit)), "\n")
writeLines(numeric_file, path)
DBI::dbWriteTable(con, "src", utils::read.csv(path))
cat("control: PIVOT_WIDER over a table           :", typed_plan(wide("PIVOT_WIDER", "src")), "\n")
x <- wide("PIVOT_WIDER")
options(marginplyr.audit_sql = TRUE)
built <- tryCatch(
  { summarize_with_margins(x, n = dplyr::n(), .grouping = rollup(k)); "built" },
  marginplyr_error = function(e) "Package refusal"
)
cat("summarize_with_margins over PIVOT_WIDER:", built, "- Sent queries:", paste(last_sent_queries()$purpose, collapse = ", "), "\n")
unlink(path)
absent <- wide("PIVOT_WIDER")
cat("file absent: native tbl() builds:", inherits(absent, "tbl_lazy"), "- typed inspection:", typed_plan(absent), "\n")
statement <- function(keyword) dplyr::tbl(con, dbplyr::sql(sprintf(
  "SELECT column_name, column_type FROM (%s %s)", keyword, quoted
)), vars = c("column_name", "column_type"))
writeBin(as.raw(c(0x6b, 0x2c, 0x76, 0x0a, 0xff, 0xfe, 0x2c, 0x31, 0x0a)), path)
for (keyword in c("SUMMARIZE", "DESCRIBE", "SHOW")) {
  cat(sprintf("%-9s 'f.csv' holding invalid UTF-8: %s\n", keyword, typed_plan(statement(keyword))))
}
cat("FROM      'f.csv' holding invalid UTF-8:", typed_plan(dplyr::tbl(con, dbplyr::sql(sprintf("SELECT * FROM %s", quoted)), vars = c("k", "v"))), "\n")
unlink(path)
DBI::dbDisconnect(con, shutdown = TRUE)
