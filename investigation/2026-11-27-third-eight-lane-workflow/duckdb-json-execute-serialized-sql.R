# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# json_execute_serialized_sql() passes the DuckDB preflight and the zero-row read follows a data row (#1812).
# Candidate 1: the json extension's json_execute_serialized_sql() binds SQL text
# given as a value, like query(), and passes the DuckDB preflight, so the
# zero-row metadata read samples a CSV/JSON source and the typed plan follows
# the file's data row.  DuckDB 2.0.1 with the json extension loaded.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
if (!file.exists(path.expand("~/Library/Caches/org.R-project.R/R/marginplyr/duckdb-json/v2.0.1/osx_arm64/json.duckdb_extension"))) {
  cat("The DuckDB json extension binary for duckdb 2.0.1 is not in the marginplyr cache on this machine; nothing was run.\n")
  quit(status = 0)
}
suppressMessages(library(dplyr))
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
invisible(DBI::dbExecute(con, sprintf("LOAD '%s'", path.expand("~/Library/Caches/org.R-project.R/R/marginplyr/duckdb-json/v2.0.1/osx_arm64/json.duckdb_extension"))))
cat("duckdb", as.character(packageVersion("duckdb")), "\n")
dir <- tempfile("cand1"); dir.create(dir)
csv <- file.path(dir, "p.csv")
qcsv <- as.character(DBI::dbQuoteString(con, csv))
DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = 1:2))
plan <- function(x) tryCatch(
  paste0("[", paste(inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included, collapse = " "), "]"),
  marginplyr_error = function(e) paste("Package refusal:", strsplit(conditionMessage(e), "\n")[[1]][1]),
  error = function(e) { while (!is.null(e$parent)) e <- e$parent; paste("External:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 70)) })
native <- function(x) tryCatch(paste(vapply(collect(head(x, 0)), function(c) class(c)[1], ""), collapse = ","),
  error = function(e) paste("native error:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 60)))
src <- function(sql, vars = "k") function() tbl(con, dbplyr::sql(sql), vars = vars)
serialized <- function(inner) paste0(
  "SELECT * FROM json_execute_serialized_sql(json_serialize_sql(",
  DBI::dbQuoteString(con, inner), "))")
inner <- paste0("SELECT * FROM read_csv(", qcsv, ")")
rows <- list(
  "json_execute_serialized_sql(json_serialize_sql('... read_csv(p.csv)'))" = src(serialized(inner)),
  "json_execute_serialized_sql(json_serialize_sql('... FROM ''p.csv'''))" = src(serialized(paste0("SELECT * FROM ", qcsv))),
  "stored view over json_execute_serialized_sql(...)" = function() {
    DBI::dbExecute(con, paste("CREATE OR REPLACE VIEW sv AS", serialized(inner))); tbl(con, "sv") },
  "control: read_csv(p.csv) written directly" = src(inner),
  "control: query('... read_csv(p.csv)')" = src(paste0("SELECT * FROM query(", DBI::dbQuoteString(con, inner), ")")),
  "control: read_csv explicit schema, auto_detect=false" = src(paste0("SELECT * FROM read_csv(", qcsv, ", auto_detect=false, header=true, columns={'k':'BIGINT'})"))
)
cat("\nExpected (ADR 0020 #1506/#1524): 'Package refusal' for both file contents in the first three rows,\n",
    "as for read_csv() and query(); the explicit-schema control gives one plan for both files.\n\n", sep = "")
for (n in names(rows)) {
  writeLines(c("k", "101"), csv); a <- c(plan(rows[[n]]()), native(rows[[n]]()))
  writeLines(c("k", "source_tail"), csv); b <- c(plan(rows[[n]]()), native(rows[[n]]()))
  cat(sprintf("%s\n  plan:   row 101 -> %s | row source_tail -> %s\n  native zero-row types: %s -> %s\n", n, a[1], b[1], a[2], b[2]))
}
cat("\nA Margin verb over the serialized reader, audit on:\n")
writeLines(c("g,k", "a,101"), csv)
options(marginplyr.audit_sql = TRUE)
x <- tbl(con, dbplyr::sql(serialized(paste0("SELECT * FROM read_csv(", qcsv, ")"))), vars = c("g", "k"))
res <- tryCatch(summarize_with_margins(x, n = n(), .grouping = rollup(where(is.character))), error = function(e) e)
print(if (inherits(res, "error")) conditionMessage(res) else as.data.frame(last_sent_queries(details = TRUE))[, c("purpose", "status")])
unlink(csv)
cat("\nAbsent file: native lazy table builds (", class(rows[[1]]())[1], "); typed inspection reaches: ", plan(rows[[1]]()), "\n", sep = "")
DBI::dbDisconnect(con, shutdown = TRUE); unlink(dir, recursive = TRUE)
