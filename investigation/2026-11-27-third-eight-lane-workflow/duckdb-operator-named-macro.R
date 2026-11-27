# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A caller macro named with an operator token is bound by the zero-row read; pass `old` as the argument to run duckdb 1.5.6 from the compatibility cache (#1813).
# Candidate 2: a caller macro named after an operator DuckDB binds from syntax
# but missing from duckdb_implicit_functions -- the json extension's `->>`
# (json_extract_string), and any generic operator such as `@@` -- is reached by
# the zero-row metadata query with no call head for the scan to see, so the
# typed plan follows a CSV file's data row.
# Run: Rscript duckdb-operator-named-macro.R          (duckdb 2.0.1, json extension loaded)
#      Rscript duckdb-operator-named-macro.R old      (duckdb 1.5.6, no json extension)
args <- commandArgs(TRUE)
old <- length(args) > 0 && args[[1]] == "old"
if (old) {
  local({ c0 <- path.expand("~/Library/Caches/org.R-project.R/R/marginplyr/duckdb-2-compatibility"); .libPaths(c(file.path(c0, "1.5.6"), file.path(c0, "shared"), .libPaths())) }) # hunt-duckdb-old
}
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
if (!old && file.exists(path.expand("~/Library/Caches/org.R-project.R/R/marginplyr/duckdb-json/v2.0.1/osx_arm64/json.duckdb_extension"))) invisible(DBI::dbExecute(con, sprintf("LOAD '%s'", path.expand("~/Library/Caches/org.R-project.R/R/marginplyr/duckdb-json/v2.0.1/osx_arm64/json.duckdb_extension"))))
cat("duckdb", as.character(packageVersion("duckdb")), "| built-in '->>' listed:",
    nrow(DBI::dbGetQuery(con, "select 1 from duckdb_functions() where function_name = '->>'")) > 0, "\n")
dir <- tempfile("cand2"); dir.create(dir); csv <- file.path(dir, "p.csv")
reader <- paste0("read_csv_auto(", DBI::dbQuoteString(con, csv), ")")
writeLines(c("k", "101"), csv)
DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = 1:2, j = c('{"a":1}', '{"a":2}')))
plan <- function(x) tryCatch(
  paste0("[", paste(inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included, collapse = " "), "]"),
  marginplyr_error = function(e) paste("Package refusal:", strsplit(conditionMessage(e), "\n")[[1]][1]),
  error = function(e) { while (!is.null(e$parent)) e <- e$parent; paste("External:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 70)) })
native <- function(x) tryCatch(class(collect(head(x, 0))$y)[1], error = function(e) paste("native error:", substr(conditionMessage(e), 1, 60)))
authored <- function(sql) function() tbl(con, dbplyr::sql(sql), vars = c("g", "v", "y"))
run <- function(op, macro_name = op) {
  q <- DBI::dbQuoteIdentifier(con, macro_name)
  DBI::dbExecute(con, paste0("CREATE MACRO ", q, "(a := NULL, b := NULL) AS (SELECT first(k) FROM ", reader, ")"))
  sql <- paste0("SELECT g, v, j ", op, " 'a' AS y FROM t")
  DBI::dbExecute(con, paste("CREATE OR REPLACE VIEW opv AS", sql))
  rows <- list(authored = authored(sql),
               `mutate(y = sql(...))` = function() mutate(select(tbl(con, "t"), g, v, j), y = sql(paste0("j ", op, " 'a'"))) |> select(g, v, y),
               `stored view` = function() tbl(con, "opv"))
  for (n in names(rows)) {
    writeLines(c("k", "101"), csv); a <- c(plan(rows[[n]]()), native(rows[[n]]()))
    writeLines(c("k", "source_tail"), csv); b <- c(plan(rows[[n]]()), native(rows[[n]]()))
    cat(sprintf("  macro %-4s %-22s plan %s -> %s | native y: %s -> %s\n", macro_name, n, a[1], b[1], a[2], b[2]))
  }
  DBI::dbExecute(con, paste("DROP VIEW opv")); DBI::dbExecute(con, paste("DROP MACRO", q))
}
cat("\nExpected (ADR 0020 #1650/#1780): 'Package refusal: ... DuckDB call' for both files, as for a\n",
    "caller macro named '+' or '~~'; observed:\n", sep = "")
run("->>"); run("@@")
# DuckDB 1.5 lexes a backtick as an operator character (#1779), so a caller
# macro named "`" is bound by `j ` 'a'` the same way.
if (old) run("`")
cat("\nControl, listed name (macro '~~', reached by LIKE):\n")
q <- DBI::dbQuoteIdentifier(con, "~~")
DBI::dbExecute(con, paste0("CREATE MACRO ", q, "(a := NULL, b := NULL) AS (SELECT first(k) FROM ", reader, ")"))
x <- tbl(con, dbplyr::sql("SELECT g, v, g LIKE 'a' AS y FROM t"), vars = c("g", "v", "y"))
writeLines(c("k", "101"), csv); cat("  ", plan(x), "\n")
DBI::dbExecute(con, paste("DROP MACRO", q))
cat("\nControl, no caller macro: plan of j ->> 'a' (built-in, if present):",
    plan(tbl(con, dbplyr::sql("SELECT g, v, j ->> 'a' AS y FROM t"), vars = c("g", "v", "y"))), "\n")
DBI::dbDisconnect(con, shutdown = TRUE); unlink(dir, recursive = TRUE)
