# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A .geojson replacement scan passes the DuckDB preflight's JSON suffix rule (#1814).
# Candidate 3: on DuckDB 2.0.1 with the json extension, a filename relation
# ending in .geojson or .geojsonl (also .gz, .zst, '?') is a JSON replacement
# scan that infers types from rows, but the preflight's suffix list knows only
# .json/.jsonl/.ndjson, so typed metadata follows the file's data row.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
if (!file.exists(path.expand("~/Library/Caches/org.R-project.R/R/marginplyr/duckdb-json/v2.0.1/osx_arm64/json.duckdb_extension"))) {
  cat("The DuckDB json extension binary for duckdb 2.0.1 is not in the marginplyr cache on this machine; nothing was run.\n")
  quit(status = 0)
}
suppressMessages(library(dplyr))
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
invisible(DBI::dbExecute(con, sprintf("LOAD '%s'", path.expand("~/Library/Caches/org.R-project.R/R/marginplyr/duckdb-json/v2.0.1/osx_arm64/json.duckdb_extension"))))
cat("duckdb", as.character(packageVersion("duckdb")), "\n")
dir <- tempfile("cand3"); dir.create(dir)
plan <- function(x) tryCatch(
  paste0("[", paste(inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included, collapse = " "), "]"),
  marginplyr_error = function(e) paste("Package refusal:", strsplit(conditionMessage(e), "\n")[[1]][1]),
  error = function(e) { while (!is.null(e$parent)) e <- e$parent; paste("External:", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 70)) })
native <- function(x) tryCatch(class(collect(head(x, 0))$k)[1], error = function(e) paste("native error:", substr(conditionMessage(e), 1, 50)))
geo <- file.path(dir, "p.geojson"); geol <- file.path(dir, "p.geojsonl"); js <- file.path(dir, "p.json")
cat("EXPLAIN of FROM 'p.geojson' names the operator:\n")
writeLines('{"k":101}', geo)
ex <- DBI::dbGetQuery(con, paste0("EXPLAIN SELECT * FROM '", geo, "'"))
cat(" ", grep("READ_JSON|JSON", unlist(strsplit(ex[[2]], "\n")), value = TRUE)[1], "\n\n")
rows <- list(
  "SELECT * FROM 'p.geojson'" = function() tbl(con, dbplyr::sql(paste0("SELECT * FROM ", DBI::dbQuoteString(con, geo))), vars = "k"),
  "SELECT * FROM \"p.geojson\"" = function() tbl(con, dbplyr::sql(paste0("SELECT * FROM ", DBI::dbQuoteIdentifier(con, geo))), vars = "k"),
  "TABLE \"p.geojsonl\"" = function() tbl(con, dbplyr::sql(paste0("TABLE ", DBI::dbQuoteIdentifier(con, geol))), vars = "k"),
  "stored view over 'p.geojson'" = function() { DBI::dbExecute(con, paste0("CREATE OR REPLACE VIEW gv AS SELECT * FROM ", DBI::dbQuoteString(con, geo))); tbl(con, "gv") },
  "control: SELECT * FROM 'p.json'" = function() tbl(con, dbplyr::sql(paste0("SELECT * FROM ", DBI::dbQuoteString(con, js))), vars = "k"),
  "control: read_json('p.geojson', explicit schema)" = function() tbl(con, dbplyr::sql(paste0("SELECT * FROM read_json(", DBI::dbQuoteString(con, geo), ", columns = {'k': 'BIGINT'}, records = true, auto_detect = false)")), vars = "k")
)
cat("Expected (ADR 0020 #1516): 'Package refusal' (JSON input) for both contents in the first four rows,\n",
    "as for the .json control; the explicit-schema reader gives one plan.\n\n", sep = "")
for (n in names(rows)) {
  for (f in c(geo, geol, js)) writeLines('{"k":101}', f); a <- c(plan(rows[[n]]()), native(rows[[n]]()))
  for (f in c(geo, geol, js)) writeLines('{"k":"source_tail"}', f); b <- c(plan(rows[[n]]()), native(rows[[n]]()))
  cat(sprintf("%-50s plan %s -> %s | native k: %s -> %s\n", n, a[1], b[1], a[2], b[2]))
}
cat("\nsummarize_with_margins() with a typed selection, audit on:\n")
writeLines('{"g":"a","k":101}', geo)
options(marginplyr.audit_sql = TRUE)
x <- tbl(con, dbplyr::sql(paste0("SELECT * FROM ", DBI::dbQuoteString(con, geo))), vars = c("g", "k"))
invisible(summarize_with_margins(x, n = n(), .grouping = rollup(where(is.character))))
print(as.data.frame(last_sent_queries(details = TRUE))[, c("purpose", "status")])
unlink(geo)
cat("\nAbsent file: native lazy table builds (", class(rows[[1]]())[1], "); typed inspection reaches: ", plan(rows[[1]]()), "\n", sep = "")
DBI::dbDisconnect(con, shutdown = TRUE); unlink(dir, recursive = TRUE)
