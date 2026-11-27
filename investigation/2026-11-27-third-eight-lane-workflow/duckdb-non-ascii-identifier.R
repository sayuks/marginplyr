# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Second round: the DuckDB preflight tokenizer splits an unquoted non-ASCII identifier (#1834).
# Candidate 2: duckdb_metadata_tokens() splits an unquoted non-ASCII identifier
# into ASCII and single-character tokens, while DuckDB reads it as one
# identifier. A view or macro named that way, written unquoted in authored SQL,
# is therefore never resolved against the catalog, and the zero-row metadata
# query binds it, sampling the CSV behind it.
args <- commandArgs(TRUE)
old <- length(args) > 0 && args[[1]] == "old"
if (old) {
  local({ c0 <- path.expand("~/Library/Caches/org.R-project.R/R/marginplyr/duckdb-2-compatibility"); .libPaths(c(file.path(c0, "1.5.6"), file.path(c0, "shared"), .libPaths())) }) # hunt-duckdb-old
}
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
cat("duckdb", as.character(packageVersion("duckdb")), "\n")
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
csv <- tempfile(fileext = ".csv"); qs <- DBI::dbQuoteString(con, csv)
writeLines(c("k", "101"), csv)
invisible(DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b"), v = 1:2)))
view_u <- "vüe"        # vüe
view_a <- "vue"             # ASCII control
mac_u <- "mé"          # mé
mac_a <- "me"
jp <- "売上"         # 売上
for (nm in c(view_u, view_a, jp)) invisible(DBI::dbExecute(con, paste0(
  "CREATE VIEW ", DBI::dbQuoteIdentifier(con, nm), " AS SELECT g, k AS y FROM t, read_csv_auto(", qs, ")")))
for (nm in c(mac_u, mac_a)) invisible(DBI::dbExecute(con, paste0(
  "CREATE MACRO ", DBI::dbQuoteIdentifier(con, nm), "(a) AS (SELECT first(k) FROM read_csv_auto(", qs, "))")))
invisible(DBI::dbExecute(con, paste0("CREATE MACRO ", DBI::dbQuoteIdentifier(con, "tm\u00e9"), "() AS TABLE SELECT 'a' AS g, k AS y FROM read_csv_auto(", qs, ")")))
outcome <- function(x) tryCatch(
  paste0("plan ", paste(inspect_grouping(x, .grouping = grouping_set(where(is.numeric)))$included, collapse = " ")),
  marginplyr_error = function(e) paste0("Package refusal: ", strsplit(conditionMessage(e), "\n")[[1]][1]),
  error = function(e) { while (!is.null(e$parent)) e <- e$parent; paste0("External: ", substr(gsub("\\s+", " ", conditionMessage(e)), 1, 70)) })
native <- function(x) tryCatch(class(collect(head(x, 0))$y)[1], error = function(e) paste("ERR", substr(conditionMessage(e), 1, 50)))
lz <- function(sql) tbl(con, dbplyr::sql(enc2utf8(sql)), vars = c("g", "y"))
forms <- list(
  "view, unquoted non-ASCII name (vüe)"      = lz(paste("SELECT g, y FROM", view_u)),
  "view, unquoted Japanese name (売上)"       = lz(paste("SELECT g, y FROM", jp)),
  "macro, unquoted non-ASCII head (mé(v))"   = lz(paste0("SELECT g, ", mac_u, "(v) AS y FROM t")),
  "table macro, unquoted non-ASCII (FROM tmé())" = lz("SELECT g, y FROM tm\u00e9()"),
  "sql() fragment in mutate (mé(v))"   = mutate(tbl(con, "t"), y = dbplyr::sql(enc2utf8(paste0(mac_u, "(v)")))) |> select(g, y),
  "control: same view quoted (\"vüe\")"       = lz(paste("SELECT g, y FROM", DBI::dbQuoteIdentifier(con, view_u))),
  "control: ASCII view unquoted (vue)"       = lz(paste("SELECT g, y FROM", view_a)),
  "control: ASCII macro unquoted (me(v))"    = lz(paste0("SELECT g, ", mac_a, "(v) AS y FROM t")),
  "control: dplyr tbl(con, \"vüe\")"          = tbl(con, view_u)
)
for (nm in names(forms)) {
  cat("\n==", nm, "\n")
  for (val in c("101", "source_tail")) {
    writeLines(c("k", val), csv)
    cat(sprintf("  file row %-12s native zero-row y: %-10s marginplyr typed: %s\n", val, native(forms[[nm]]), outcome(forms[[nm]])))
  }
  writeLines(c("k", "101"), csv)
}
unlink(csv)
cat("\nfile absent, unquoted view:", outcome(forms[[1]]), "\n")
cat("file absent, unquoted macro:", outcome(forms[[3]]), "\n")
writeLines(c("k", "101"), csv)
options(marginplyr.audit_sql = TRUE)
s <- tryCatch(summarize_with_margins(forms[[2]], n = n(), .grouping = rollup(where(is.character))), error = function(e) e)
cat("\nsummarize_with_margins over 売上, rollup(where(is.character)):", if (inherits(s, "error")) conditionMessage(s) else "returned", "\n")
print(as.data.frame(last_sent_queries(details = TRUE))[, c("purpose", "status")])
cat("expected: Package refusal (CSV input / DuckDB call) for the unquoted non-ASCII forms, as for their quoted and ASCII controls\n")
# Reverse direction, same tokenizer: an explicit-schema reader keyed by an unquoted
# two-character non-ASCII name is refused although its type reads no row.
key <- "\u540d\u524d"   # 名前
for (k in c(key, DBI::dbQuoteString(con, key))) {
  sql <- enc2utf8(paste0("SELECT * FROM read_csv(", qs, ", columns = {", k, ": 'BIGINT'}, auto_detect = false, header = true)"))
  r <- character()
  for (val in c("101", "202")) { writeLines(enc2native(c(key, val)), csv); r <- c(r, outcome(tbl(con, dbplyr::sql(sql), vars = key))) }
  cat(sprintf("\nexplicit schema, key %-8s typed: %s | %s\n", k, r[1], r[2]))
}
cat("expected (reverse): plan (名前) for both spellings, as for the quoted key\n")
DBI::dbDisconnect(con, shutdown = TRUE)
