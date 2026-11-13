# Investigated SHA: 6165f4768400a28520cc8977e056da0db49a32ed
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A stored DuckDB view over read_parquet(), range() or unnest() is refused, while the same SQL written directly and a view over the same file as a replacement scan are admitted.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
parquet <- tempfile(fileext = ".parquet")
quoted <- DBI::dbQuoteString(con, parquet)
run <- function(sql) invisible(DBI::dbExecute(con, sql))
run(paste0("COPY (SELECT * FROM (VALUES ('a', 1), ('b', 2), ('a', 3)) t(g, v)) TO ", quoted, " (FORMAT PARQUET)"))
run(paste0("CREATE VIEW v_read_parquet AS SELECT * FROM read_parquet(", quoted, ")"))
run(paste0("CREATE VIEW v_parquet_scan AS SELECT * FROM parquet_scan(", quoted, ")"))
run(paste0("CREATE VIEW v_replacement AS SELECT * FROM ", quoted))
run("CREATE VIEW v_range AS SELECT CASE WHEN range < 2 THEN 'a' ELSE 'b' END AS g, range AS v FROM range(4)")
run("CREATE VIEW v_series AS SELECT 'a' AS g, generate_series AS v FROM generate_series(1, 3)")
run("CREATE VIEW v_unnest AS SELECT 'a' AS g, v FROM unnest([1, 2, 3]) AS u(v)")
run("CREATE TABLE plain AS SELECT * FROM v_replacement")
run("CREATE VIEW v_plain AS SELECT * FROM plain")
margin <- function(x) tryCatch(
  {
    out <- suppressWarnings(collect(summarize_with_margins(x, s = sum(v, na.rm = TRUE), .grouping = rollup(g), .sort = "last")))
    paste(sprintf("%s=%s", out$g, out$s), collapse = " ")
  },
  marginplyr_error = function(e) paste("Package refusal:", conditionMessage(e)[1] |> strsplit("\n") |> unlist() |> head(1))
)
native <- function(x) {
  out <- suppressWarnings(collect(arrange(summarise(x, s = sum(v, na.rm = TRUE), .by = g), g)))
  paste(sprintf("%s=%s", out$g, out$s), collapse = " ")
}
cat("expected by hand for the Parquet file: a=4 b=2 Total=6\n")
for (name in c("v_read_parquet", "v_parquet_scan", "v_replacement", "v_range", "v_series", "v_unnest", "v_plain")) {
  cat(sprintf("%-15s native: %-12s Margin: %s\n", name, native(tbl(con, name)), margin(tbl(con, name))))
}
direct <- function(sql) margin(tbl(con, dbplyr::sql(sql)))
cat("authored SELECT * FROM read_parquet(...):", direct(paste0("SELECT * FROM read_parquet(", quoted, ")")), "\n")
cat("authored SELECT ... FROM range(4)       :", direct("SELECT CASE WHEN range < 2 THEN 'a' ELSE 'b' END AS g, range AS v FROM range(4)"), "\n")
cat("authored SELECT * FROM v_read_parquet   :", direct("SELECT * FROM v_read_parquet"), "\n")
zero_row_types <- function(name) paste(vapply(collect(head(tbl(con, name), 0)), function(col) class(col)[1], ""), collapse = ",")
cat("native zero-row types of v_read_parquet :", zero_row_types("v_read_parquet"), "\n")
cat("name-only inspect_grouping(v_read_parquet):", paste(inspect_grouping(tbl(con, "v_read_parquet"), .grouping = rollup(g))$included, collapse = " "), "\n")
unlink(parquet)
DBI::dbDisconnect(con, shutdown = TRUE)
