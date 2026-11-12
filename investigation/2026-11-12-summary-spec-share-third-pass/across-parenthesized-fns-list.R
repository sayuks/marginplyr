# Investigated SHA: 26846d0127f333250668e928e53b200d04c99ca5
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# An across() whose `.fns` is a parenthesized list(): since #1582 the rebuilt
# call keeps the pair, so DuckDB ignores `.names` and dtplyr and Arrow refuse a
# call that worked at 22043a46. MP=<path> selects another checkout copy.
suppressPackageStartupMessages({library(dplyr); library(dbplyr)})
pkgload::load_all(Sys.getenv("MP", "."), quiet = TRUE)
d <- data.frame(g = c("a", "a", "b"), v = 1:3, w = c(10L, 20L, 30L))
sq <- DBI::dbConnect(RSQLite::SQLite(), ":memory:"); DBI::dbWriteTable(sq, "d", d)
dk <- DBI::dbConnect(duckdb::duckdb()); DBI::dbWriteTable(dk, "d", d)
inputs <- list(data.frame = d, SQLite = tbl(sq, "d"), DuckDB = tbl(dk, "d"),
               dtplyr = dtplyr::lazy_dt(data.table::as.data.table(d)),
               Arrow = arrow::arrow_table(d))
show <- function(x) tryCatch({
  x <- as.data.frame(suppressWarnings(collect(x))); x <- x[order(x$g), , drop = FALSE]
  paste0(paste(names(x), collapse = ", "), "  |  ", paste(do.call(paste, unname(x)), collapse = " ; "))
}, error = function(e) paste("ERROR:", gsub("\\s+", " ", substr(conditionMessage(e), 1, 110))))
for (b in names(inputs)) {
  x <- inputs[[b]]
  cat(sprintf("%-10s paren list : %s\n", b, show(summarize_with_margins(
    x, across(c(v, w), (list(a = ~ sum(.x, na.rm = TRUE))), .names = "{.fn}_{.col}"), .grouping = rollup(g)))))
  cat(sprintf("%-10s bare list  : %s\n", b, show(summarize_with_margins(
    x, across(c(v, w), list(a = ~ sum(.x, na.rm = TRUE)), .names = "{.fn}_{.col}"), .grouping = rollup(g)))))
  cat(sprintf("%-10s paren fn   : %s\n", b, show(summarize_with_margins(
    x, across(c(v, w), (function(z) sum(z, na.rm = TRUE)), .names = "s_{.col}"), .grouping = rollup(g)))))
}
cat("dplyr on the data frame at (g):",
    paste(names(summarise(d, across(c(v, w), (list(a = ~ sum(.x, na.rm = TRUE))), .names = "{.fn}_{.col}"), .by = g)), collapse = ", "), "\n")
