# Investigated SHA: 26846d0127f333250668e928e53b200d04c99ca5
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Independent evidence that the query is executed, not merely recorded:
# drop the table (or delete the parquet file) and see which error arrives.
suppressPackageStartupMessages({
  pkgload::load_all(".", quiet = TRUE)
  library(dplyr)
})
say <- function(e) paste(class(e)[1], "|", substr(gsub("\n", " | ", conditionMessage(e)), 1, 110))
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
x <- copy_to(con, data.frame(g = c("a", "b", "c"), v = 1:3), "t")
DBI::dbRemoveTable(con, "t")
for (chk in c(FALSE, TRUE)) cat("sqlite check =", chk, ":", tryCatch(
  summarize_with_margins(x, s = grouping_bit(), .grouping = rollup(g), .check_margin_label = chk),
  error = say), "\n")
cat("sqlite control (.sort invalid, check TRUE):", tryCatch(
  summarize_with_margins(x, s = sum(v), .grouping = rollup(g), .check_margin_label = TRUE, .sort = "x"),
  error = say), "\n")
d <- file.path(tempdir(), "ds"); dir.create(d)
arrow::write_parquet(data.frame(g = c("a", "b"), v = 1:2), file.path(d, "p.parquet"))
ds <- arrow::open_dataset(d); unlink(d, recursive = TRUE)
for (chk in c(FALSE, TRUE)) cat("arrow  check =", chk, ":", tryCatch(
  summarize_with_margins(ds, s = grouping_bit(), .grouping = rollup(g), .check_margin_label = chk),
  error = say), "\n")
# dtplyr: the check evaluates the step; a colliding value masks the call error.
dt <- dtplyr::lazy_dt(data.table::data.table(g = c("a", "Total"), v = 1:2))
for (chk in c(FALSE, TRUE)) cat("dtplyr check =", chk, ":", tryCatch(
  summarize_with_margins(dt, s = grouping_bit(), .grouping = rollup(g), .check_margin_label = chk),
  error = say), "\n")
