# Investigated SHA: 26846d0127f333250668e928e53b200d04c99ca5
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# E1: compute() of an unsorted typed-missing SQLite Margin summary (no .id, no
# share) creates an untyped destination; the dimension then collects as
# logical when empty or all-missing. Native compute and the sorted / .id
# variants keep the declared type.
suppressPackageStartupMessages({library(dplyr); library(dbplyr)})
pkgload::load_all(".", quiet = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
invisible(DBI::dbExecute(con, "CREATE TABLE tn (i INTEGER, c TEXT, x REAL)"))
invisible(DBI::dbExecute(con, "INSERT INTO tn VALUES (NULL, NULL, 30.0)"))
invisible(DBI::dbExecute(con, "CREATE TABLE t0 (i INTEGER, c TEXT, x REAL)"))
ddl <- function(nm) DBI::dbGetQuery(con, sprintf("SELECT sql FROM sqlite_temp_master WHERE name='%s'", nm))$sql
k <- 0
show <- function(tag, q, col) {
  k <<- k + 1; nm <- paste0("out", k)
  cat(sprintf("%-34s class=%-30s collect=%-9s compute->collect=%-9s %s\n", tag, class(q)[1],
              class(collect(q)[[col]])[1], class(collect(compute(q, name = nm))[[col]])[1], ddl(nm)))
}
for (tb in c("tn", "t0")) for (col in c("i", "c")) {
  src <- tbl(con, tb)
  show(paste(tb, col, "native summarise"), src |> group_by(.data[[col]]) |> summarise(s = sum(x, na.rm = TRUE)), col)
  show(paste(tb, col, "margin unsorted"), summarize_with_margins(src, s = sum(x, na.rm = TRUE), .grouping = rollup(all_of(col)), .margin_label = NULL), col)
  show(paste(tb, col, "margin unsorted one set"), summarize_with_margins(src, s = sum(x, na.rm = TRUE), .grouping = grouping_set(all_of(col)), .margin_label = NULL), col)
  show(paste(tb, col, "margin unsorted + .id"), summarize_with_margins(src, s = sum(x, na.rm = TRUE), .grouping = rollup(all_of(col)), .margin_label = NULL, .id = "gid"), col)
  show(paste(tb, col, "margin sorted"), summarize_with_margins(src, s = sum(x, na.rm = TRUE), .grouping = rollup(all_of(col)), .margin_label = NULL, .sort = "last"), col)
}
