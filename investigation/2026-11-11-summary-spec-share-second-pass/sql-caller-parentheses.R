# Investigated SHA: 22043a461c5c48662ff54ea0a1238f00ac316059
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
suppressMessages({pkgload::load_all(".", quiet = TRUE); library(dplyr)})
d <- data.frame(g = c("a", "a", "b", "b"), h = c("x", "y", "x", "y"), v = c(1L, 2L, 3L, 4L), w = c(10L, 20L, 30L, 40L))
run <- function(x) summarize_with_margins(x,
  diff = sum(w, na.rm = TRUE) - (sum(v, na.rm = TRUE) - 1L),                   # a - (b - c)
  cond = sum(if_else(g == "a" & (h == "x" | v > 3L), w, 0L), na.rm = TRUE),    # p & (q | r)
  isin = sum(if_else((v + 1L) %in% c(2L, 5L), w, 0L), na.rm = TRUE),           # (a + b) %in% c
  .grouping = rollup(g), .sort = "none")
nat <- function(x, ...) summarise(x,
  diff = sum(w, na.rm = TRUE) - (sum(v, na.rm = TRUE) - 1L),
  cond = sum(if_else(g == "a" & (h == "x" | v > 3L), w, 0L), na.rm = TRUE),
  isin = sum(if_else((v + 1L) %in% c(2L, 5L), w, 0L), na.rm = TRUE), ...)
ord <- function(x) { x <- as.data.frame(collect(x)); x[order(x$g), , drop = FALSE] }
for (backend in c("sqlite", "duckdb")) {
  con <- if (backend == "sqlite") DBI::dbConnect(RSQLite::SQLite(), ":memory:") else DBI::dbConnect(duckdb::duckdb(dbdir = ":memory:", shared_home = FALSE))
  t <- copy_to(con, d, "t")
  cat("==", backend, ": native dbplyr at (g), then ()\n"); print(ord(nat(t, .by = g))); print(as.data.frame(collect(nat(t))))
  cat("==", backend, ": summarize_with_margins\n"); print(ord(run(t)))
  cat("==", backend, ": rendered, .by = g only\n")
  print(dbplyr::sql_render(summarize_with_margins(t, diff = sum(w, na.rm = TRUE) - (sum(v, na.rm = TRUE) - 1L), .by = g)))
  DBI::dbDisconnect(con, shutdown = TRUE)
}
cat("== local data frame: summarize_with_margins\n"); print(ord(run(d)))
cat("== dtplyr: summarize_with_margins\n"); print(ord(run(dtplyr::lazy_dt(d))))
