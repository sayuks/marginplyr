# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A lazy SQL expansion drops the input's window frame and keeps its window order (#1815).
# Candidate 1: expand_with_margins() on DuckDB (and the generic SQL path) drops
# the input's dbplyr window frame while keeping its window order, so a later
# framed window expression over the copies silently spans the whole partition.
# Native control: the mutate() + union_all() composition ?expand_with_margins
# says the expansion is, built on the same connection.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
d <- data.frame(a = c("x", "y", "x", "y", "x", "y"), v = c(1, 2, 3, 4, 5, 6))
fr <- function(q) paste0("frame=[", paste(unlist(q$lazy_query$frame), collapse = ","), "] order=",
                         paste(vapply(dbplyr::op_sort(q), rlang::expr_deparse, ""), collapse = ","))
show <- function(r) paste(paste0(r$a, ":", r$v, "->", r$r), collapse = "  ")
later <- function(q) collect(mutate(q, r = sum(v, na.rm = TRUE), .by = c(set, a))) |> arrange(set, a, v)
for (eng in c("duckdb", "RSQLite")) {
  con <- if (eng == "duckdb") DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), ":memory:") else DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "df", d)
  input <- dbplyr::window_frame(dbplyr::window_order(tbl(con, "df"), v), -1, 0)
  mp <- expand_with_margins(input, .grouping = rollup(a), .id = "set")
  # ?expand_with_margins: "what you would get by applying dplyr::mutate() to
  # set each omitted dimension to the label, once per grouping set, and
  # combining the copies with dplyr::union_all()".
  nat <- union_all(mutate(input, set = 1L), mutate(input, a = "Total", set = 2L))
  cat("==", eng, as.character(packageVersion(eng)), "\n")
  cat("  marginplyr:", fr(mp), "\n  native    :", fr(nat), "\n")
  cat("  later framed sum, marginplyr:", show(later(mp)), "\n")
  cat("  later framed sum, native    :", show(later(nat)), "\n")
  if (eng == "duckdb") DBI::dbDisconnect(con, shutdown = TRUE) else DBI::dbDisconnect(con)
}
cat("expected (frame -1..0 ordered by v, per (set, a)): x:1->1 x:3->4 x:5->8 y:2->2 y:4->6 y:6->10 Total:1->1 Total:2->3 Total:3->5 Total:4->7 Total:5->9 Total:6->11\n")
