# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A share summary drops the input's window frame; refuted as its own defect and folded into #1815's decision.
# Candidate 3: a share summary (share_of_total()/share_of_parent()) drops the
# input's dbplyr window frame on every SQL backend while the share-less summary
# and native summarise() + mutate() keep it; a later framed window expression
# then spans the whole partition.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
d <- data.frame(a = c("x", "y", "x", "y", "z", "z"), v = c(1, 2, 3, 4, 5, 6))
fr <- function(q) paste0("frame=[", paste(unlist(q$lazy_query$frame), collapse = ","), "] order=",
                         paste(vapply(dbplyr::op_sort(q), rlang::expr_deparse, ""), collapse = ","))
rr <- function(q) { r <- collect(mutate(q, r = sum(n, na.rm = TRUE))); r <- r[order(r$a), ]; paste(paste0(r$a, ":", r$n, "->", r$r), collapse = "  ") }
for (eng in c("duckdb", "RSQLite")) {
  con <- if (eng == "duckdb") DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), ":memory:") else DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "df", d)
  inp <- dbplyr::window_frame(dbplyr::window_order(tbl(con, "df"), a), -1, 0)
  chk <- eng == "duckdb"
  q0 <- summarize_with_margins(inp, n = sum(v, na.rm = TRUE), .grouping = rollup(a))
  q1 <- summarize_with_margins(inp, n = sum(v, na.rm = TRUE), p = share_of_total(n), .grouping = rollup(a), .check_share_source = chk)
  q2 <- summarize_with_margins(inp, n = sum(v, na.rm = TRUE), p = share_of_parent(n), .grouping = rollup(a), .check_share_source = chk)
  natv <- inp |> group_by(a) |> summarise(n = sum(v, na.rm = TRUE)) |> ungroup() |> mutate(p = n / sum(n, na.rm = TRUE))
  cat("==", eng, as.character(packageVersion(eng)), "\n")
  cat("  no share          :", fr(q0), "\n  share_of_total()  :", fr(q1), "\n  share_of_parent() :", fr(q2), "\n  native summarise+mutate:", fr(natv), "\n")
  cat("  later sum(n), no share        :", rr(q0), "\n")
  cat("  later sum(n), share_of_total():", rr(q1), "\n")
  cat("  later sum(n), share_of_parent():", rr(q2), "\n")
  cat("  later sum(n), native          :", rr(natv), "\n")
  if (eng == "duckdb") DBI::dbDisconnect(con, shutdown = TRUE) else DBI::dbDisconnect(con)
}
cat("expected: the share result's rows (Total 21, x 4, y 6, z 11) with the no-share result's framed values, rows ordered by a: Total:21->21 x:4->25 y:6->10 z:11->17\n")
