# Investigated SHA: 6165f4768400a28520cc8977e056da0db49a32ed
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A SQL input whose top node is a join, with an x side ordered by a term that names no column, fails at construction.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
try_it <- function(expr) tryCatch(
  paste(nrow(suppressWarnings(collect(expr))), "rows"),
  error = function(e) paste0("<", class(e)[1], "> ", gsub("\\s+", " ", conditionMessage(e)))
)
run <- function(con, label) {
  DBI::dbWriteTable(con, "t", data.frame(g = c("a", "b", "a", "b"), h = c("x", "y", "y", "x"), v = 1:4))
  DBI::dbWriteTable(con, "u", data.frame(g = c("a", "b"), lab = c("A", "B")))
  top <- tbl(con, "t") |> arrange(dbplyr::sql("v DESC NULLS LAST")) |> head(3)
  joined <- left_join(top, tbl(con, "u"), by = "g")
  cat("==", label, "- top node:", class(joined$lazy_query)[1], "\n")
  cat("native collect                  :", try_it(joined), "\n")
  cat("native summarise by h           :", try_it(summarise(group_by(joined, h), n = n())), "\n")
  cat("native mutate(id = 1L)          :", try_it(mutate(joined, id = 1L)), "\n")
  cat("summarize rollup(h)             :", try_it(summarize_with_margins(joined, n = n(), .grouping = rollup(h))), "\n")
  cat("expand rollup(g)                :", try_it(expand_with_margins(joined, .grouping = rollup(g))), "\n")
  cat("expand rollup(g), .id           :", try_it(expand_with_margins(joined, .grouping = rollup(g), .id = "id")), "\n")
  cat("inspect rollup(h)               :", tryCatch(paste(nrow(inspect_grouping(joined, .grouping = rollup(h))), "sets"), error = function(e) class(e)[1]), "\n")
  cat("semi_join, summarize            :", try_it(summarize_with_margins(semi_join(top, tbl(con, "u"), by = "g"), n = n(), .grouping = rollup(h))), "\n")
  cat("join |> filter(), summarize     :", try_it(summarize_with_margins(filter(joined, v > 0), n = n(), .grouping = rollup(h))), "\n")
  cat("arrange(1L) |> head, join       :", try_it(summarize_with_margins(left_join(head(arrange(tbl(con, "t"), 1L), 3), tbl(con, "u"), by = "g"), n = n(), .grouping = rollup(h))), "\n")
  cat("control: no join                :", try_it(summarize_with_margins(top, n = n(), .grouping = rollup(h))), "\n")
  cat("control: join |> mutate(w = 1L) :", try_it(summarize_with_margins(mutate(joined, w = 1L), n = n(), .grouping = rollup(h))), "\n")
  cat("control: desc(v) before join    :", try_it(summarize_with_margins(left_join(head(arrange(tbl(con, "t"), desc(v)), 3), tbl(con, "u"), by = "g"), n = n(), .grouping = rollup(h))), "\n")
  cat("control: ordering on the y side :", try_it(summarize_with_margins(left_join(tbl(con, "u"), top, by = "g"), n = n(), .grouping = rollup(h))), "\n")
  cat("upstream: native relocate(h)    :", try_it(relocate(joined, h)), "\n")
  e <- tryCatch(summarize_with_margins(joined, n = n(), .grouping = rollup(h)), error = function(e) e)
  cat("is Package condition:", inherits(e, "marginplyr_error"), "\n\n")
  DBI::dbDisconnect(con)
}
sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
run(sqlite, "SQLite")
duck <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), dbdir = ":memory:")
run(duck, "DuckDB")
