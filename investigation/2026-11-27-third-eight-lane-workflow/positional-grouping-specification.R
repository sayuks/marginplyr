# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Rejected candidate: a Grouping specification passed by position to the other verbs gets tidyselect's `.by` error, as native `.by` does.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
d <- tibble(a = c("x", "y", "x"), v = 1:3)
show <- function(label, expr) {
  e <- tryCatch({ expr; NULL }, error = function(e) e)
  cat(sprintf("%-46s %s | %s\n", label, class(e)[1], gsub("\n", " / ", conditionMessage(e))))
}
show("summarize_with_margins(d, n = n(), rollup(a))", summarize_with_margins(d, n = n(), rollup(a)))
show("expand_with_margins(d, rollup(a))", expand_with_margins(d, rollup(a)))
show("nest_with_margins(d, rollup(a))", nest_with_margins(d, rollup(a)))
show("nest_by_with_margins(d, rollup(a))", nest_by_with_margins(d, rollup(a)))
show("inspect_grouping(d, rollup(a))", inspect_grouping(d, rollup(a)))
show("inspect_grouping(rollup(a), d)  [data not first]", inspect_grouping(rollup(a), d))
show("native: dplyr::summarise(d, n = n(), .by = rollup(a))", summarise(d, n = n(), .by = rollup(a)))
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE)); copy_to(con, d, "d")
show("duckdb: expand_with_margins(tbl, rollup(a))", expand_with_margins(tbl(con, "d"), rollup(a)))
