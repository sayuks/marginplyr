# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A bare reference to an earlier summary output is refused as a captured Grouping specification when the caller's environment binds that name to one.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressPackageStartupMessages({library(dplyr)})
msg <- function(expr) tryCatch(expr, error = function(e) paste0("<", class(e)[1], "> ", gsub("\\s+", " ", conditionMessage(e))))
d <- data.frame(g = c("a", "a", "b"), v = c(1, 2, 3))

report <- function(data) {
  grouping <- rollup(g)                 # the caller's specification, held in a local
  summarize_with_margins(data, grouping = n(), again = grouping, .grouping = grouping)
}
cat("expected (dplyr's data mask: `again` reads the earlier summary `grouping`):\n")
cat("  g = a, b, Total; grouping = 2, 1, 3; again = 2, 1, 3\n")
cat("observed, local data frame:\n"); print(msg(report(d)))
cat("observed, dtplyr:\n"); print(msg(collect(report(dtplyr::lazy_dt(d)))))
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
DBI::dbWriteTable(con, "d", d)
cat("observed, DuckDB:\n"); print(msg(collect(report(tbl(con, "d")))))

cat("\nnative control, plain dplyr with the same bindings:\n")
print((function(data) { grouping <- rollup(g); summarise(data, grouping = n(), again = grouping, .by = g) })(d))
cat("native control, dbplyr on the same DuckDB connection (dbplyr itself refuses; not counted):\n")
print(msg((function(data) { grouping <- rollup(g); collect(summarise(data, grouping = n(), again = grouping, .by = g)) })(tbl(con, "d"))))
cat("native control, dtplyr:\n")
print(msg((function(data) { grouping <- rollup(g); collect(summarise(data, grouping = n(), again = grouping, .by = g)) })(dtplyr::lazy_dt(d))))
cat("native control, data.table input to plain dplyr:\n")
print((function(data) { grouping <- rollup(g); summarise(data, grouping = n(), again = grouping, .by = g) })(data.table::as.data.table(d)))
cat("observed, data.table input:\n"); print(msg(report(data.table::as.data.table(d))))
cat("marginplyr control, the earlier output under a name bound to no specification:\n")
print(summarize_with_margins(d, cnt = n(), again = cnt, .grouping = rollup(g)))
cat("marginplyr control, an input column named like the bound specification (tested, computed):\n")
grouping <- rollup(g)
print(summarize_with_margins(transform(d, grouping = v * 10), t = max(grouping), again = grouping, .grouping = rollup(g)) |> msg())
DBI::dbDisconnect(con, shutdown = TRUE)
invisible(0)
