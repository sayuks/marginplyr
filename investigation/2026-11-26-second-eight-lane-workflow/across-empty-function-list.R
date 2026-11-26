# Investigated SHA: 0fe5f1e40daa14e2ba2b5246452fa668bc330f80
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# An across() whose function list is an empty literal list() is predicted to produce one column per selected column.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({library(dplyr)})
d <- data.frame(g = c("a", "a", "b"), x = c(1, 2, 5), y = c(10, 20, 30))
show <- function(e) tryCatch(print(as.data.frame(e)), error = function(c)
  cat("ERROR <", paste(class(c), collapse = "/"), ">:", conditionMessage(c), "\n"))
fns <- list()   # e.g. a wrapper's function list that happens to be empty

cat("Native control: across(x, list()) produces no column\n")
print(summarise(d, across(x, list()), m = n(), .by = g))
print(summarise(d, across(x, list(!!!fns)), x_ = sum(x), .by = g))

cat("\nA. Local: share of the non-existent `x_` -- expected: pre-execution refusal 'unknown preceding ordinary summary `x_`'\n")
show(summarize_with_margins(d, across(x, list(!!!fns)), s = share_of_total(x_), .grouping = rollup(g)))
cat("   control: same shape with list(a = sum) (x_ also absent):\n")
show(summarize_with_margins(d, across(x, list(a = sum)), s = share_of_total(x_), .grouping = rollup(g)))

con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE)); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
DBI::dbWriteTable(con, "d", d); t <- tbl(con, "d")
dt <- dtplyr::lazy_dt(d)
cat("\nB. Lazy inputs, the same share of `x_` (built, fails only when collected):\n")
show(collect(summarize_with_margins(t, across(x, list()), s = share_of_total(x_), .grouping = rollup(g))))
show(collect(summarize_with_margins(dt, across(x, list()), s = share_of_total(x_), .grouping = rollup(g))))

cat("\nC. A genuine `x_` summary after across(x, list()) -- expected: x_ = 3, 5, 8; s = 0.375, 0.625, 1\n")
cat("   data frame:\n"); show(summarize_with_margins(d, across(x, list()), x_ = sum(x), s = share_of_total(x_), .grouping = rollup(g)))
cat("   DuckDB:\n");     show(collect(summarize_with_margins(t, across(x, list()), x_ = sum(x), s = share_of_total(x_), .grouping = rollup(g))))
cat("   dtplyr:\n");     show(collect(summarize_with_margins(dt, across(x, list()), x_ = sum(x), s = share_of_total(x_), .grouping = rollup(g))))
cat("   native DuckDB / dtplyr:\n")
print(collect(summarise(t, across(x, list()), x_ = sum(x), .by = g)))
print(collect(summarise(dt, across(x, list()), x_ = sum(x), .by = g)))

cat("\nD. .id = \"x_\" -- expected: accepted, no summary is named x_\n")
cat("   data frame:\n"); show(summarize_with_margins(d, across(x, list()), m = n(), .grouping = rollup(g), .id = "x_"))
cat("   DuckDB:\n");     show(collect(summarize_with_margins(t, across(x, list()), m = n(), .grouping = rollup(g), .id = "x_")))
cat("   dtplyr:\n");     show(collect(summarize_with_margins(dt, across(x, list()), m = n(), .grouping = rollup(g), .id = "x_")))
