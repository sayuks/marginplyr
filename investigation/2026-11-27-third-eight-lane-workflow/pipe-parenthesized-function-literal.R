# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Second round: a parenthesised function literal on the right of %>% loses its parentheses in a summary (#1826).
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
d <- tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
show <- function(label, e) {
  r <- tryCatch(e, error = function(c) paste("ERROR:", gsub("\n", " ", conditionMessage(c))))
  cat(label, "->", if (is.data.frame(r)) paste(r$n, collapse = ",") else r, "\n")
}
cat("expected: 7 (sum of v), the value plain summarise() returns\n")
show("native summarise  ", summarise(d, n = v %>% (function(x) sum(x))))
show("native summarise .by", summarise(d, n = v %>% (function(x) sum(x)), .by = g))
show("marginplyr .grouping=NULL", summarize_with_margins(d, n = v %>% (function(x) sum(x)), .grouping = NULL))
show("marginplyr rollup(g)", summarize_with_margins(d, n = v %>% (function(x) sum(x)), .grouping = rollup(g)))
show("marginplyr \\(x) form", summarize_with_margins(d, n = v %>% (\(x) sum(x)), .grouping = NULL))
show("native (base::sum)", summarise(d, n = v %>% (base::sum)))
show("marginplyr (base::sum)", summarize_with_margins(d, n = v %>% (base::sum), .grouping = NULL))
dt <- data.table::as.data.table(d)
show("marginplyr data.table", summarize_with_margins(dt, n = v %>% (function(x) sum(x)), .grouping = NULL))
cat("controls that work: (sum) and sum():\n")
show("marginplyr (sum)", summarize_with_margins(d, n = v %>% (sum), .grouping = NULL))
show("marginplyr |> (function)()", summarize_with_margins(d, n = v |> (function(x) sum(x))(), .grouping = NULL))
cat("the rewritten expression marginplyr hands to dplyr:\n")
print(marginplyr:::rewrite_summary_selections(quote(v %>% (function(x) sum(x))), env = globalenv(), data_proxy = d, defer_local = TRUE))
