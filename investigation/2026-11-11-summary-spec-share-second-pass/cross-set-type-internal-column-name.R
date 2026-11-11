# Investigated SHA: 22043a461c5c48662ff54ea0a1238f00ac316059
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
suppressMessages({pkgload::load_all(".", quiet = TRUE); library(dplyr)})
d <- data.frame(g = c("a", "a", "b"), v = 1:3)
msg <- function(expr) cat(tryCatch({expr; "no error"}, error = function(e) conditionMessage(e)), "\n")
cat("== native union of the two grains\n")
msg(union_all(summarise(d, x = if (n() > 2) "a" else 1L, .by = g), mutate(summarise(d, x = if (n() > 2) "a" else 1L), g = "Total")))
msg(bind_rows(summarise(d, x = if (n() > 2) "a" else 1L, .by = g), summarise(d, x = if (n() > 2) "a" else 1L)))
cat("== summarize_with_margins, data.frame / tibble / data.table\n")
msg(summarize_with_margins(d, x = if (n() > 2) "a" else 1L, .grouping = rollup(g)))
msg(summarize_with_margins(tibble::as_tibble(d), s = sum(v), x = if (n() > 2) "a" else 1L, .grouping = rollup(g)))
msg(summarize_with_margins(data.table::as.data.table(d), x = if (n() > 2) "a" else 1L, .grouping = rollup(g)))
