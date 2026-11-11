# Investigated SHA: 22043a461c5c48662ff54ea0a1238f00ac316059
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
suppressMessages({pkgload::load_all(".", quiet = TRUE); library(dplyr)})
d <- data.frame(g = c("a", "a", "b"), v = 1:3)
show <- function(label, expr) { cat("==", label, "\n"); print(tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e)))) }
show("native, grain (g)", summarise(d, v = sum(v), across(v, ~ .x / 2, .names = "half"), .by = g))
show("native, grain ()", summarise(d, v = sum(v), across(v, ~ .x / 2, .names = "half")))
show("margin data.frame", summarize_with_margins(d, v = sum(v), across(v, ~ .x / 2, .names = "half"), .grouping = rollup(g)))
show("margin tibble, .grouping = NULL", summarize_with_margins(tibble::as_tibble(d), v = sum(v), across(v, ~ .x / 2, .names = "half"), .by = g))
show("margin data.table", summarize_with_margins(data.table::as.data.table(d), v = sum(v), across(v, ~ .x / 2, .names = "half"), .grouping = rollup(g)))
show("margin, overwrite by unnamed across", summarize_with_margins(d, across(v, sum), across(v, ~ .x / 2, .names = "half"), .grouping = rollup(g)))
show("margin, .fn-only template over a list", summarize_with_margins(d, v = sum(v), across(v, list(h = ~ .x / 2, d = ~ .x * 2), .names = "{.fn}"), .grouping = rollup(g)))
show("control: template with {.col}", summarize_with_margins(d, v = sum(v), across(v, ~ .x / 2, .names = "{.col}_half"), .grouping = rollup(g)))
show("control: no .names needed -> named", summarize_with_margins(d, v = sum(v), half = v / 2, .grouping = rollup(g)))
show("control: earlier output does not reuse an input name", summarize_with_margins(d, s = sum(v), across(s, ~ .x / 2, .names = "half"), .grouping = rollup(g)))
show("control: across first", summarize_with_margins(d, across(v, ~ sum(.x) / 2, .names = "half"), v = sum(v), .grouping = rollup(g)))
