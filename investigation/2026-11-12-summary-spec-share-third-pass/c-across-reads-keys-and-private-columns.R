# Investigated SHA: 26846d0127f333250668e928e53b200d04c99ca5
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
suppressMessages({pkgload::load_all(".", quiet = TRUE); library(dplyr)})
d <- tibble::tibble(g = c(100, 100, 200, 200, 300), v = c(1, 2, 3, 4, 10), w = c(1, 0, 2, 5, 2))
show <- function(label, expr) { cat("==", label, "\n"); print(tryCatch(as.data.frame(expr), error = function(e) paste("ERROR:", conditionMessage(e)))) }
show("native, grain (g)", summarise(group_by(d, g), u = sum(c_across(everything()))))
show("native, grain ()", summarise(select(d, -g), u = sum(c_across(everything()))))
show("margin, .by = g, no .grouping", summarize_with_margins(d, u = sum(c_across(everything())), .by = g))
show("margin, group_by(g)", summarize_with_margins(group_by(d, g), u = sum(c_across(everything()))))
show("margin, rollup(g) with a Total share", summarize_with_margins(d, u = sum(c_across(everything())), t = share_of_total(u), .grouping = rollup(g), .margin_label = NULL))
show("margin data.table, rollup(g)", summarize_with_margins(data.table::as.data.table(d), u = sum(c_across(everything())), .grouping = rollup(g), .margin_label = NULL))
show("control: pick() in the same position", summarize_with_margins(d, u = sum(pick(everything())), .grouping = rollup(g), .margin_label = NULL))
show("control: explicit columns", summarize_with_margins(d, u = sum(c_across(c(v, w))), .grouping = rollup(g), .margin_label = NULL))
dc <- mutate(d, g = as.character(g))
show("native character key, grain (g)", summarise(group_by(dc, g), u = sum(c_across(everything()))))
show("margin character key, everything()", summarize_with_margins(dc, u = sum(c_across(everything())), .grouping = rollup(g)))
show("margin character key, c_across(-g)", summarize_with_margins(dc, u = sum(c_across(-g)), .grouping = rollup(g)))
show("margin, c_across() after a share", summarize_with_margins(d, s = sum(v), t = share_of_total(s), u = length(c_across(everything())), .grouping = rollup(g), .margin_label = NULL))
