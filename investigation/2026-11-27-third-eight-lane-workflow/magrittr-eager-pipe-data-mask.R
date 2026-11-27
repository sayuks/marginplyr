# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Second round: a verb on the right of magrittr's eager pipe gets no data-mask ownership (#1827).
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages({library(dplyr); library(magrittr)})
d <- tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
nested_g <- tibble(g = c("x", "y", "z"), aa = 1:3)
cat("expected 2 in every row (plain summarise() and the nested call alone)\n")
cat("native %!>%     :", summarise(d, n = nrow(nested_g %!>% filter(!if_any(everything(), ~ .x == "x"))), .by = g)$n, "\n")
cat("alone  %!>%     :", nrow(nested_g %!>% filter(!if_any(everything(), ~ .x == "x"))), "\n")
cat("marginplyr %>%  :", summarize_with_margins(d, n = nrow(nested_g %>% filter(!if_any(everything(), ~ .x == "x"))), .grouping = rollup(g))$n, "\n")
cat("marginplyr %!>% :", summarize_with_margins(d, n = nrow(nested_g %!>% filter(!if_any(everything(), ~ .x == "x"))), .grouping = rollup(g))$n, "\n")
