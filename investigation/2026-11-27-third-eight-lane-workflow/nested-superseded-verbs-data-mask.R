# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Second round: top_n(), sample_n(), group_nest() and tidyr::uncount() nested in a summary do not own their helpers; folded into #1819.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
d <- tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
nested_g <- tibble(g = c("x", "y", "z"), aa = 1:3)
tryv <- function(expr) tryCatch(expr, error = function(e) paste("ERR:", gsub("\n", " ", substr(conditionMessage(e), 1, 90))))
run <- function(label, q) {
  mp <- tryv(suppressWarnings(summarize_with_margins(d, n = !!q, .grouping = rollup(g)))$n)
  nat <- tryv(suppressWarnings(summarise(d, n = !!q, .by = g))$n)
  alone <- tryv(eval(q))
  cat(sprintf("%-26s marginplyr=%s | native summarise=%s | nested alone=%s\n", label,
              paste(mp, collapse = ","), paste(nat, collapse = ","), paste(alone, collapse = ",")))
}
cat("registered control (filter), then verbs the registry does not list:\n")
run("filter", quote(nrow(filter(nested_g, !if_any(everything(), ~ .x == "x")))))
run("top_n", quote(nrow(top_n(nested_g, 1, if_any(everything(), ~ .x == "x")))))
run("top_frac", quote(nrow(top_frac(nested_g, 0.34, if_any(everything(), ~ .x == "x")))))
run("group_nest", quote(ncol(group_nest(nested_g, across(everything())))))
run("tidyr::uncount", quote(nrow(tidyr::uncount(nested_g, as.integer(if_any(everything(), ~ .x == "x"))))))
run("sample_n(weight=)", quote(sample_n(nested_g, 1, weight = as.numeric(if_any(everything(), ~ .x == "x")))$aa))
cat("expected for every row: top_n 1, top_frac 1, group_nest 3, uncount 1, sample_n 1 (the nested call alone)\n")
cat("same wrong values on a data.table input:\n")
dt <- data.table::as.data.table(d)
r <- summarize_with_margins(dt, n = nrow(top_n(nested_g, 1, if_any(everything(), ~ .x == "x"))), .grouping = rollup(g))
print(r$n)
