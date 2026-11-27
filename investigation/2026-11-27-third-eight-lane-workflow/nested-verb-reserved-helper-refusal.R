# Investigated SHA: 56c7679f35b266cb4c598afffa4fd63ed84d3cba
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# Second round: cur_group_id() inside an unregistered nested verb is refused as reserved; folded into #1819.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
d <- tibble(g = c("a", "a", "b"), v = c(1, 2, 4))
nested_g <- tibble(g = c("x", "y", "z"), aa = 1:3)
tryv <- function(expr) tryCatch(expr, error = function(e) paste("ERR:", gsub("\n", " ", substr(conditionMessage(e), 1, 110))))
run <- function(label, q) {
  mp <- tryv(suppressWarnings(summarize_with_margins(d, n = !!q, .grouping = rollup(g)))$n)
  nat <- tryv(suppressWarnings(summarise(d, n = !!q, .by = g))$n)
  alone <- tryv(eval(q))
  cat(sprintf("%-24s marginplyr=%s | native summarise=%s | nested alone=%s\n", label,
              paste(mp, collapse = ","), paste(nat, collapse = ","), paste(alone, collapse = ",")))
}
cat("(a) cur_group_id() inside a nested verb that sets up its own mask; control filter() is accepted\n")
run("filter (registered)", quote(nrow(filter(nested_g, aa >= cur_group_id()))))
run("with_groups", quote(with_groups(nested_g, "g", mutate, k = cur_group_id())$k[1]))
run("mutate_at", quote(mutate_at(nested_g, "aa", ~ . + cur_group_id())$aa[1]))
run("top_n", quote(nrow(top_n(nested_g, 1, -cur_group_id()))))
run("tidyr::uncount", quote(nrow(tidyr::uncount(nested_g, cur_group_id() * 2L))))
cat("\n(b) a share source reading a nested frame's own column named like an earlier summary\n")
nested_t <- tibble(total = c(1, 5, 3))
share <- function(label, q) {
  r <- tryv(summarize_with_margins(d, total = sum(v), x = !!q, p = share_of_total(x), .grouping = rollup(g)))
  cat(sprintf("%-24s %s\n", label, if (is.data.frame(r)) paste(r$x, collapse = ",") else r))
}
share("filter (registered)", quote(sum(filter(nested_t, total > 2)$total)))
share("top_n", quote(sum(top_n(nested_t, 2, total)$total)))
share("tidyr::uncount", quote(nrow(tidyr::uncount(nested_t, total))))
cat("native summarise of the top_n call: ")
print(summarise(d, total = sum(v), x = sum(top_n(nested_t, 2, total)$total), .by = g)$x)
