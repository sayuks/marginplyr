# Investigated SHA: 03e3cef502f7332b69e50608740033043d27f2b4
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A Grouping helper call used as a call head raises rlang's non-callable error; the bquote() and quote() cases are controls.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
d <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 3))
report <- function(label, expr) {
  cnd <- tryCatch(expr, error = function(e) e)
  cat("==", label, "\n")
  if (!inherits(cnd, "condition")) return(print(as.data.frame(cnd)))
  cat("class:", paste(class(cnd), collapse = "/"), "\n")
  cat("call: ", deparse(conditionCall(cnd))[[1]], "\n")
  cat("message:", conditionMessage(cnd), "\n")
}
report("margin: helper call as a call head", summarize_with_margins(d, a = grouping_bit(g)(1), .grouping = rollup(g)))
report("margin: grouping_id() as a call head", summarize_with_margins(d, a = grouping_id()(v), .grouping = rollup(g)))
report("margin control: another non-function head", summarize_with_margins(d, a = sum(v)(1), .grouping = rollup(g)))
report("native: the constant the helper stands for", summarise(d, a = (!!0L)(1), .by = g))
report("bquote unmatched", summarize_with_margins(d, a = length(base::bquote(v, zzz = 1)), .grouping = rollup(g)))
report("bquote unmatched N", summarise(d, a = length(base::bquote(v, zzz = 1)), .by = g))
report("bquote too many", summarize_with_margins(d, a = length(bquote(v, globalenv(), FALSE, 4)), .grouping = rollup(g)))
report("quote too many", summarize_with_margins(d, a = length(quote(v, w)), .grouping = rollup(g)))
report("quote too many N", summarise(d, a = length(quote(v, w)), .by = g))
