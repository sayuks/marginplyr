# Investigated SHA: 03e3cef502f7332b69e50608740033043d27f2b4
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# c_across() with an unmatched argument raises a bare match.call() error naming a private placeholder.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
d <- tibble::tibble(g = c("a", "a", "b"), v = c(1, 2, 3), w = c(4, 5, 6))
report <- function(label, expr) {
  cnd <- tryCatch(expr, error = function(e) e)
  cat("==", label, "\n")
  if (!inherits(cnd, "condition")) return(print(as.data.frame(cnd)))
  cat("class:", paste(class(cnd), collapse = "/"), "\n")
  cat("call: ", deparse(conditionCall(cnd))[[1]], "\n")
  cat("message:", conditionMessage(cnd), "\n")
}
# The caller passes two columns as two arguments; c_across() has one formal.
report("native dplyr", summarise(d, z = sum(c_across(v, w)), .by = g))
report("margin, rollup", summarize_with_margins(d, z = sum(c_across(v, w)), .grouping = rollup(g)))
report("margin, .by only", summarize_with_margins(d, z = sum(c_across(v, w)), .by = g))
report("margin, alias + data.table", summarise_with_margins(data.table::as.data.table(d), z = sum(c_across(cols = v, vars = w)), .by = g))
# The call is refused even where dplyr never evaluates it.
report("native, unevaluated branch", summarise(d, z = if (FALSE) sum(c_across(v, w)) else 1, .by = g))
report("margin, unevaluated branch", summarize_with_margins(d, z = if (FALSE) sum(c_across(v, w)) else 1, .by = g))
# Before #1599 (c_across unrecognized) the same call got dplyr's own error: control with a non-recognized head.
ca <- dplyr::c_across
report("margin, unrecognized head (pre-#1599 behaviour)", summarize_with_margins(d, z = sum(ca(v, w)), .by = g))
