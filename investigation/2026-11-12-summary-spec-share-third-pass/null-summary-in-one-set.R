# Investigated SHA: 26846d0127f333250668e928e53b200d04c99ca5
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A summary that is NULL in some grouping sets only reaches a bare stop() that
# speaks of "Union branch"; #1441 gave the unnamed-frame form a Package condition.
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(".", quiet = TRUE)
d <- data.frame(g = c("a", "a", "b"), v = 1:3)
e <- function(label, expr) {
  r <- tryCatch({ expr; "ok" }, error = function(e)
    paste0("<", paste(class(e), collapse = "/"), "> call=", deparse1(conditionCall(e)), "\n", conditionMessage(e)))
  cat("## ", label, "\n", r, "\n", sep = "")
}
e("named, NULL at the margin", summarize_with_margins(d, val = if (grouping_bit(g) == 1L) NULL else sum(v), .grouping = rollup(g)))
e("named, NULL at the detail", summarize_with_margins(d, n = n(), val = if (grouping_bit(g) == 0L) NULL else sum(v), .grouping = rollup(g)))
e("unnamed scalar", summarize_with_margins(d, if (grouping_bit(g) == 1L) NULL else sum(v), .grouping = rollup(g)))
e("across() function", summarize_with_margins(d, across(v, ~ if (grouping_bit(g) == 1L) NULL else sum(.x)), .grouping = rollup(g)))
e("data.table input", summarize_with_margins(data.table::as.data.table(d), val = if (grouping_bit(g) == 1L) NULL else sum(v), .grouping = rollup(g)))
e("control: unnamed frame vs NULL (#1441 form)", summarize_with_margins(d, if (grouping_bit(g) == 1L) NULL else tibble(a = sum(v)), .grouping = rollup(g)))
e("control: NULL in every set", summarize_with_margins(d, n = n(), val = NULL, .grouping = rollup(g)))
cat("## dplyr per grain, bound\n"); print(bind_rows(summarise(d, val = sum(v), .by = g), summarise(d, val = NULL)))
