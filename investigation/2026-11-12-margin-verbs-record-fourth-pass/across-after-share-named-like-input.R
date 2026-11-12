# Investigated SHA: 03e3cef502f7332b69e50608740033043d27f2b4
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# An unnamed across() after a share named like an input column is refused over a generated all_of().
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
d <- data.frame(a = c("a1", "a1", "a2", "a2", "a3"), v = c(1L, 2L, 3L, 4L, 6L), w = c(10, 20, 30, 40, 50))
show <- function(expr) {
  r <- tryCatch(expr, error = function(e) e)
  if (inherits(r, "error")) cat("<", class(r)[1], ">\n", conditionMessage(r), "\n\n", sep = "") else { print(as.data.frame(r)); cat("\n") }
}
# 1. Share output `w` reuses an input column's name; a later unnamed across() never names it.
show(summarize_with_margins(d, s = sum(v), w = share_of_total(s),
  across(everything(), max, .names = "mx_{.col}"), .grouping = rollup(a)))
# 2. any_of() is tolerant by definition, yet "Element `w` doesn't exist".
show(summarize_with_margins(d, s = sum(v), w = share_of_parent(s),
  across(any_of(c("w", "v")), max, .names = "mx_{.col}"), .grouping = rollup(a)))
# Controls: what the same selections see when they run in the mask.
show(summarize_with_margins(d, s = sum(v), w = share_of_total(s),
  sel = paste(names(across(everything(), max)), collapse = ","),
  n_any = ncol(across(any_of("w"), max)),
  across(where(is.numeric), max, .names = "mx_{.col}"), .grouping = rollup(a)))
# Control: the same call with the share under a name that is not an input.
show(summarize_with_margins(d, s = sum(v), t = share_of_total(s),
  across(everything(), max, .names = "mx_{.col}"), .grouping = rollup(a)))
# Independent oracle: the share dropped, the input it hides dropped, per grain.
bind_rows(summarise(select(d, -w), s = sum(v), across(everything(), max, .names = "mx_{.col}"), .by = a),
          summarise(select(d, -w, -a), s = sum(v), across(everything(), max, .names = "mx_{.col}")))
