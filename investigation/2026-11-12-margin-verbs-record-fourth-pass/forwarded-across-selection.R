# Investigated SHA: 03e3cef502f7332b69e50608740033043d27f2b4
# Run from the repository root: Rscript <this file>. Prints observed behaviour.
# A wrapper's forwarded across() selection is refused where dplyr evaluates across() per group.
suppressMessages(pkgload::load_all(".", quiet = TRUE))
suppressMessages(library(dplyr))
d <- tibble::tibble(g = c("a", "a", "b", "b"), v = c(1, 2, 3, 4), w = c(4, 5, 6, 7))
range2 <- function(x) tibble::tibble(lo = min(x), hi = max(x))
show <- function(label, expr) {
  out <- tryCatch(suppressWarnings(expr), error = function(e) e)
  cat("==", label, "\n")
  if (inherits(out, "condition")) cat(conditionMessage(out), "\n") else print(as.data.frame(out))
}
native <- function(data, ...) summarise(data, across(...), .by = g)
margin <- function(data, ...) summarize_with_margins(data, across(...), .by = g)
show("native  where(), .unpack = TRUE", native(d, where(is.numeric), range2, .unpack = TRUE))
show("margin  where(), .unpack = TRUE", margin(d, where(is.numeric), range2, .unpack = TRUE))
show("native  c(v, w), mean, na.rm = TRUE", native(d, c(v, w), mean, na.rm = TRUE))
show("margin  c(v, w), mean, na.rm = TRUE", margin(d, c(v, w), mean, na.rm = TRUE))
native_p <- function(data, ...) summarise(data, (across(...)), .by = g)
margin_p <- function(data, ...) summarize_with_margins(data, (across(...)), .by = g)
show("native  (across(...)) <- c(v, w), mean", native_p(d, c(v, w), mean))
show("margin  (across(...)) <- c(v, w), mean", margin_p(d, c(v, w), mean))
margin_r <- function(data, ...) summarise_with_margins(data, across(...), .grouping = rollup(g))
show("margin  alias, rollup, data.table", margin_r(data.table::as.data.table(d), c(v, w), range2, .unpack = TRUE))
# Controls: the same forwarding works when the summary is named or the selection is written out.
margin_n <- function(data, ...) summarize_with_margins(data, z = across(...), .by = g)
show("control named z = across(...)", margin_n(d, where(is.numeric), range2, .unpack = TRUE)$z)
margin_x <- function(data, ...) summarize_with_margins(data, across(where(is.numeric), ..., .unpack = TRUE), .by = g)
show("control explicit selection", margin_x(d, range2))
