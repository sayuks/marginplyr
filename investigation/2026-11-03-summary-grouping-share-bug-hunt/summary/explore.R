options(warn = 1, lifecycle_verbosity = "quiet")
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
library(dplyr)
x <- tibble(g = c("a", "a", "b"), x = 1:3, y = 5:7)
exprs <- c(
  '(function(...) pick(...))(x)',
  '(function(...) pick(..1))(x)',
  '(function(...) pick(c(...)))(x)',
  '(function(...) across(c(...), sum))(x)',
  '(function(...) across(all_of(...), sum))("x")',
  '(function(...) across((...), sum))(x)',
  '(function(...) across((..1), sum))(x)',
  '(function(...) across(c(..1), sum))(x)',
  '(function(...) across({ ... }, sum))(x)',
  '(function(...) across(identity(...), sum))(x)',
  '(function(...) across(...))(x, sum)',
  '(function(...) if_any(...))(x, ~ .x > 2)',
  '(function(...) across(.cols = ..., .fns = ..2))(x, sum)',
  '(function(...) across(..1, ..2))(x, sum)',
  '(function(...) across(x, ..1, .unpack = TRUE))(function(v) data.frame(z = sum(v)))',
  '(function(...) across(x, ..1, .unpack = FALSE))(sum)',
  '(function(...) across(x, .fns = ..1, .names = "{.col}_{.fn}"))(sum)',
  '(function(...) { across(x, ...)})(sum)',
  '(function(...) { across(x, .fns = ..., .unpack = TRUE)})(function(v) data.frame(z = sum(v)))',
  '(function(...) { across(x, .fns = ..., .unpack = FALSE)})(sum)',
  '(function(...) pick(all_of(...)))("x")'
)
cap <- function(e, margin) tryCatch({
  q <- rlang::new_quosure(e, globalenv())
  suppressWarnings(if (margin) summarize_with_margins(x, !!q, .by = g) else summarise(x, !!q, .by = g))
}, error = function(cnd) paste(class(cnd)[[1]], conditionMessage(cnd)))
for (txt in exprs) {
  e <- str2lang(txt)
  a <- cap(e, FALSE)
  b <- cap(e, TRUE)
  cat("\nEXPR:", txt, "\n")
  cat("equal:", identical(a,b), "\n")
  if (!identical(a,b)) { print(a); print(b) }
}
