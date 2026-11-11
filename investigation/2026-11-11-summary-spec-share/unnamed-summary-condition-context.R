# Investigated SHA: fbf39cd2780365af59f4b25c869e4332a57f19ff
# Run from the repository root: Rscript <this file> [--assert-contract]
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
library(dplyr)
assert_contract <- "--assert-contract" %in% commandArgs(TRUE)
# Local input: two or more unnamed summaries are quoted as the internal
# wrapper, and distinct arguments' warnings collapse into one.
d <- data.frame(g = c("a", "a", "b"), v = 1:3, w = c("1", "x", "4"))
collect_warnings <- function(expr) {
  seen <- list()
  withCallingHandlers(expr, warning = function(w) {
    seen[[length(seen) + 1L]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  })
  seen
}
one <- collect_warnings(summarize_with_margins(
  d, sum(v), sum(as.numeric(w)), .grouping = rollup(g)
))
cat(one[[1L]], "\n")
failed <- tryCatch(summarize_with_margins(
  d, mean(v), max(w + 1), .grouping = rollup(g)
), error = identity)
cat(conditionMessage(failed), "\n")
unnamed <- collect_warnings(summarize_with_margins(
  d, sum(if (n() == 3L) as.integer(w) else v),
  max(if (n() < 3L) as.integer(w) else v), .grouping = rollup(g)
))
named <- collect_warnings(summarize_with_margins(
  d, a = sum(if (n() == 3L) as.integer(w) else v),
  b = max(if (n() < 3L) as.integer(w) else v), .grouping = rollup(g)
))
cat("unnamed warnings:", length(unnamed), " named warnings:", length(named), "\n")
stopifnot(length(named) == 2L)
if (assert_contract) {
  stopifnot(grepl("`sum(as.numeric(w))`", one[[1L]], fixed = TRUE),
            grepl("`max(w + 1)`", conditionMessage(failed), fixed = TRUE),
            length(unnamed) == 2L)
}
