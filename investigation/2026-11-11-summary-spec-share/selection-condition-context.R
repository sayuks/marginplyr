# Investigated SHA: fbf39cd2780365af59f4b25c869e4332a57f19ff
# Run from the repository root: Rscript <this file> [--assert-contract]
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
library(dplyr)
assert_contract <- "--assert-contract" %in% commandArgs(TRUE)
# A failed column selection blames an internal call, and a summary selection
# also quotes an argument the caller did not write.
d <- tibble::tibble(g = c("a", "b"), x = 1:2)
caught <- function(expr) tryCatch(expr, error = identity)
chain <- function(cnd) {
  out <- list()
  while (!is.null(cnd)) {
    out[[length(out) + 1L]] <- cnd
    cnd <- cnd$parent
  }
  out
}
head_name <- function(cnd) {
  call <- conditionCall(cnd)
  if (is.call(call)) rlang::as_label(call[[1L]]) else NA_character_
}
cases <- list(
  by = caught(summarize_with_margins(d, n = n(), .by = zz)),
  grouping = caught(summarize_with_margins(d, n = n(), .grouping = rollup(zz))),
  first_across = caught(summarize_with_margins(
    d, across(zz, sum), .grouping = rollup(g)
  )),
  later_across = caught(summarize_with_margins(
    d, n = n(), across(zz, sum), .grouping = rollup(g)
  )),
  pick = caught(summarize_with_margins(
    d, n = nrow(pick(zz)), .grouping = rollup(g)
  ))
)
native <- caught(summarise(d, n = n(), across(zz, sum), .by = g))
cat("native:\n", conditionMessage(native), "\n")
for (name in names(cases)) {
  cat("--", name, "\n")
  cat(conditionMessage(cases[[name]]), "\n")
  cat("blamed:", vapply(chain(cases[[name]]), head_name, ""), "\n")
}
if (assert_contract) {
  for (cnd in cases) {
    blamed <- vapply(chain(cnd), head_name, "")
    stopifnot(!any(grepl("^resolve_", blamed)),
              !grepl("(function (x)", conditionMessage(cnd), fixed = TRUE))
  }
}
