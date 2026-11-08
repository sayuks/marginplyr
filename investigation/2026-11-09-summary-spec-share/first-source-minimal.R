# Investigated SHA: 5c667c956fd50ba39be621bf1b617a17abda7b14
Sys.setenv(TESTTHAT_PARALLEL = "false")
args <- commandArgs(TRUE)
source_dir <- if (length(args) && !startsWith(args[[1L]], "--")) args[[1L]] else getwd()
pkgload::load_all(source_dir, quiet = TRUE)
library(dplyr)
data <- tibble(g = c("a", "b", "b"), x = c(1, 10, 20))
assert_contract <- "--assert-contract" %in% args
ordinary <- rlang::expr(across(x, list(s = (function() {
  offset <- grouping_bit(g)
  function(x) sum(x) + offset
})())))
control <- rlang::inject(summarize_with_margins(data, !!ordinary, .grouping = rollup(g)))
stopifnot(identical(control$x_s, c(1, 30, 32)))
actual <- tryCatch(rlang::inject(summarize_with_margins(
  data, !!ordinary, p = share_of_total(x_s), .grouping = rollup(g)
)), error = identity)
print(control)
if (inherits(actual, "error")) cat(conditionMessage(actual), "
") else print(actual)
if (assert_contract) stopifnot(!inherits(actual, "error"), identical(actual$x_s, control$x_s))
stopifnot(inherits(actual, "error"), grepl("only be used inside", conditionMessage(actual), fixed = TRUE))
