# Investigated SHA: 5c667c956fd50ba39be621bf1b617a17abda7b14
Sys.setenv(TESTTHAT_PARALLEL = "false")
args <- commandArgs(TRUE)
source_dir <- if (length(args) && !startsWith(args[[1L]], "--")) args[[1L]] else getwd()
pkgload::load_all(source_dir, quiet = TRUE)
library(dplyr)
data <- tibble(g = c("a", "b", "b"), x = c(1, 10, 20))
assert_contract <- "--assert-contract" %in% args
ordinary <- rlang::exprs(sentinel = 7L, (across)(x, sum, .names = "measure"))
reference <- bind_rows(summarise(data, !!!ordinary, .by = g),
  mutate(summarise(data, !!!ordinary), g = "Total"))
control <- rlang::inject(summarize_with_margins(data, !!!ordinary, .grouping = rollup(g)))
bare <- summarize_with_margins(data, sentinel = 7L, across(x, (sum), .names = "measure"),
  p = share_of_total(measure), .grouping = rollup(g))
first <- summarize_with_margins(data, (across)(x, sum, .names = "measure"),
  p = share_of_total(measure), .grouping = rollup(g))
actual <- tryCatch(rlang::inject(summarize_with_margins(data, !!!ordinary,
  p = share_of_total(measure), .grouping = rollup(g))), error = identity)
stopifnot(identical(reference$measure, c(1, 30, 31)),
  identical(control$measure, reference$measure), identical(bare$measure, reference$measure),
  identical(first$measure, reference$measure), identical(bare$p, c(1/31, 30/31, 1)))
print(reference); print(control); print(bare); print(first)
if (inherits(actual, "error")) cat(conditionMessage(actual), "
") else print(actual)
if (assert_contract) stopifnot(!inherits(actual, "error"), identical(actual$measure, reference$measure))
stopifnot(inherits(actual, "marginplyr_error"), grepl("Detected type tbl_df/tbl/data.frame",
  conditionMessage(actual), fixed = TRUE))
