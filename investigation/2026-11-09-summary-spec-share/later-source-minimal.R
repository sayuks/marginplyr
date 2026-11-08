# Investigated SHA: 5c667c956fd50ba39be621bf1b617a17abda7b14
Sys.setenv(TESTTHAT_PARALLEL = "false")
args <- commandArgs(TRUE)
source_dir <- if (length(args) && !startsWith(args[[1L]], "--")) args[[1L]] else getwd()
pkgload::load_all(source_dir, quiet = TRUE)
library(dplyr)
data <- tibble(g = c("a", "b", "b"), x = c(1, 10, 20))
assert_contract <- "--assert-contract" %in% args
factory <- function(offset) {
  force(offset)
  function(x) sum(x) + offset
}
ordinary <- rlang::exprs(sentinel = 7L, across(x, list(s = factory(dplyr::n()))))
detail <- summarise(data, !!!ordinary, .by = g)
root <- mutate(summarise(data, !!!ordinary), g = "Total")
reference <- bind_rows(detail, root)
control <- rlang::inject(summarize_with_margins(data, !!!ordinary, .grouping = rollup(g)))
actual <- rlang::inject(summarize_with_margins(
  data, !!!ordinary, p = share_of_total(x_s), .grouping = rollup(g)
))
print(reference); print(control); print(actual)
stopifnot(identical(reference$x_s, c(1, 30, 31)), identical(control$x_s, reference$x_s))
stopifnot(identical(actual$x_s, c(2, 32, 34)), identical(actual$p, c(2/34, 32/34, 1)))
if (assert_contract) stopifnot(identical(actual$x_s, reference$x_s))
