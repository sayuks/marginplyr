# Investigated SHA: fbf39cd2780365af59f4b25c869e4332a57f19ff
# Run from the repository root: Rscript <this file> [--assert-contract]
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
library(dplyr)
assert_contract <- "--assert-contract" %in% commandArgs(TRUE)
# Local Margin order: NaN and NA partitions of one fixed key interleave.
d <- data.frame(
  x = c(NaN, NA, NaN, NA, 1, 1), g = c("a", "a", "b", "b", "a", "b"),
  v = c(1L, 2L, 4L, 8L, 16L, 32L)
)
ordered <- function(source) {
  as.data.frame(collect(summarize_with_margins(
    source, s = sum(v), .by = x, .grouping = rollup(g), .sort = "last"
  )))
}
# Ordinary dplyr keeps NaN and NA as separate groups.
print(summarise(d, s = sum(v), .by = x))
local_result <- ordered(d)
lazy_result <- ordered(dtplyr::lazy_dt(d))
print(local_result); print(lazy_result)
partition <- function(result) {
  ifelse(is.nan(result$x), "NaN", ifelse(is.na(result$x), "NA", "1"))
}
contiguous <- function(result) {
  runs <- rle(partition(result))$values
  !anyDuplicated(runs)
}
stopifnot(contiguous(lazy_result))
if (assert_contract) stopifnot(contiguous(local_result))
