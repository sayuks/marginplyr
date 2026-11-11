# Investigated SHA: fbf39cd2780365af59f4b25c869e4332a57f19ff
# Run from the repository root: Rscript <this file> [--assert-contract]
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
library(dplyr)
assert_contract <- "--assert-contract" %in% commandArgs(TRUE)
# Arrow: two different Grouping-bit sort terms receive one truncated field
# name, and collecting a sorted result fails.
sales <- data.frame(
  region = c("E", "W", "E", "W"), channel = c("web", "web", "shop", "shop"),
  segment = c("b2b", "b2c", "b2c", "b2b"), brand = c("x", "y", "x", "y"),
  tier = c("hi", "lo", "lo", "hi"), revenue = c(10, 20, 30, 40)
)
one_way <- grouping_sets(
  grouping_set(region), grouping_set(channel), grouping_set(segment),
  grouping_set(brand), grouping_set(tier)
)
wide <- function(source, sort) {
  tryCatch(as.data.frame(collect(summarize_with_margins(
    source, revenue = sum(revenue), .grouping = one_way,
    .margin_label = NULL, .sort = sort
  ))), error = identity)
}
long_id <- function(source, id) {
  tryCatch(as.data.frame(collect(summarize_with_margins(
    source, revenue = sum(revenue), .grouping = rollup(region, channel),
    .sort = "last", .id = id
  ))), error = identity)
}
table <- arrow::arrow_table(sales)
expected <- wide(sales, "last")
print(expected)
observed <- wide(table, "last")
if (inherits(observed, "error")) cat(conditionMessage(observed), "\n")
stopifnot(nrow(wide(table, "none")) == 10L)
short <- long_id(table, "gid")
long <- long_id(table, "margin_grouping_set_identifier")
stopifnot(is.data.frame(short), nrow(short) == 7L)
if (inherits(long, "error")) cat(conditionMessage(long), "\n")
if (assert_contract) {
  stopifnot(is.data.frame(observed),
            isTRUE(all.equal(observed, expected, check.attributes = FALSE)),
            is.data.frame(long), nrow(long) == 7L)
}
