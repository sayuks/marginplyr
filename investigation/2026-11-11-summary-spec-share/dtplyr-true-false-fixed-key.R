# Investigated SHA: fbf39cd2780365af59f4b25c869e4332a57f19ff
# Run from the repository root: Rscript <this file> [--assert-contract]
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
library(dplyr)
assert_contract <- "--assert-contract" %in% commandArgs(TRUE)
# dtplyr: a fixed key named T (or F) is overwritten by TRUE (FALSE) once a
# share is requested, merging the fixed partitions.
run <- function(key, input) {
  d <- stats::setNames(
    data.frame(c("a", "a", "b", "b"), c("x", "y", "x", "y"), c(1, 2, 3, 4)),
    c(key, "g", "v")
  )
  source <- if (input == "dtplyr") dtplyr::lazy_dt(d) else d
  out <- summarize_with_margins(
    source, zz = sum(v), sh = share_of_total(zz),
    .by = all_of(key), .grouping = rollup(g), .sort = "last"
  )
  as.data.frame(collect(out))
}
# Independent oracle: ordinary summaries at the two explicit grains.
oracle <- function() {
  d <- data.frame(k = c("a", "a", "b", "b"), v = c(1, 2, 3, 4))
  detail <- d$v / ave(d$v, d$k, FUN = sum)
  c(detail[1:2], 1, detail[3:4], 1)
}
for (key in c("T", "F")) {
  local_result <- run(key, "local")
  lazy_result <- run(key, "dtplyr")
  cat("key", key, "- local\n"); print(local_result)
  cat("key", key, "- dtplyr\n"); print(lazy_result)
  stopifnot(isTRUE(all.equal(local_result$sh, oracle())))
  if (assert_contract) {
    stopifnot(identical(lazy_result[[key]], local_result[[key]]),
              isTRUE(all.equal(lazy_result$sh, oracle())))
  }
}
