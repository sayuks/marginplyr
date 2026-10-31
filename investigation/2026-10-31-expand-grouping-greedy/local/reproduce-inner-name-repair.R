# Dated investigation replay; run from the repository root.
devtools::load_all(quiet = TRUE)
assert <- "--assert" %in% commandArgs(trailingOnly = TRUE)
input <- tibble::tibble(g = "a", payload = summary(1)[1L])
included <- expand_with_margins(input, .grouping = grouping_set(g))
stopifnot(identical(included, input), identical(names(input), c("g", "payload")))
expected <- dplyr::union_all(input, dplyr::mutate(input, g = "Total"))
stopifnot(nrow(expected) == 2L)
stopifnot(identical(as.numeric(expected$payload), c(1, 1)))
actual <- tryCatch(
  expand_with_margins(input, .grouping = rollup(g)),
  error = identity
)
cat("Included-only comparison: passed\n")
cat("Ordinary mutation/union comparison: passed\n")
if (inherits(actual, "error")) {
  cat("Expansion condition:", class(actual)[[1L]], "\n")
  cat(conditionMessage(actual), "\n")
} else {
  cat("Expansion comparison:", isTRUE(all.equal(actual, expected)), "\n")
}
if (assert) {
  stopifnot(!inherits(actual, "error"), nrow(actual) == 2L)
  stopifnot(identical(names(actual), names(expected)))
  stopifnot(identical(actual$g, expected$g))
  stopifnot(identical(as.numeric(actual$payload), c(1, 1)))
}
