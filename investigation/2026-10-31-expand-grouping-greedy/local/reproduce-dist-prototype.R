# Dated investigation replay; run from the repository root.
devtools::load_all(quiet = TRUE)
assert <- "--assert" %in% commandArgs(trailingOnly = TRUE)
input <- tibble::tibble(
  key = stats::dist(matrix(c(0, 1), ncol = 1L)),
  row = 1L
)
included <- expand_with_margins(
  input, .grouping = grouping_set(key), .margin_label = NULL
)
stopifnot(identical(included, input))
expected <- dplyr::union_all(
  dplyr::mutate(input, occurrence = 1L),
  dplyr::mutate(
    input, key = vctrs::vec_init(key, dplyr::n()), occurrence = 2L
  )
)
expected <- dplyr::relocate(expected, key, occurrence)
stopifnot(inherits(expected$key, "dist"), nrow(expected) == 2L)
stopifnot(identical(as.numeric(expected$key), c(1, NA_real_)))
actual <- tryCatch(
  expand_with_margins(
    input, .grouping = rollup(key), .margin_label = NULL, .id = "occurrence"
  ),
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
  stopifnot(!inherits(actual, "error"), identical(actual, expected))
}
