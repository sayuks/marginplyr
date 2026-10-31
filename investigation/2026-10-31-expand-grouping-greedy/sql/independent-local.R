pkgload::load_all(quiet = TRUE)
assert <- "--assert" %in% commandArgs(trailingOnly = TRUE)
fixtures <- list(
  dist = stats::dist(matrix(c(0, 1, 3), ncol = 1L))
)
passed <- logical()
for (name in names(fixtures)) {
  key <- fixtures[[name]]
  input <- tibble::tibble(key = key, row = seq_along(key))
  missing <- vctrs::vec_init(key, nrow(input))
  expected <- dplyr::union_all(input, dplyr::mutate(input, key = missing))
  included <- expand_with_margins(input, .grouping = grouping_set(key), .margin_label = NULL)
  stopifnot(identical(included, input), nrow(expected) == 2L * nrow(input))
  result <- tryCatch(
    expand_with_margins(input, .grouping = rollup(key), .margin_label = NULL),
    error = identity
  )
  cat(name, "control_rows", nrow(expected), "control_class", class(expected$key), "\n")
  if (inherits(result, "error")) {
    cat("actual_class", class(result)[[1L]], "\n", conditionMessage(result), "\n")
  }
  passed[[name]] <- !inherits(result, "error") && isTRUE(all.equal(result, expected))
}
input <- tibble::tibble(g = "a", payload = summary(1)[1L])
expected <- dplyr::union_all(input, dplyr::mutate(input, g = "Total"))
stopifnot(nrow(expected) == 2L)
result <- tryCatch(expand_with_margins(input, .grouping = rollup(g)), error = identity)
cat("summary payload control_rows", nrow(expected), "control_class", class(expected$payload), "\n")
if (inherits(result, "error")) {
  cat("actual_class", class(result)[[1L]], "\n", conditionMessage(result), "\n")
}
passed[["summary_payload"]] <- !inherits(result, "error") && isTRUE(all.equal(result, expected))
print(passed)
if (assert && !all(passed)) stop("Expected ordinary typed-missing expansion behavior was not met.")
