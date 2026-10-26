# Dated investigation evidence, investigated 2026-10-26 at 6b91254016dbcce60cc2cc69071f01f69eb8084e.
# Run from the repository root, or pass its path as the first argument.
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1L]] else "."
pkgload::load_all(repo, quiet = TRUE)

x <- tibble::tibble(g = c("a", "b"), value = 1:2)
expected <- nest_with_margins(x, .grouping = rollup(g), .id = "set")
stopifnot(
  identical(expected$set, c(1L, 1L, 2L)),
  identical(vapply(expected$data, nrow, integer(1)), c(1L, 1L, 2L))
)
names(expected)[names(expected) == "set"] <- ".rows"
actual <- tryCatch(
  nest_with_margins(x, .grouping = rollup(g), .id = ".rows"),
  error = identity
)
if (inherits(actual, "error")) {
  cat(conditionMessage(actual), "\n")
  stop("Confirmed .rows nesting failure; safely named control passed.", call. = FALSE)
}
stopifnot(identical(actual, expected))
cat("PASS: .rows identifier agrees with the safely named control.\n")
