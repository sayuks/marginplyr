pkgload::load_all(quiet = TRUE)

data <- data.frame(a = 1L)
capture <- function(spec) {
  result <- tryCatch(inspect_grouping(data, .grouping = spec), error = identity)
  stopifnot(inherits(result, "marginplyr_error"))
  conditionMessage(result)
}
bare <- function(...) capture(grouping_set(alias = ..1, a))
parenthesized <- function(...) capture(grouping_set(alias = (..1), a))
unnamed <- function(...) capture(grouping_set((..1), a))

# jarl-ignore missing_argument: The missing first dot is the test input.
expected <- bare(, NULL)
# jarl-ignore missing_argument: The missing first dot is the test input.
observed <- parenthesized(, NULL)
# jarl-ignore missing_argument: The missing first dot is the test input.
unnamed_control <- unnamed(, NULL)
stopifnot(
  grepl("Argument 1 of `grouping_set()` is empty.", expected, fixed = TRUE),
  identical(expected, unnamed_control),
  grepl("takes no named arguments", parenthesized(a), fixed = TRUE)
)

if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(identical(observed, expected))
} else {
  stopifnot(grepl("takes no named arguments", observed, fixed = TRUE))
  cat("Bare:", expected, "\n")
  cat("Parenthesized:", observed, "\n")
}
