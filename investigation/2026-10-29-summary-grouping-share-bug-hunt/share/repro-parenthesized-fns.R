args <- commandArgs(trailingOnly = TRUE)
devtools::load_all(if (length(args)) args[[1L]] else getwd(), quiet = TRUE)
data <- data.frame(g = c("a", "b"), x = c(1, 3))
control <- summarize_with_margins(
  data, dplyr::across(x, (~sum(.x)), .names = "s"),
  .grouping = rollup(g)
)
stopifnot(identical(control$s, c(1, 3, 4)))
unparenthesized <- summarize_with_margins(
  data, dplyr::across(x, ~sum(.x), .names = "s"),
  share = share_of_total(s), .grouping = rollup(g)
)
stopifnot(identical(unparenthesized$share, c(0.25, 0.75, 1)))
observed <- tryCatch(summarize_with_margins(
  data, dplyr::across(x, (~sum(.x)), .names = "s"),
  share = share_of_total(s), .grouping = rollup(g)
), error = identity)
if (inherits(observed, "error")) {
  cat("OBSERVED:", class(observed)[[1L]], conditionMessage(observed), "\n")
  stop("A named scalar across source with one parenthesized formula must remain eligible for Total shares.")
}
stopifnot(identical(observed$share, c(0.25, 0.75, 1)))
