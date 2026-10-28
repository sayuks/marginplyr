args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) normalizePath(args[[1L]]) else getwd()
suppressPackageStartupMessages(library(dplyr))
devtools::load_all(repo, quiet = TRUE)
data <- tibble(g = c("a", "b"), x = c(1, 2), y = c(10, 20))
run <- function(margin, modern) {
  reads <- 0L
  makeActiveBinding("cols", function() {
    reads <<- reads + 1L
    if (reads == 1L) "x" else "y"
  }, environment())
  selection <- if (modern) rlang::expr(all_of(cols)) else rlang::sym("cols")
  dot <- rlang::expr(across(!!selection, sum))
  result <- suppressWarnings(if (margin) {
    rlang::eval_tidy(rlang::expr(summarize_with_margins(
      data, !!dot, .grouping = grouping_set(g), .margin_label = NULL
    )))
  } else {
    rlang::eval_tidy(rlang::expr(summarise(data, !!dot, .by = g)))
  })
  list(result = result, reads = reads)
}
for (modern in c(FALSE, TRUE)) {
  expected <- run(FALSE, modern)
  actual <- run(TRUE, modern)
  cat("MODERN all_of", modern, "\nDPLYR\n"); print(expected)
  cat("MARGIN\n"); print(actual)
  if (modern) stopifnot(identical(expected, actual))
}
cat("Deprecated bare external-vector selection differs; modern all_of control passed.\n")
