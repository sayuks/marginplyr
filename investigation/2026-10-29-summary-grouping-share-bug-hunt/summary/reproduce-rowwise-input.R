args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) normalizePath(args[[1L]]) else getwd()
suppressPackageStartupMessages(library(dplyr))
devtools::load_all(repo, quiet = TRUE)
data <- tibble(g = "a", x = 1)
expected <- summarise(
  data,
  width = ncol(dplyr::rowwise(data = dplyr::pick(everything()))),
  .by = g
)
positional <- summarize_with_margins(
  data,
  width = ncol(dplyr::rowwise(dplyr::pick(everything()))),
  .grouping = grouping_set(g), .margin_label = NULL
)
actual <- summarize_with_margins(
  data,
  width = ncol(dplyr::rowwise(data = dplyr::pick(everything()))),
  .grouping = grouping_set(g), .margin_label = NULL
)
cat("DPLYR NAMED INPUT CONTROL\n"); print(expected)
cat("MARGIN POSITIONAL INPUT CONTROL\n"); print(positional)
cat("MARGIN NAMED INPUT\n"); print(actual)
stopifnot(identical(expected, positional))
if (!identical(actual, expected)) {
  stop("BUG: naming rowwise's data input changes a Margin summary value.",
       call. = FALSE)
}
