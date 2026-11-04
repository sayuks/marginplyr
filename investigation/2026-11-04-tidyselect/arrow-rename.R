# Run from the root of the archived package revision.
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
suppressPackageStartupMessages(library(dplyr))

data <- tibble(g = c("a", "b"), x = 1:2, y = 10:11, z = 100:101)
arrow_data <- arrow::Table$create(data)
inputs <- list(tibble = data, Table = arrow_data,
               query = filter(arrow_data, x > 0L))
selections <- rlang::exprs(
  rename = across(c(y = x), sum),
  vector = across(tidyselect::all_of(c(y = "x")), sum),
  exclusion = across(c(y = x, -z), sum),
  names = across(x, sum, .names = "y")
)
divergences <- c(native = 0L, margin = 0L, rollup = 0L)
for (input in names(inputs)) {
  x <- inputs[[input]]
  for (selection in names(selections)) {
    expr <- selections[[selection]]
    results <- list(
      native = summarise(x, !!expr, .by = g),
      margin = summarize_with_margins(x, !!expr, .by = g),
      rollup = summarize_with_margins(x, !!expr, .grouping = rollup(g))
    )
    for (verb in names(results)) {
      result <- results[[verb]]
      if (input != "tibble") stopifnot(inherits(result, "arrow_dplyr_query"))
      result <- collect(result)
      keys <- if (verb == "rollup") c("a", "b", "Total") else c("a", "b")
      want <- if (verb == "rollup") c(1, 2, 3) else c(1, 2)
      actual <- as.numeric(result$y[match(keys, result$g)])
      if (!identical(actual, want)) {
        divergences[[verb]] <- divergences[[verb]] + 1L
        cat(input, selection, verb, paste(actual, collapse = "/"), "\n")
      }
      if (input == "tibble" || selection == "names") {
        stopifnot(identical(actual, want))
      }
    }
  }
}
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(all(divergences[c("margin", "rollup")] == 0L))
}
print(divergences)
