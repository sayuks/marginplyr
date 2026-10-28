suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(if (length(commandArgs(TRUE))) commandArgs(TRUE)[1L] else getwd(), quiet = TRUE)
data <- data.frame(.N = c('P', 'P'), g = c('a', 'b'), x = c(1, 3))
source <- dtplyr::lazy_dt(data)
result <- summarize_with_margins(
  source,
  value = sum(x),
  fraction = share_of_total(value),
  .by = .N,
  .grouping = rollup(g),
  .margin_label = NULL
) |> collect()
stopifnot(identical(sort(result$fraction), c(0.25, 0.75, 1)))
