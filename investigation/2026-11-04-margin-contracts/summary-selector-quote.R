suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(quiet = TRUE)
data <- data.frame(x = 1:3, y = 10:12)
run <- function(verb) {
  quote <- function(...) base::quote(y)
  verb(data, value = sum(dplyr::pick(x)))
}
ordinary <- run(dplyr::summarise)
margin <- run(marginplyr::summarize_with_margins)
print(ordinary)
print(margin)
stopifnot(identical(ordinary$value, 6L))
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(identical(margin, ordinary))
}
