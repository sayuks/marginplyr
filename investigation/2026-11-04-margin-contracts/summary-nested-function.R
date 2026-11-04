suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(quiet = TRUE)
data <- data.frame(x = 1:3)
replacement <- function(...) {
  function(...) {
    function(...) data.frame(z = 99L)
  }
}
run <- function(verb) {
  assign("function", replacement, envir = environment())
  verb(data, value = sum(dplyr::mutate(data.frame(y = x), z = y)$z))
}
ordinary <- run(dplyr::summarise)
margin <- run(marginplyr::summarize_with_margins)
print(ordinary)
print(margin)
stopifnot(identical(ordinary$value, 6L))
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(identical(margin, ordinary))
}
