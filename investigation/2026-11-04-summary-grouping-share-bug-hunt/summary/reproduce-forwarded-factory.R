pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))

data <- data.frame(g = c("a", "a", "b"), x = 1:3)
run <- function(verb, canonical = FALSE) {
  calls <- 0L
  unpack <- TRUE
  factory <- function() {
    calls <<- calls + 1L
    offset <- calls
    function(x) data.frame(value = sum(x) + offset)
  }
  wrapper <- if (canonical) {
    function(...) verb(data, across(x, ..1, .unpack = unpack), .by = g)
  } else {
    function(...) verb(data, across(x, ..01, .unpack = unpack), .by = g)
  }
  list(result = wrapper(factory()), calls = calls)
}
ordinary <- run(dplyr::summarise)
margin <- run(marginplyr::summarize_with_margins)
canonical <- run(marginplyr::summarize_with_margins, TRUE)
stopifnot(identical(ordinary$result$x_value, c(5L, 4L)),
          identical(ordinary$calls, 2L), identical(canonical, ordinary))
cat("Ordinary:\n")
print(ordinary)
cat("Noncanonical Margin:\n")
print(margin)
cat("Canonical Margin:\n")
print(canonical)
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  stopifnot(identical(margin, ordinary))
} else {
  stopifnot(identical(margin$result$x_value, c(4L, 4L)),
            identical(margin$calls, 1L))
}
