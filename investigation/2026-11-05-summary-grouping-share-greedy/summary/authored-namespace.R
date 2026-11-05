Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
data <- data.frame(x = 1:3)
hits <- 0L
`:::` <- function(pkg, name) {
  pkg <- as.character(substitute(pkg))
  name <- as.character(substitute(name))
  if (identical(pkg, 'base') && identical(name, 'sum')) {
    hits <<- hits + 1L
    return(function(...) 17L)
  }
  get(name, envir = asNamespace(pkg))
}
expected <- dplyr::summarise(data, dplyr::across(
  x, sum, .names = '{.col}_{base:::sum(1L, 2L)}', .unpack = identity(FALSE)
))
actual <- summarize_with_margins(data, dplyr::across(
  x, sum, .names = '{.col}_{base:::sum(1L, 2L)}', .unpack = identity(FALSE)
))
stopifnot(identical(actual, expected), identical(names(actual), 'x_17'), hits == 2L)
print(actual)
cat('caller-authored namespace hits:', hits, '\n')
