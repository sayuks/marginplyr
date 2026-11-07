Sys.setenv(TESTTHAT_PARALLEL = 'false')
args <- commandArgs(trailingOnly = TRUE)
pkgload::load_all(args[[1L]], quiet = TRUE)
for (kind in c('list', 'matrix', 'packed')) {
  key <- switch(kind,
    list = list(1, 1, 2, NULL, 2, NULL),
    matrix = matrix(1:12, ncol = 2),
    packed = tibble::tibble(a = 1:6, b = letters[1:6])
  )
  data <- tibble::tibble(k = key, g = letters[1:6], value = 1:6)
  lazy <- dtplyr::lazy_dt(data)
  actual <- tryCatch(
    tibble::as_tibble(dplyr::summarise(
      lazy, n = sum(value), .by = dplyr::all_of('k')
    )), error = identity
  )
  stopifnot(inherits(actual, 'error'))
  cat(kind, conditionMessage(actual), '\n')
}
