args <- commandArgs(trailingOnly = TRUE)
source_root <- if (length(args)) args[[1L]] else getwd()
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(source_root, quiet = TRUE)
frame <- function(x) tibble::tibble(a = sum(x))
cases <- list(
  list(quote(frame(x)), z = quote(1:2)),
  list(z = quote(1:2), quote(frame(x))),
  list(quote(frame(x)), z = quote(integer())),
  list(z = quote(integer()), quote(frame(x))),
  list(quote(frame(x)), z = quote(NA_real_)),
  list(quote(frame(x)), z = quote(NULL))
)
converters <- list(identity, tibble::as_tibble, data.table::as.data.table)
counts <- c(paired_errors = 0L, exact_results = 0L)
for (convert in converters) {
  input <- convert(data.frame(g = c("a", "b"), x = c(1, 2)))
  for (dots in cases) {
    expected <- tryCatch(rlang::inject(dplyr::summarise(input, !!!dots, .by = "g")), error = identity)
    actual <- tryCatch(rlang::inject(summarize_with_margins(input, !!!dots, .by = "g")), error = identity)
    stopifnot(identical(inherits(actual, "error"), inherits(expected, "error")))
    if (inherits(actual, "error")) {
      counts[["paired_errors"]] <- counts[["paired_errors"]] + 1L
    } else {
      stopifnot(identical(actual, expected))
      counts[["exact_results"]] <- counts[["exact_results"]] + 1L
    }
  }
}
print(counts)
