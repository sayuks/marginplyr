# Investigation evidence, 2026-10-26. Run from the repository root.
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
assert_mode <- "--assert" %in% commandArgs(trailingOnly = TRUE)
make_source <- function(values) {
  encoded <- arrow::call_function("dictionary_encode", arrow::Array$create(values))
  arrow::Table$create(g = encoded, payload = seq_along(values))
}
cases <- list(
  integer = c(9L, 10L, NA_integer_, -2L, 2L),
  double = c(9.5, 10.1, NA_real_, 2.2, -2.1),
  character = c("b", "aa", NA_character_, "ab", "a")
)
failures <- 0L
for (kind in names(cases)) {
  source <- make_source(cases[[kind]])
  decoded <- switch(
    kind,
    integer = mutate(source, g = as.integer(g)),
    double = mutate(source, g = as.numeric(g)),
    character = mutate(source, g = as.character(g))
  )
  for (position in c("first", "last")) {
    for (verb in c("expand", "summarize")) {
      run <- function(data) {
        if (verb == "expand") {
          expand_with_margins(data, .by = g, .sort = position)
        } else {
          summarize_with_margins(
            data, payload = sum(payload), .by = g, .sort = position
          )
        }
      }
      got <- suppressWarnings(collect(run(source)))
      control <- suppressWarnings(collect(run(decoded)))
      stopifnot(identical(control$payload, order(cases[[kind]], na.last = TRUE)))
      stopifnot(tail(is.na(got$g), 1L))
      passed <- identical(got$payload, control$payload)
      cat(kind, verb, position, "actual:", got$payload,
          "expected:", control$payload, "contract:", passed, "\n")
      if (kind == "character") stopifnot(passed)
      failures <- failures + as.integer(!passed)
    }
  }
}
cat("Desired-contract failures:", failures, "\n")
if (assert_mode) stopifnot(failures == 0L)
