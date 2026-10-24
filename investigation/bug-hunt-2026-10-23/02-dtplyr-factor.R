# Investigated 2026-10-23 at 0e3f1dcca46f4d51d4d99cd200cf73540c96133d.
# Run from the repository root. Expected behavior assertions fail on that commit.
pkgload::load_all('.', quiet = TRUE)
input <- dtplyr::lazy_dt(tibble::tibble(f = factor('a'), v = 1L))
input <- dplyr::mutate(input, f = 'z')
stopifnot(identical(as.character(dplyr::collect(input)$f), 'z'))
result <- dplyr::collect(expand_with_margins(
  input, .grouping = rollup(f), .id = 'set'
))
print(result)
# Label reconstruction must preserve the actual detail value.
stopifnot(identical(as.character(result$f[result$set == 1L]), 'z'))
