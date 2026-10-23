# Investigated 2026-10-23 at 0e3f1dcca46f4d51d4d99cd200cf73540c96133d.
# Run from the repository root. Expected behavior assertions fail on that commit.
pkgload::load_all('.', quiet = TRUE)
input <- dtplyr::lazy_dt(tibble::tibble(i = 1L, v = 3))
input <- dplyr::mutate(input, i = 'x')
spec <- grouping_set(dplyr::where(is.numeric))
actual <- inspect_grouping(input, .grouping = spec, .format = 'list')
expected <- inspect_grouping(dplyr::collect(input), .grouping = spec, .format = 'list')
print(actual$included)
print(expected$included)
stopifnot(identical(actual$included, expected$included))
