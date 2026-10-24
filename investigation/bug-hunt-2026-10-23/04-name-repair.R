# Investigated 2026-10-23 at 0e3f1dcca46f4d51d4d99cd200cf73540c96133d.
# Run from the repository root. Expected behavior assertions fail on that commit.
pkgload::load_all('.', quiet = TRUE)
input <- tibble::tibble(g = 'a', `sales...1` = 1L)
result <- expand_with_margins(input, .grouping = rollup(g))
# A margin expansion retains every input column under its original name.
stopifnot(identical(names(result), names(input)))
