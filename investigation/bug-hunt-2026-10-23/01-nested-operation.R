# Investigated 2026-10-23 at 0e3f1dcca46f4d51d4d99cd200cf73540c96133d.
# Run from the repository root. Expected behavior assertions fail on that commit.
suppressPackageStartupMessages({pkgload::load_all(quiet=TRUE); library(dplyr)})
d <- tibble(g = 'a', x = 1)
expected <- summarize_with_margins(d, bit = grouping_bit(g), .grouping = rollup(g))
actual <- summarize_with_margins(
  d,
  nested = list(summarize_with_margins(d, bit = grouping_bit(g), .grouping = rollup(g))),
  .grouping = rollup(g)
)
cat('independent inner result:\n');print(expected)
cat('inner result for outer detail:\n');print(actual$nested[[1]])
cat('inner result for outer total:\n');print(actual$nested[[2]])
stopifnot(identical(actual$nested[[1]], expected), identical(actual$nested[[2]], expected))
