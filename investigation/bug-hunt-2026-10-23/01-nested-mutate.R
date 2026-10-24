# Investigated 2026-10-23 at 0e3f1dcca46f4d51d4d99cd200cf73540c96133d.
# Run from the repository root. Expected behavior assertions fail on that commit.
suppressPackageStartupMessages({pkgload::load_all(quiet=TRUE);library(dplyr)})
d <- tibble(g='a')
inner <- tibble(g=1,x=2)
expected <- mutate(inner,across(everything(),~ .x*2))
actual <- summarize_with_margins(
  d,nested=list(dplyr::mutate(inner,across(everything(),~ .x*2))),
  .grouping=rollup(g)
)
cat('independent mutate:\n');print(expected)
cat('mutate nested in margin summary:\n');print(actual$nested[[1]])
stopifnot(identical(actual$nested[[1]],expected))
