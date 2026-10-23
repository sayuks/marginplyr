# Investigated 2026-10-23 at 0e3f1dcca46f4d51d4d99cd200cf73540c96133d.
# Run from the repository root. Expected behavior assertions fail on that commit.
suppressPackageStartupMessages({pkgload::load_all(quiet=TRUE);library(dplyr)})
d <- tibble(g = c('a','b'), x = c(2,6))
scalar <- summarize_with_margins(d, v=sum(x^2), p=share_of_parent(v), .grouping=rollup(g))
cat('ordinary scalar result:\n');print(scalar)
ordinary_matrix <- summarize_with_margins(d, v=crossprod(x), .grouping=rollup(g))
cat('ordinary matrix summary:\n');str(ordinary_matrix)
err <- tryCatch(summarize_with_margins(d,v=crossprod(x),p=share_of_parent(v),.grouping=rollup(g)), error=identity)
cat('matrix share condition classes:',paste(class(err),collapse=', '),'\n')
cat(conditionMessage(err),'\n')
stopifnot(inherits(err,'marginplyr_error'))
