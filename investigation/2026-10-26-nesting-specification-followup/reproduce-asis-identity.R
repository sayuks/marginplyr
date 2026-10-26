# Dated investigation evidence: 2026-10-26, source bfcdaf76a36d102132cd039c68ba8bc05a4fd465.
args <- commandArgs(TRUE)
root <- if(length(args)) args[[1]] else "."
pkgload::load_all(root, quiet=TRUE)
x <- tibble::tibble(g = I(1))
l <- dtplyr::lazy_dt(x)
local <- nest_with_margins(x, .grouping = rollup(g))
single <- dplyr::collect(nest_with_margins(l, .grouping = grouping_set(g)))
upstream <- dplyr::collect(dplyr::union_all(l, dplyr::mutate(l, g = I(NA_real_))))
stopifnot(nrow(local) == 2L, nrow(single) == 1L, nrow(upstream) == 2L)
cat("local rollup, dtplyr single set, and class-preserving upstream union: PASS\n")
actual <- dplyr::collect(nest_with_margins(l, .grouping = rollup(g)))
stopifnot(nrow(actual) == 2L, identical(vapply(actual$data, nrow, integer(1)), c(1L,1L)))
