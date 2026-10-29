# Investigated 2026-10-29 against 59d55a7867ffc0255f75be32a233d9d475bcb7fb.
# Run with Rscript <script> [repository-root] [--assert].
args <- commandArgs(trailingOnly = TRUE)
positional <- args[!startsWith(args, '--')]
repo <- if (length(positional)) positional[[1L]] else '.'
pkgload::load_all(repo, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
assert <- '--assert' %in% args
src <- data.table::data.table(g = c('a', 'b', 'a'), v = 1:3)
before <- data.table::copy(src)
root <- dtplyr::lazy_dt(src)
step <- filter(root, TRUE) |> compute() |> rename(amount = v)
materialized <- collect(step)
stopifnot(identical(materialized$g, c('a', 'b', 'a')),
          identical(materialized$amount, 1:3), identical(src, before))
cat('Standalone collect: 3 source rows; caller source unchanged\n')
expected_cells <- list(c(1L, 3L), 2L, 1:3)
check_nest <- function(value) {
 stopifnot(identical(value$g, c('a', 'b', 'Total')),
   identical(lapply(value$data, function(cell) cell$amount), expected_cells))
}
consumers <- list(
 nest = function(x, spec = rollup(g)) collect(nest_with_margins(
   x, .grouping = spec, .sort = 'last')),
 nest_by = function(x, spec = rollup(g)) nest_by_with_margins(
   x, .grouping = spec, .sort = 'last'),
 expand = function(x, spec = rollup(g)) collect(expand_with_margins(
   x, .grouping = spec, .sort = 'last')),
 summarize = function(x, spec = rollup(g)) collect(summarize_with_margins(
   x, n = n(), .grouping = spec, .sort = 'last'))
)
check <- function(value, name) {
 if(name %in% c('nest', 'nest_by')) check_nest(value)
 if(name == 'expand') stopifnot(nrow(value) == 6L,
   identical(sort(paste(value$g, value$amount)),
     sort(c('a 1', 'b 2', 'a 3', 'Total 1', 'Total 2', 'Total 3'))))
 if(name == 'summarize') stopifnot(identical(value$g, c('a', 'b', 'Total')),
   identical(value$n, c(2L, 1L, 3L)))
}
failed <- character()
for(name in names(consumers)) {
 f <- consumers[[name]]
 # Each control covers the same source rows through the complete public path.
 check(f(materialized), name)
 check(f(compute(step)), name)
 single <- f(step, grouping_set(g))
 stopifnot(identical(sort(unique(single$g)), c('a','b')))
 value <- tryCatch(f(step), error = identity)
 if(inherits(value, 'error')) {
   failed <- c(failed, name)
   cat(name, ': ', conditionMessage(value), '\n', sep = '')
 } else check(value, name)
 stopifnot(identical(src, before))
}
ordinary_union <- tryCatch(collect(union_all(step, step)), error = identity)
cat('Ordinary union_all(step, step): ',
 if(inherits(ordinary_union, 'error')) conditionMessage(ordinary_union) else
   paste(nrow(ordinary_union), 'rows'), '\n', sep = '')
stopifnot(identical(src, before))
cat('Margin failures', length(failed),
    '; materialized/recached/single-set controls 12 passed; source unchanged\n')
if(assert) stopifnot(length(failed) == 0L)
