Sys.setenv(TESTTHAT_PARALLEL='false')
args <- commandArgs(TRUE)
root <- normalizePath(if(length(args)) args[[1]] else '.')
pkgload::load_all(root,quiet=TRUE)
condition <- function(expr) tryCatch(force(expr),error=identity)
# Ordinary nesting reproduces this class's grouping recursion even without a
# Margin operation or an omitted set.
s <- tibble::tibble(g=survival::Surv(1,1),rid=1L)
native <- condition(tidyr::nest(s,.by=g))
margin <- condition(nest_with_margins(s,.grouping=grouping_set(g),.margin_label=NULL))
expanded <- expand_with_margins(s,.grouping=rollup(g),.margin_label=NULL)
stopifnot(inherits(native,'expressionStackOverflowError'),inherits(margin,'expressionStackOverflowError'),nrow(expanded)==2L)
cat('Surv: ordinary tidyr grouping and included-only Margin nesting recurse; expansion succeeds.\n')
# Existing dimension normalization works; an additional unsliced AsIs POSIXlt
# payload still fails ordinary branch union under the same vctrs rule.
p <- tibble::tibble(g=I(as.POSIXlt('2020-01-01',tz='UTC')),rid=1L)
for(verb in list(nest_with_margins,nest_by_with_margins)) for(keep in c(FALSE,TRUE)) {
 z <- verb(p,.grouping=rollup(g),.margin_label=NULL,.keep=keep,.id='sid')
 stopifnot(identical(z$sid,1:2),all(vapply(z$data,function(cell)identical(cell$rid,1L),logical(1))))
}
p$p <- p$g
native <- condition(dplyr::union_all(p,p))
margin <- condition(nest_with_margins(p,.grouping=rollup(g),.margin_label=NULL))
stopifnot(inherits(native,'vctrs_error_cast'),inherits(margin,'vctrs_error_cast'))
cat('AsIs POSIXlt: four dimension/keep controls pass; payload union has the same native cast refusal.\n')
# posterior caches data in mutable metadata under ordinary vctrs grouping.
r <- tibble::tibble(g=posterior::rvar(matrix(c(1,1,2,2),2L)),rid=1:2)
before <- serialize(r,NULL)
groups <- vctrs::vec_group_loc(r['g'])
after <- serialize(r,NULL)
stopifnot(nrow(groups)==2L,!identical(before,after))
for(verb in list(nest_with_margins,nest_by_with_margins)) {
 z <- verb(r,.grouping=grouping_set(g),.keep=TRUE)
 stopifnot(nrow(z)==2L,identical(sort(unlist(lapply(z$data,`[[`,'rid'))),1:2))
}
cat('rvar: ordinary vctrs grouping changes serialized metadata; two included-only Margin controls retain source membership.\n')
# Fault calibration: an oracle that accepted a lost row would miss this hunt's
# core contract. All three corruptions must be rejected.
partition <- function(cells) identical(sort(as.integer(unlist(cells))),1:2)
stopifnot(partition(list(1L,2L)),!partition(list(1L)),!partition(list(1L,1L)),!partition(list(1L,2L,2L)))
cat('Membership calibration rejected missing, duplicated and extra source rows.\n')
