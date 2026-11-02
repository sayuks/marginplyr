# Independently authored public-seam confirmation of empty-dots forwarding.
pkgload::load_all('.',quiet=TRUE)
d <- tibble::tibble(g=c('a','b'),value=1:2)
observe <- function(expr) tryCatch(force(expr),error=identity)
wrapper <- function(...) {
  list(
    direct=tidyselect::eval_select(rlang::quo(all_of(c(...))),d),
    inspect_by=observe(inspect_grouping(d,.by=all_of(c(...)))),
    inspect_spec=observe(inspect_grouping(d,.grouping=grouping_set(all_of(c(...))))),
    nest_by_key=observe(nest_with_margins(d,.by=all_of(c(...)))),
    nest_spec=observe(nest_with_margins(d,.grouping=grouping_set(all_of(c(...))))),
    rowwise_by=observe(nest_by_with_margins(d,.by=all_of(c(...)))),
    rowwise_spec=observe(nest_by_with_margins(d,.grouping=grouping_set(all_of(c(...)))))
  )
}
empty <- wrapper()
full <- wrapper('g')
stopifnot(identical(unname(empty$direct),integer()),identical(unname(full$direct),1L))
for (name in setdiff(names(empty),'direct')) {
  cat(name,':',class(empty[[name]])[[1L]],conditionMessage(empty[[name]]),'\n')
  stopifnot(inherits(empty[[name]],'getvarError'),!inherits(full[[name]],'error'))
}
if ('--assert' %in% commandArgs(TRUE)) stopifnot(!inherits(empty$inspect_by,'error'))
cat('Six empty-dots failures independently confirmed; all populated controls passed.\n')
