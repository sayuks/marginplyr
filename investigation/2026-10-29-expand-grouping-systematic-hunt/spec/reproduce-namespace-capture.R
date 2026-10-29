args <- commandArgs(TRUE)
repo <- if(length(args) && args[[1L]] != '--assert') args[[1L]] else '.'
pkgload::load_all(repo, quiet=TRUE)
data <- tibble::tibble(region=c('east','west'), amount=1:2)
env <- rlang::env(globalenv(), reads=0L)
env[['::']] <- function(package, name) function(...) {
  env$reads <- env$reads + 1L
  if (env$reads == 1L) 'region' else 'amount'
}
q <- rlang::new_quosure(quote(marginplyr::rollup()),env)
actual_spec <- grouping_sets(!!q, grouping_set())
actual <- inspect_grouping(data, .grouping=actual_spec, .format='list')
actual_reads <- env$reads
control <- inspect_grouping(data, .grouping=grouping_sets(grouping_set(region), grouping_set()), .format='list')
cat('actual included: ', paste(actual$included, collapse=' | '), '\n',sep='')
cat('expected included: ', paste(control$included, collapse=' | '), '\n',sep='')
cat('constructor result reads: ',actual_reads,'\n',sep='')
if('--assert' %in% args) stopifnot(identical(actual$included, control$included), identical(actual_reads, 1L))
