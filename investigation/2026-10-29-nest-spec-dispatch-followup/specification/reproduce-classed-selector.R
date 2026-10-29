args <- commandArgs(TRUE)
root <- if (length(args) > 0L && !startsWith(args[[1L]], '--')) args[[1L]] else '.'
pkgload::load_all(root, quiet=TRUE)
input <- tibble::tibble(region=c('east','west'), amount=c(1L,1L))
selector <- structure(1L, class='type_selector')
vec_cast.integer.type_selector <- function(x,to,...) which(vapply(tidyselect::peek_data(),is.character,logical(1)))
vec_ptype2.integer.type_selector <- function(x,y,...) integer()
vec_ptype2.type_selector.integer <- function(x,y,...) integer()
expected <- names(tidyselect::eval_select(rlang::quo(!!selector),input))
fixed <- inspect_grouping(input,.by=!!selector,.format='list')$fixed[[1L]]
control <- inspect_grouping(input,.by=all_of(selector),.format='list')$fixed[[1L]]
cat('ordinary typed tidyselect:',paste(expected,collapse=','),'\n')
cat('injected selector fixed keys:',paste(fixed,collapse=','),'\n')
cat('all_of control fixed keys:',paste(control,collapse=','),'\n')
stopifnot(identical(expected,'region'),identical(control,expected))
if('--assert' %in% args) stopifnot(identical(fixed,expected))
