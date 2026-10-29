pkgload::load_all(quiet=TRUE)
data <- data.frame(region=c('a','b'), amount=1:2)
selector <- function(...) {
  proxy <- tidyselect::peek_data()
  names(proxy)[vapply(proxy,is.character,logical(1))]
}
env <- rlang::env(rlang::global_env())
env[['::']] <- function(package,name) selector
selection <- rlang::new_quosure(quote(dplyr::all_of('amount')),env)
expected <- names(tidyselect::eval_select(selection,data))
actual <- inspect_grouping(data,.by=!!selection,.format='list')$fixed[[1L]]
print(expected);print(actual)
stopifnot(identical(expected,'region'))
cat('independent_typed_namespace_match:',identical(actual,expected),'\n')
if('--assert' %in% commandArgs(TRUE)) stopifnot(identical(actual,expected))
