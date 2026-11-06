Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all(Sys.getenv('MARGINPLYR_HUNT_SOURCE', '.'), quiet = TRUE)
dat <- tibble::tibble(a = 'alpha', b = 3L, c = TRUE)
a <- 'b'
q <- rlang::quo(a)
forms <- list(
  qualified = function(x) rlang::call2(quote(base::`(`), x),
  syntactic = function(x) rlang::call2('(', x),
  private = function(x) rlang::call2(rlang::parse_expr('base:::`(`'), x),
  parenthesized_head = function(x) rlang::call2(quote((base::`(`)), x),
  nested_head = function(x) rlang::call2(rlang::parse_expr('((base::`(`))'), x),
  private_head = function(x) rlang::call2(rlang::parse_expr('(base:::`(`)'), x),
  identity = function(x) rlang::call2(quote(base::identity), x),
  syntactic_head = function(x) rlang::call2(quote((`(`)), x)
)
results <- list()
for (outer in names(forms)) for (inner in c('none', names(forms))) {
 e <- if(inner=='none') q else forms[[inner]](q)
 e <- forms[[outer]](e)
 ref <- tryCatch(names(tidyselect::eval_select(rlang::new_quosure(e, globalenv()), dat)), error = function(c) paste('ERROR',conditionMessage(c)))
 for (con in c('grouping_set','grouping_sets','rollup','cube','grouping_spec')) {
  sp <- eval(rlang::call2(con, e))
  got <- tryCatch(inspect_grouping(dat, .grouping=sp, .format='list')$included[[1]], error = function(c) paste('ERROR',conditionMessage(c)))
  results[[length(results)+1L]] <- data.frame(outer,inner,con,expected=paste(ref,collapse=','),actual=paste(got,collapse=','),same=identical(ref,got))
 }
}
r <- do.call(rbind,results)
write.csv(r, file.path(Sys.getenv('MARGINPLYR_HUNT_OUTPUT', tempdir()), 'forward-grid.csv'), row.names=FALSE)
print(table(r$same)); print(r[!r$same,], row.names=FALSE)
stopifnot(all(r$same))
