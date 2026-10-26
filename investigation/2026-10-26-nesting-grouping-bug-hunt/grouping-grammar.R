# Dated investigation evidence, investigated 2026-10-26 at 6b91254016dbcce60cc2cc69071f01f69eb8084e.
# Run from the repository root, or pass its path as the first argument.
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1L]] else "."
pkgload::load_all(repo, quiet = TRUE)
ctors <- c('grouping_set','grouping_sets','rollup','cube','grouping_spec')
parents <- ctors; count <- 0L
parens <- function(x,n) {for(i in seq_len(n)) x <- rlang::call2('(',x);x}
for(parent in parents) for(child in ctors) for(collision in c(FALSE,TRUE)) {
  e <- rlang::env(globalenv()); e$s <- eval(rlang::call2(child,quote(a)),e)
  d <- if(collision)tibble::tibble(a=1,s=2) else tibble::tibble(a=1)
  admitted <- parent %in%c('grouping_sets','grouping_spec') || (parent%in%c('rollup','cube')&&child=='grouping_set')
  childcall <- rlang::call2(child,quote(a))
  forms <- list(bare=quote(s), parens1=parens(quote(s),1),parens2=parens(quote(s),2),call=childcall,call1=parens(childcall,1),call2=parens(childcall,2),head=rlang::call2(parens(rlang::sym(child),2),quote(a)),qualified=rlang::call2(rlang::parse_expr(paste0('marginplyr::',child)),quote(a)),object=e$s,object1=parens(e$s,1),object2=parens(e$s,2))
  for(form in names(forms)) {
    arg <- forms[[form]]; spec <- eval(rlang::call2(parent,arg),e)
    got <- tryCatch(inspect_grouping(d,.grouping=spec,.duplicates='keep',.format='list'),error=identity)
    byname <- form %in% c('bare','parens1','parens2')
    expected_ok <- if(collision&&byname) !admitted else admitted
    if(expected_ok!=!inherits(got,'error')) {print(list(parent=parent,child=child,collision=collision,form=form,expected_ok=expected_ok,got=got));stop('mismatch')}
    if(inherits(got,'error')) stopifnot(inherits(got,'marginplyr_error'))
    count <- count+1L
  }
}
cat('Passed grammar and ambiguity combinations:',count,'\n')
