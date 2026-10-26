# Dated investigation evidence: 2026-10-26, source bfcdaf76a36d102132cd039c68ba8bc05a4fd465.
args <- commandArgs(trailingOnly=TRUE)
repo_root <- if(length(args)) args[[1L]] else '.'
pkgload::load_all(repo_root,quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
x <- tibble(a=c('x','y'),b=c(1,1),z=c(FALSE,FALSE))
forms <- list(c=quote(base::c(a)),minus_unary=quote(base::`-`(a)),minus_binary=quote(base::`-`(a,0L)),range=quote(base::`:`(a,2L)),divide=quote(base::`/`(a,1L)),and=quote(base::`&`(a,1L)),or=quote(base::`|`(a,1L)),not=quote(base::`!`(a)))
get_names<-function(expr) {
  e<-new.env(parent=globalenv())
  makeActiveBinding('a',function() if(is.numeric(tidyselect::peek_data()$a)) 1L else 2L,e)
  q<-rlang::new_quosure(expr,e)
  exp<-tryCatch(names(tidyselect::eval_select(q,x)),error=function(e) paste('ERROR:',class(e)[[1L]]))
  obs<-tryCatch(inspect_grouping(x,.by=!!q,.format='list')$fixed[[1L]],error=function(e) paste('ERROR:',class(e)[[1L]]))
  list(expected=exp,actual=obs)
}
mismatches <- 0L
for(n in names(forms)) {
  e<-forms[[n]]
  for(head in c('qualified','parenthesized')) {
    if(head=='parenthesized') e<-rlang::call2(rlang::call2('(',e[[1L]]),!!!as.list(e)[-1L])
    r<-get_names(e)
    mismatches <- mismatches + as.integer(!identical(r$expected, r$actual))
    cat(n,head,'| expected:',paste(r$expected,collapse=','),'| actual:',paste(r$actual,collapse=','),'\n')
  }
}

stopifnot(mismatches == 8L)
cat('Observed eight wrong selections and eight matching refusals: PASS\n')
