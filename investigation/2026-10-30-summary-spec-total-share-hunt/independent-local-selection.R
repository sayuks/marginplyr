pkgload::load_all('.',quiet=TRUE)
options(dplyr.summarise.inform=FALSE)
data<-tibble::tibble(g=c('a','a','b'),x=1:3,y=c(5,6,7))
cols<-'y'
cat('across ordinary\n');print(dplyr::summarise(data,(function(cols)dplyr::across(dplyr::all_of(cols),sum))('x'),.by=g))
cat('across margin\n');print(summarize_with_margins(data,(function(cols)dplyr::across(dplyr::all_of(cols),sum))('x'),.by=g))
for(form in c('across','pick','if_any','if_all')) {
 dots<-switch(form,across=rlang::exprs((function(cols)dplyr::across(dplyr::all_of(cols),sum))('x')),pick=rlang::exprs(value=(function(cols)sum(unlist(dplyr::pick(dplyr::all_of(cols)))))('x')),if_any=rlang::exprs(value=(function(cols)any(dplyr::if_any(dplyr::all_of(cols),~.x>3)))('x')),if_all=rlang::exprs(value=(function(cols)any(dplyr::if_all(dplyr::all_of(cols),~.x>3)))('x')))
 ordinary<-rlang::eval_tidy(rlang::expr(dplyr::summarise(data,!!!dots,.by=g)))
 margin<-rlang::eval_tidy(rlang::expr(summarize_with_margins(data,!!!dots,.by=g)))
 cat(form,'equal:',isTRUE(all.equal(ordinary,margin)),'\n');print(ordinary);print(margin)
 stopifnot(!isTRUE(all.equal(ordinary,margin)))
}
rm(cols)
cat('No outer cols binding\n')
condition<-tryCatch(summarize_with_margins(data,(function(cols)dplyr::across(dplyr::all_of(cols),sum))('x'),.by=g),error=identity)
stopifnot(inherits(condition,'error'));cat(conditionMessage(condition),'\n')
