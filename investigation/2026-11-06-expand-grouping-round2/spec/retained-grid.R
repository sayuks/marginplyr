Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all(Sys.getenv('MARGINPLYR_HUNT_SOURCE', '.'), quiet=TRUE)
data <- tibble::tibble(a='alpha',b=3L,c=TRUE)
constructors <- c('grouping_set','grouping_sets','rollup','cube','grouping_spec')
forms <- list(
 bare=function(x)x,
 syntactic=function(x)rlang::call2('(',x),
 qualified=function(x)rlang::call2(quote(base::`(`),x),
 private=function(x)rlang::call2(quote(base:::`(`),x),
 identity=function(x)rlang::call2(quote(base::identity),x),
 syntactic_deep=function(x)rlang::call2('(',rlang::call2('(',x)),
 qualified_deep=function(x)rlang::call2(quote(base::`(`),rlang::call2(quote(base::`(`),x)),
 qualified_head=function(x)rlang::call2(quote((base::`(`)),x)
)
r <- list()
for(parent in constructors) for(child in constructors) for(form in names(forms)) for(injected in c(FALSE,TRUE)) {
 count <- 0L
 shadow <- function(...) { count <<- count+1L; 'b' }
 env <- rlang::env(globalenv(), !!child := shadow)
 expr <- rlang::call2(child,quote(stop('must not force ignored arguments')))
 if(injected)expr <- rlang::new_quosure(expr,env)
 expr <- forms[[form]](expr)
 # A direct typed selection is the behavior oracle; preflight may inspect the
 # constructor spelling but must retain the very answer this evaluation gets.
 ref <- names(tidyselect::eval_select(if(rlang::is_quosure(expr)) expr else rlang::new_quosure(expr,env),data))
 stopifnot(identical(ref,'b'),count==1L)
 count <- 0L
 spec <- rlang::eval_bare(rlang::call2(parent,expr,.ns='marginplyr'),env)
 actual <- inspect_grouping(data,.grouping=spec,.format='list')$included[[1L]]
 r[[length(r)+1L]] <- data.frame(parent,child,form,injected,same=identical(ref,actual),calls=count)
}
r <- do.call(rbind,r)
write.csv(r,file.path(Sys.getenv('MARGINPLYR_HUNT_OUTPUT', tempdir()), 'retained-grid.csv'),row.names=FALSE)
print(with(r,table(same,calls)))
stopifnot(all(r$same),all(r$calls==1L))
