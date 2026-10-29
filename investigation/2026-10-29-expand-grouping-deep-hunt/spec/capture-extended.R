args <- commandArgs(TRUE)
pkgload::load_all(if (length(args)) args[[1]] else '.', quiet=TRUE)
options(lifecycle_verbosity='quiet')
data<-data.frame(alpha=1:2,beta=c('x','y'),gamma=factor(c('a','b')))
constructors<-c('grouping_set','grouping_sets','rollup','cube','grouping_spec')
wrap<-function(x,n=1L){for(i in seq_len(n))x<-rlang::call2('(',x);x}
obs<-function(fn)tryCatch(suppressWarnings(fn()),error=identity)
consumers<-list(inspect=function(s)inspect_grouping(data,.grouping=s,.duplicates='keep',.format='list'),expand=function(s)expand_with_margins(data,.grouping=s,.duplicates='keep',.margin_label=NULL),summary=function(s)summarize_with_margins(data,n=dplyr::n(),.grouping=s,.duplicates='keep',.margin_label=NULL))
values<-list(quote(alpha),expression(alpha),pairlist(alpha=1L),matrix(1L),array(1L,c(1L,1L,1L)),1+2i,as.raw(1),as.Date('2026-10-29'),factor('alpha'),structure(1L,class='foo'),rlang::new_quosure(quote(all_of(cols)),rlang::env(cols='beta')),rlang::new_quosure(wrap(rlang::new_quosure(quote(all_of(cols)),rlang::env(cols='gamma')),5L),rlang::env(cols='alpha')),function(x)is.factor(x))
count<-0L; errors<-0L; success<-0L
for(name in constructors)for(value in values)for(depth in c(0L,1L,7L)){
 env<-rlang::env(environment(),cols='alpha',calls=0L)
 env$choose<-function(...)value
 assign(name,function(...){env$calls<-env$calls+1L;value},env)
 expr<-rlang::call2(wrap(rlang::sym(name),depth))
 expr<-wrap(expr,depth)
 s<-eval(rlang::call2('grouping_sets',expr,.ns='marginplyr'),env)
 control<-eval(quote(marginplyr::grouping_sets(choose())),env)
 for(consume in consumers){
  env$calls<-0L
  actual<-obs(function()consume(s));expected<-obs(function()consume(control));count<-count+1L
  stopifnot(env$calls==1L)
  if(inherits(expected,'error')){stopifnot(inherits(actual,'error'),identical(class(actual),class(expected)));errors<-errors+1L}else{stopifnot(identical(actual,expected));success<-success+1L}
 }
}
cat('PASS extended constructor-result capture:',count,'consumer comparisons;',success,'matching successes;',errors,'matching condition classes; every constructor called once\n')
