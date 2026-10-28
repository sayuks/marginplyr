# Compare masked constructor selections with ordinary calls returning the same value.
args<-commandArgs(trailingOnly=TRUE)
pkgload::load_all(if(length(args))args[[1L]]else getwd(),quiet=TRUE)
options(lifecycle_verbosity='quiet')
data<-data.frame(a=c('x','y'),b=1:2,c=c(TRUE,FALSE))
values<-list('a',c('a','b'),integer(),c(3L,1L),c(a=1L),logical(),NULL,is.numeric,tidyselect::where(is.numeric),quote(a),rlang::quo(a),expression(a),list('a'),TRUE,NA_integer_,NA_character_,1.2)
constructors<-c('grouping_set','grouping_sets','rollup','cube','grouping_spec')
parents<-constructors
checked<-0L
for(v in values)for(parent in parents)for(constructor in constructors){
 count<-0L
 env<-rlang::env(globalenv(), ordinary=function(...) {count<<-count+1L;v})
 env[[constructor]]<-env$ordinary
 make<-function(head) eval(rlang::call2(rlang::call2('::',quote(marginplyr),rlang::sym(parent)),rlang::call2(head)),env)
 control<-make('ordinary');masked<-make(constructor)
 expected<-tryCatch(inspect_grouping(data,.grouping=control,.duplicates='keep',.format='list'),error=identity)
 baseline_reads<-count;count<-0L
 observed<-tryCatch(inspect_grouping(data,.grouping=masked,.duplicates='keep',.format='list'),error=identity)
 masked_reads<-count;count<-0L
 if(inherits(expected,'error')&&inherits(observed,'error')) {
  # Context attributes may quote different call heads; compare leaf classes.
  ok<-identical(class(expected),class(observed))
 }else ok<-identical(expected,observed)
 if(!ok || masked_reads!=1L){cat('DIFF',parent,constructor,'value class',class(v),'reads',baseline_reads,masked_reads,'\n');print(expected);print(observed);stop('retained constructor value disagrees')}
 checked<-checked+1L
}
cat('PASS constructor-result differential:',checked,'comparisons\n')
