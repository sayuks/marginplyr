pkgload::load_all('.',quiet=TRUE)
options(lifecycle_verbosity='quiet')
data<-tibble::tibble(a=c('x','y'),b=c('q','r'),s=c('column','other'),value=1:2)
consumers<-list(function(x)inspect_grouping(data,.grouping=x),function(x)summarize_with_margins(data,n=dplyr::n(),.grouping=x),function(x)expand_with_margins(data,.grouping=x),function(x)nest_with_margins(data,.grouping=x),function(x)nest_by_with_margins(data,.grouping=x))
checked<-0L
constructors<-c('grouping_set','grouping_sets','rollup','cube','grouping_spec')
for(name in constructors)for(kind in c('column','spec','failure')) {
 state<-rlang::env(reads=0L)
 marker<-rlang::error_cnd('binding_failure',message='binding sentinel')
 e<-rlang::env(rlang::current_env())
 makeActiveBinding('s',function(){state$reads<-state$reads+1L;switch(kind,column='a',spec=grouping_set(a),failure=stop(marker))},e)
 s<-eval(rlang::call2(name,quote(s)),e)
 for(consume in consumers) {
  state$reads<-0L
  actual<-tryCatch(consume(s),error=identity)
  should_ambiguous<-kind=='spec'&&name!='grouping_set'
  if(should_ambiguous)stopifnot(inherits(actual,'marginplyr_error'),grepl('both a column',conditionMessage(actual),fixed=TRUE))
  else {expected<-consume(eval(rlang::call2(name,quote(tidyselect::all_of('s')))));stopifnot(identical(actual,expected))}
  # set admits no specification so does not read its binding; all other positions read once.
  stopifnot(state$reads==if(name=='grouping_set')0L else 1L)
  checked<-checked+1L
 }
}
# Active noncolliding references switch once per operation and keep original captured environments.
state<-rlang::env(reads=0L)
e<-rlang::env(rlang::current_env())
makeActiveBinding('current',function(){state$reads<-state$reads+1L;if(state$reads%%2L)grouping_set(a)else grouping_set(b)},e)
s<-eval(quote(grouping_sets(current)),e)
for(consume in consumers) {
 state$reads<-0L
 stopifnot(identical(consume(s),consume(grouping_sets(a))),state$reads==1L)
 stopifnot(identical(consume(s),consume(grouping_sets(b))),state$reads==2L)
 checked<-checked+2L
}
# Caller-defined constructor warnings/errors remain the same condition and occur once.
for(name in constructors)for(kind in c('warning','error')) {
 state<-rlang::env(reads=0L)
 marker<-if(kind=='error')rlang::error_cnd('constructor_failure',message='constructor sentinel') else rlang::warning_cnd('constructor_warning',message='constructor warning sentinel')
 e<-rlang::env(rlang::current_env())
 assign(name,function(){state$reads<-state$reads+1L;if(kind=='error')stop(marker);warning(marker);grouping_set(a)},e)
 s<-eval(rlang::expr(marginplyr::grouping_sets(!!rlang::call2(name))),e)
 for(consume in consumers) {
  state$reads<-0L;seen<-list()
  actual<-withCallingHandlers(tryCatch(consume(s),error=identity),warning=function(w){seen[[length(seen)+1L]]<<-w;invokeRestart('muffleWarning')})
  if(kind=='error')stopifnot(identical(actual,marker),length(seen)==0L) else stopifnot(identical(seen,list(marker)),identical(actual,consume(grouping_sets(a))))
  stopifnot(state$reads==1L)
  checked<-checked+1L
 }
}
cat(checked,'binding/context public calls passed\n')
