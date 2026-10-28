pkgload::load_all('.',quiet=TRUE)
options(lifecycle_verbosity='quiet')
data<-tibble::tibble(a=c('x','y'), b=c('q','r'), value=1:2)
consumers<-list(inspect=function(s)inspect_grouping(data,.grouping=s),
 summarize=function(s)summarize_with_margins(data,n=dplyr::n(),.grouping=s),
 expand=function(s)expand_with_margins(data,.grouping=s),
 nest=function(s)nest_with_margins(data,.grouping=s),
 nest_by=function(s)nest_by_with_margins(data,.grouping=s))
constructors<-c('grouping_set','grouping_sets','rollup','cube','grouping_spec')
checked<-0L
wrap<-function(x,n=1L){for(i in seq_len(n))x<-rlang::call2('(',x);x}
# All statically recognized constructor spellings execute caller masking once.
for(name in constructors)for(ret in c('spec','selection'))for(spelling in c('bare','paren-head','paren-whole','both')) {
 state<-rlang::env(reads=0L,arguments=0L)
 e<-rlang::env(rlang::current_env())
 fn<-if(ret=='spec') function(...) { state$reads<-state$reads+1L; marginplyr::grouping_set(a) } else function(...) {state$reads<-state$reads+1L;'a'}
 makeActiveBinding(name,function(){state$arguments<-state$arguments+1L;fn},e)
 head<-rlang::sym(name);if(spelling%in%c('paren-head','both'))head<-wrap(head,3L)
 expr<-rlang::call2(head);if(spelling%in%c('paren-whole','both'))expr<-wrap(expr,3L)
 s<-eval(rlang::expr(marginplyr::grouping_sets(!!expr)),e)
 for(consume in consumers){state$reads<-state$arguments<-0L;actual<-consume(s);expected<-consume(grouping_sets(a));stopifnot(identical(actual,expected),state$reads==1L,state$arguments==1L);checked<-checked+1L}
}
# Computed heads are ordinary selections; spec results get the documented refusal.
for(name in constructors)for(which in c('get','identity','brace','dollar','subset')) {
 e<-rlang::env(rlang::current_env(), holder=list(fn=get(name)))
 head<-switch(which,get=rlang::call2('get',name),identity=rlang::call2('identity',rlang::sym(name)),brace=rlang::call2('{',rlang::sym(name)),dollar=quote(holder$fn),subset=quote(holder[[1]]))
 expr<-rlang::call2(wrap(head,2L),quote(a))
 s<-eval(rlang::expr(grouping_sets(!!expr)),e)
 for(consume in consumers){err<-tryCatch(consume(s),error=identity);stopifnot(inherits(err,'marginplyr_error'),grepl('nested position recognizes',conditionMessage(err),fixed=TRUE));checked<-checked+1L}
}
# Dynamic dots, injection, embracing and nested quosures must keep source environments.
forward1<-function(...)grouping_sets(...)
forward2<-function(...)forward1(...)
embrace<-function(x)grouping_sets(({{x}}))
for(name in constructors)for(form in c('dot','dot2','inject','splice-quos','splice-exprs','embrace','nested-quosure')) {
 e<-rlang::env(rlang::current_env(),cols='a')
 payload<-rlang::call2(name,quote(all_of(cols)))
 q<-rlang::new_quosure(payload,e)
 s<-switch(form,dot=rlang::eval_tidy(rlang::expr(forward1(!!q))),dot2=rlang::eval_tidy(rlang::expr(forward2(!!q))),inject=rlang::inject(grouping_sets(!!q)),`splice-quos`=rlang::inject(grouping_sets(!!!list(q))),`splice-exprs`=eval(rlang::expr(grouping_sets(!!!list(payload))),e),embrace=rlang::eval_tidy(rlang::expr(embrace(!!q))),`nested-quosure`={outer<-rlang::env(rlang::current_env(),cols='b');qq<-rlang::new_quosure(wrap(q,3L),outer);rlang::inject(grouping_sets(!!qq))})
 expected<-eval(rlang::call2('grouping_sets',rlang::call2(name,quote(a))))
 for(consume in consumers){stopifnot(identical(consume(s),consume(expected)));checked<-checked+1L}
}
# delayed bindings are forced once, across operations; inactive names keep their captured environment.
for(ret in c('spec','selection')) {
 state<-rlang::env(reads=0L)
 e<-rlang::env(rlang::current_env())
 delayedAssign('bound',{state$reads<-state$reads+1L;if(ret=='spec')grouping_set(a)else'a'},eval.env=environment(),assign.env=e)
 s<-eval(quote(grouping_sets(bound)),e)
 for(consume in consumers){stopifnot(identical(consume(s),consume(grouping_sets(a))));checked<-checked+1L}
 stopifnot(state$reads==1L)
}
cat(checked,'adversarial public calls matched capture/masking contracts\n')
