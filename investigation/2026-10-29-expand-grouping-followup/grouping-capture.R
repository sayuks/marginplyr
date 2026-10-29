pkgload::load_all('.',quiet=TRUE)
options(lifecycle_verbosity='quiet')
data<-tibble::tibble(alpha=c('x','y'),beta=1:2,gamma=c(TRUE,FALSE))
# Each consumer receives the same immutable captured specification.
consumers<-list(inspect=function(s)inspect_grouping(data,.grouping=s,.duplicates='keep',.format='list'),expand=function(s)expand_with_margins(data,.grouping=s,.duplicates='keep',.margin_label=NULL),summarize=function(s)summarize_with_margins(data,n=dplyr::n(),.grouping=s,.duplicates='keep',.margin_label=NULL))
constructors<-c('grouping_set','grouping_sets','rollup','cube','grouping_spec')
# Each lexical environment carries different names; all injections must select the innermost one.
wrap<-function(x,n){for(i in seq_len(n))x<-rlang::call2('(',x);x}
forward1<-function(x)grouping_sets({{x}})
# jarl-ignore unnecessary_parentheses: Repeated pairs are capture syntax under test.
forward2<-function(x)grouping_sets(((({{x}}))))
forms<-c('direct','parens','head','both','splice','quosure','nested-quosure','forward','forward-parens')
count<-0L
for(constructor in constructors)for(depth in c(0L,1L,2L,5L,11L))for(form in forms) {
 e<-rlang::env(rlang::current_env(),cols='alpha')
 outer<-rlang::env(rlang::current_env(),cols='beta')
 head<-rlang::sym(constructor)
 if(form%in%c('head','both'))head<-wrap(head,depth)
 expr<-rlang::call2(head,quote(all_of(cols)))
 if(form%in%c('parens','both'))expr<-wrap(expr,depth)
 q<-rlang::new_quosure(expr,e)
 if(form=='nested-quosure') {
  for(i in seq_len(depth+1L))q<-rlang::new_quosure(wrap(q,1L),outer)
 }
 s<-switch(form,splice=rlang::inject(grouping_sets(!!!list(q))),quosure=rlang::inject(grouping_sets(!!q)),`nested-quosure`=rlang::inject(grouping_sets(!!q)),forward=rlang::eval_tidy(rlang::expr(forward1(!!q))),`forward-parens`=rlang::eval_tidy(rlang::expr(forward2(!!q))),eval(rlang::call2('grouping_sets',expr),e))
 expected<-eval(rlang::call2('grouping_sets',rlang::call2(constructor,quote(alpha))))
 for(consumer in consumers){stopifnot(identical(consumer(s),consumer(expected)));count<-count+1L}
}
# Constructor-shaped lexical functions may return ordinary tidyselect values.
values<-list(character(),c('alpha','gamma'),c(1L,3L),-2L,c(alpha='alpha'),c(alpha='gamma'),NULL,is.numeric,tidyselect::where(is.character),rlang::quo(alpha),quote(alpha),list('alpha'),NA_character_,1.25)
error_obs<-function(f)tryCatch(suppressWarnings(f()),error=function(e)e)
for(name in constructors)for(value in values)for(spelling in c('bare','head','whole','both')) {
 e<-rlang::env(rlang::current_env(),calls=0L,choose=function()value)
 fn<-function(...){e$calls<-e$calls+1L;value}
 assign(name,fn,e)
 head<-rlang::sym(name);if(spelling%in%c('head','both'))head<-wrap(head,3L)
 expr<-rlang::call2(head);if(spelling%in%c('whole','both'))expr<-wrap(expr,3L)
 s<-eval(rlang::call2('grouping_sets',expr,.ns='marginplyr'),e)
 control<-eval(quote(marginplyr::grouping_sets(choose())),e)
 for(consumer in consumers){e$calls<-0L;actual<-error_obs(function()consumer(s));expected<-error_obs(function()consumer(control));if(inherits(expected,'error'))stopifnot(inherits(actual,'error'),identical(class(expected),class(actual)))else stopifnot(identical(actual,expected));stopifnot(e$calls==1L);count<-count+1L}
}
cat('PASS capture and lexical-result matrix:',count,'consumer comparisons\n')
