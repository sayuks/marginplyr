args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) normalizePath(args[[1L]]) else getwd()
suppressPackageStartupMessages(library(dplyr))
devtools::load_all(repo, quiet = TRUE)
d<-tibble(g=c('a','a','b'),x=c(1,2,30),y=c(2,5,9)); failures<-list();total<-0L
cap<-function(e) tryCatch(withCallingHandlers(e,warning=function(c)invokeRestart('muffleWarning')),error=identity)
fns<-alist(~sum(.x),~sum(.x)+n(),function(v)sum(v)+n(),(function(k)function(v)sum(v)+k)(n()),(function(k)function(v)sum(v)+k)(sum(x)),~sum(pick(x)),~sum(across(c(x,y),sum)),~sum(.x)+ncol(pick(everything())),~sum(.x)+length(cur_column()),~tibble(a=sum(.x),b=n()),(function(k)function(v)tibble(a=sum(v),b=k))(n()),list(s=~sum(.x),z=~n()),list(s=~tibble(a=sum(.x))),list(s=(function(k)function(v)sum(v)+k)(n())))
wrappers<-list(identity=function(e)e,parenthesized=function(e)call('(',e),named=function(e)e,identity_call=function(e)call('identity',e),block=function(e)call('{',e),function_body=function(e)as.call(list(call('function',pairlist(),e))),eval_quote=function(e)call('eval',call('quote',e)))
for(f in fns)for(w in names(wrappers))for(unpack in list(FALSE,TRUE,'{outer}_{inner}'))for(prior in c(FALSE,TRUE)){
  e<-rlang::call2('across',quote(c(x,y)),f,.unpack=unpack);e<-wrappers[[w]](e);dots<-if(prior)rlang::quos(x=sum(x),y=sum(y))else list();dots<-c(dots,list(rlang::new_quosure(e,globalenv())));if(w=='named')names(dots)[length(dots)]<-'result';total<-total+1L
  exp<-cap(rlang::eval_tidy(rlang::expr(summarise(!!d,!!!dots,.by=g))));act<-cap(rlang::eval_tidy(rlang::expr(summarize_with_margins(!!d,!!!dots,.grouping=grouping_set(g),.margin_label=NULL))));
  if(inherits(exp,'error')&&inherits(act,'error'))next
  if(!isTRUE(all.equal(exp,act))){failures[[length(failures)+1L]]<-list(expr=e,prior=prior,expected=exp,actual=act);cat('FAIL',deparse1(e),'prior',prior,'\n');print(exp);print(act)}
}
cat('TOTAL',total,'FAILURES',length(failures),'\n')
if (length(failures)) stop('Differential mismatch', call. = FALSE)
