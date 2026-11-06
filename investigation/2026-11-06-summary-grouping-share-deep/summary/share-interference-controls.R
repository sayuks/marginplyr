# Standalone dated differential probe; run from the investigated source root.
Sys.setenv(TESTTHAT_PARALLEL='false'); pkgload::load_all('.',quiet=TRUE); library(dplyr)
run <- function(ex,share,mode,named=FALSE,first=FALSE) {
 d<-tibble(g=c('a','a','b'),x=1:3,y=11:13,z=21:23)
 calls<-c(fns=0L,names=0L,unpack=0L,select=0L)
 factory<-function() {calls[['fns']]<<-calls[['fns']]+1L; k<-calls[['fns']]; function(x)tibble(v=sum(x)+k)}
 uf<-function(){calls[['unpack']]<<-calls[['unpack']]+1L;mode}
 nf<-function(){calls[['names']]<<-calls[['names']]+1L;'{.col}_{.fn}'}
 sf<-function(){calls[['select']]<<-calls[['select']]+1L;'x'}
 local_unpack<-mode
 eval_run<-function(...) {
  args<-list(quote(d),total=quote(sum(z)),ex,.grouping=quote(rollup(g)),.id='sid')
  if(first)args<-c(args[1],args[3],args[2],args[-(1:3)])
  if(named)names(args)[[if(first)2L else 3L]]<-'out'
  if(share)args<-append(args,list(share=quote(share_of_total(total))))
  value<-tryCatch(suppressWarnings(eval(as.call(c(list(quote(summarize_with_margins)),args)))),error=function(e)structure(list(message=conditionMessage(e),class=class(e)[1]),class='caught'))
  if(is.data.frame(value)&&share)value$share<-NULL
  value
 }
 value<-eval_run(factory())
 list(value=value,calls=calls)
}
forms<-c('across(x,factory(),.unpack=local_unpack)', 'across(x,..1,.unpack=local_unpack)', 'across(x,...,.unpack=local_unpack)', 'across(x,factory(),.unpack=uf())', 'across(sf(),factory(),.unpack=local_unpack)', 'across(x,factory(),.unpack=local_unpack,.names=nf())', '(across(x,factory(),.unpack=local_unpack,.names=nf()))', 'identity(across(x,factory(),.unpack=local_unpack,.names=nf()))', 'across(x,factory(),.unpack=!!local_unpack,.names=nf())','pick(x,y)','tibble(q=sum(pick(x)[[1]]))','nrow(pick(x,y))')
nd<-0L;np<-0L
for(f in forms)for(mode in list(FALSE,TRUE,'{outer}_{inner}'))for(named in c(FALSE,TRUE))for(first in c(FALSE,TRUE)){
 ex<-parse(text=f)[[1]]
 a<-run(ex,FALSE,mode,named,first);b<-run(ex,TRUE,mode,named,first);np<-np+1L
 if(inherits(a$value,'caught')&&inherits(b$value,'caught'))next
 if(!isTRUE(all.equal(a,b))){nd<-nd+1L;cat('DIFF',f,'mode',mode,'named',named,'first',first,'\n');print(a);print(b)}
}
cat('INTERFERENCE',np,'DIFFS',nd,'\n')
stopifnot(nd == 0L)
