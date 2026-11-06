Sys.setenv(TESTTHAT_PARALLEL='false'); pkgload::load_all('.',quiet=TRUE); library(dplyr)
run <- function(verb, expr, mode, named=FALSE) {
 d<-data.frame(g=c('a','a','b'),x=1:3,y=11:13)
 calls<-0L; names_calls<-0L; unpack_calls<-0L; select_calls<-0L
 factory<-function() {calls<<-calls+1L; offset<-calls; function(x) data.frame(v=sum(x)+offset)}
 uf<-function(){unpack_calls<<-unpack_calls+1L; mode}
 nf<-function(){names_calls<<-names_calls+1L; '{.col}_{.fn}'}
 sf<-function(){select_calls<<-select_calls+1L; 'x'}
 local_unpack<-mode
 eval_run<-function(...) {
  args<-list(quote(d),expr, .by=quote(g)); if(named) names(args)[2]<-'out'
  tryCatch(suppressWarnings(eval(as.call(c(list(quote(verb)),args)),envir=environment())),error=function(e)paste(class(e)[1],conditionMessage(e)))
 }
 value<-eval_run(factory())
 list(value=value,calls=c(calls,names_calls,unpack_calls,select_calls))
}
forms<-c('across(x,factory(),.unpack=local_unpack)','across(x,..1,.unpack=local_unpack)','across(x,...,.unpack=local_unpack)', 'across(x,factory(),.unpack=uf())','across(x,..1,.unpack=uf())','across(sf(),factory(),.unpack=local_unpack)','across(x,factory(),.unpack=local_unpack,.names=nf())','(across(x,factory(),.unpack=local_unpack,.names=nf()))','identity(across(x,factory(),.unpack=local_unpack,.names=nf()))','across(x,factory(),.unpack=!!local_unpack,.names=nf())')
for(f in forms) for(mode in list(FALSE,TRUE,'{outer}_{inner}')) for(named in c(FALSE,TRUE)) {
 expr<-parse(text=f)[[1]]
 a<-run(dplyr::summarize,expr,mode,named); b<-run(summarize_with_margins,expr,mode,named)
 if(!isTRUE(all.equal(a,b))) {cat('\nDIFF',f,'mode',mode,'named',named,'\n');print(a);print(b)}
}
