Sys.setenv(TESTTHAT_PARALLEL='false'); pkgload::load_all('.',quiet=TRUE); library(dplyr)
run<-function(verb,ex,seq,named=FALSE){
 d<-data.frame(g=c('a','a','b'),x=1:3)
 counts<-c(unpack=0L,names=0L,fns=0L,cols=0L)
 uf<-function(){counts['unpack']<<-counts['unpack']+1L;seq[[min(counts['unpack'],length(seq))]]}
 nf<-function(){counts['names']<<-counts['names']+1L;paste0('outer',counts['names'],'_{.col}')}
 ff<-function(){counts['fns']<<-counts['fns']+1L; k<-counts['fns']; function(x) data.frame(z=sum(x)+k)}
 cf<-function(){counts['cols']<<-counts['cols']+1L;'x'}
 args<-list(quote(d),ex,.by=quote(g));if(named)names(args)[2]<-'out'
 a<-tryCatch(suppressWarnings(eval(as.call(c(list(quote(verb)),args)))),error=function(e)list(error=class(e)[1],message=conditionMessage(e)))
 list(value=a,counts=counts)
}
forms<-c('across(x,ff(),.unpack=uf(),.names=nf())','across(x,ff(),.unpack=uf())','across(cf(),ff(),.unpack=uf(),.names=nf())','(across(cf(),ff(),.unpack=uf(),.names=nf()))','identity(across(cf(),ff(),.unpack=uf(),.names=nf()))')
for(ex in forms)for(seq in list(list(FALSE,TRUE),list(TRUE,FALSE),list(TRUE,'{outer}..{inner}'),list('{outer}_{inner}',TRUE),list(FALSE,'{outer}_{inner}')))for(named in c(FALSE,TRUE)){
 a<-run(dplyr::summarize,parse(text=ex)[[1]],seq,named);b<-run(summarize_with_margins,parse(text=ex)[[1]],seq,named)
 if(!isTRUE(all.equal(a,b))){cat('\nDIFF',ex,paste(seq,collapse=','),named,'\n');print(a);print(b)}
}
