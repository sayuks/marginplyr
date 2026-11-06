# Standalone dated differential probe; run from the investigated source root.
Sys.setenv(TESTTHAT_PARALLEL='false'); pkgload::load_all('.',quiet=TRUE); library(dplyr)
obs<-function(ex)tryCatch(suppressWarnings(eval(ex)),error=function(e)structure(list(class=class(e)[1],message=conditionMessage(e)),class='caught'))
d<-tibble(g=c('a','b'),x=1:2,y=3:4)
fns<-list(quote(sum),quote(list(sum)),quote(list(a=sum)),quote(list(sum,a=mean)),quote(setNames(list(sum,mean),c('',''))),quote(setNames(list(sum,mean),c('a','a'))),quote(setNames(list(sum),NA_character_)),quote(list()),quote(NULL),quote(function(x)sum(x)),quote(~sum(.x)),quote(1),quote(list(NULL)),quote(identity(list(a=sum,b=mean))))
nms<-list(quote(NULL),quote('{.col}'),quote('{.fn}'),quote('{.col}_{.fn}'),quote(c('a','b')),quote(''),quote(NA_character_))
nd<-0;np<-0
for(f in fns)for(nm in nms)for(unp in list(FALSE,TRUE,'{outer}_{inner}'))for(shape in c('top','parens','named')){
 ex<-as.call(list(quote(across),quote(c(x,y)),f,.names=nm,.unpack=unp));if(shape=='parens')ex<-call('(',ex)
 args<-list(quote(d),ex,.by=quote(g));if(shape=='named')names(args)[2]<-'out'
 a<-obs(as.call(c(list(quote(dplyr::summarize)),args)));b<-obs(as.call(c(list(quote(summarize_with_margins)),args)));np<-np+1
 if(inherits(a,'caught')&&inherits(b,'caught'))next
 if(!isTRUE(all.equal(a,b))){nd<-nd+1;cat('DIFF',deparse(ex),'shape',shape,'\n');print(a);print(b)}
}
cat('ACROSS',np,'DIFFS',nd,'\n')
stopifnot(nd == 0L)
