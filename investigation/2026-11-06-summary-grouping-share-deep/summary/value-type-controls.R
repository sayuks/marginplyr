# Standalone dated differential probe; run from the investigated source root.
Sys.setenv(TESTTHAT_PARALLEL='false'); pkgload::load_all('.',quiet=TRUE); library(dplyr)
obs <- function(ex) tryCatch(withCallingHandlers(eval(ex),warning=function(w)invokeRestart('muffleWarning')),error=function(e)structure(list(message=conditionMessage(e),class=class(e)),class='caught'))
d<-tibble(g=c('a','b'),x=1:2,y=3:4)
vals<-list(quote(data.frame(q=1)),quote(data.frame(q=NA)),quote(data.frame(q=I(list(NULL)))),quote(data.frame()),quote(tibble(.rows=1)),quote(data.frame(q=matrix(1:2,nrow=1))),quote(matrix(1:2,nrow=1)),quote(list(1)),quote(NULL),quote(NA),quote(1),quote(structure(NA,class='vctrs_unspecified')),quote(as.Date('2020-01-01')),quote(as.POSIXct('2020-01-01',tz='UTC')),quote(as.POSIXlt('2020-01-01',tz='UTC')),quote(as.difftime(2,units='secs')),quote(factor('a')),quote(ordered('a')),quote(tibble(q=1,r='a')),quote(tibble(r='a',q=1)),quote(tibble(q=1L)),quote(tibble(q=list(1))),quote(tibble(q=NA)),quote(tibble(r=1)),quote(tibble(q=as.Date('2020-01-01'))),quote(tibble(q=factor('a'))),quote(tibble(q=ordered('a'))),quote(tibble(q=tibble(a=1))),quote(tibble(q=matrix(1:2,nrow=1))),quote(tibble(q=structure(NA,class='vctrs_unspecified'))),quote(array(1:4,dim=c(1,2,2))),quote(as.raw(1)),quote(1i),quote(c(q=1)))
nd<-0; np<-0
for(i in seq_along(vals))for(j in seq_along(vals)){
 ex<-substitute(if(x==1) A else B,list(A=vals[[i]],B=vals[[j]]))
 a<-obs(as.call(list(quote(dplyr::summarize),quote(d),ex,.by=quote(g))))
 b<-obs(as.call(list(quote(summarize_with_margins),quote(d),ex,.by=quote(g))))
 np<-np+1
 if(inherits(a,'caught')&&inherits(b,'caught'))next
 if(!isTRUE(all.equal(a,b))){nd<-nd+1;cat('DIFF pair',i,j,'\n');print(a);print(b)}
}
cat('PAIRS',np,'DIFFS',nd,'\n')
stopifnot(nd == 0L)
