# Standalone dated differential probe; run from the investigated source root.
Sys.setenv(TESTTHAT_PARALLEL="false"); pkgload::load_all(".",quiet=TRUE); library(dplyr)
obs <- function(ex) tryCatch(withCallingHandlers(eval(ex),warning=function(w)invokeRestart("muffleWarning")),error=function(e)structure(list(message=conditionMessage(e),class=class(e)),class="caught"))
d<-tibble(g=c("a","b"),x=1:2,y=3:4)
exprs<-list(quote(across(c(x,y),sum)),quote((across(c(x,y),sum))),quote(identity(across(c(x,y),sum))),quote(pick(x,y)),quote(nrow(pick(x,y))),quote(tibble(z=sum(pick(x)[[1]]))),quote(data.frame(z=sum(pick(x)[[1]]))),quote((function(z)sum(z))(pick(x)[[1]])),quote({ k<-pick(x);sum(k$x)}),quote({ function()pick(x) }()),quote(across(x,~sum(.x),.names='{.col}_q')),quote(across(c(x,y),~tibble(v=sum(.x)),.unpack=TRUE)),quote(across(c(x,y),~tibble(v=sum(.x)),.unpack='{outer}.{inner}')))
pre<-list(quote(sum(x)),quote(sum(y)),quote(sum(x)),quote(tibble(x=sum(x))),quote(tibble(z=sum(x))),quote(NULL))
np<-0;nd<-0
for(p in seq_along(pre))for(q in seq_along(exprs))for(nm in c('', 'out')){
 parts<-list(quote(d),pre[[p]],exprs[[q]])
 names(parts)<-c('','','');if(p<=3) {names(parts)[2]<-c('x','y','z')[[p]]}
 names(parts)[3]<-nm
 a<-obs(as.call(c(list(quote(dplyr::summarize)),parts,list(.by=quote(g)))))
 b<-obs(as.call(c(list(quote(summarize_with_margins)),parts,list(.by=quote(g)))))
 np<-np+1
 if(inherits(a,'caught')&&inherits(b,'caught'))next
 if(!isTRUE(all.equal(a,b))){nd<-nd+1;cat('DIFF SEQ',p,q,nm,'\n');print(a);print(b)}
}
cat('SEQ',np,'DIFFS',nd,'\n')
stopifnot(nd == 0L)
