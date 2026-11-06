Sys.setenv(TESTTHAT_PARALLEL='false'); pkgload::load_all('.',quiet=TRUE); library(dplyr)
obs<-function(expr)tryCatch(suppressWarnings(eval(expr)),error=function(e)list(error=class(e)[1],message=conditionMessage(e)))
d<-data.frame(g=c('a','b'),x=c(1L,2L))
vals<-list(quote(data.frame(q=1)),quote(data.frame(q=NA)),quote(data.frame(q=list(NULL))),quote(data.frame(q=I(list(NULL)))),quote(data.frame()),quote(tibble(.rows=1)),quote(data.frame(q=matrix(1:2,nrow=1))),quote(matrix(1:2,nrow=1)),quote(list(1)),quote(NULL),quote(NA),quote(1),quote(structure(NA,class='vctrs_unspecified')))
for(i in seq_along(vals)) for(j in seq_along(vals)) {
 ex<-substitute(if (x==1) A else B,list(A=vals[[i]],B=vals[[j]]))
 a<-obs(as.call(list(quote(dplyr::summarize),quote(d),ex,.by=quote(g))))
 b<-obs(as.call(list(quote(summarize_with_margins),quote(d),ex,.by=quote(g))))
 if(is.list(a)&&!is.null(a$error)&&is.list(b)&&!is.null(b$error))next
 if(!isTRUE(all.equal(a,b))) {cat('\nDIFF',i,j,deparse(ex),'\n');print(a);print(b)}
}
