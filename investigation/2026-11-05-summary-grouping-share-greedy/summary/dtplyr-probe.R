Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all(quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
d<-dtplyr::lazy_dt(tibble(g=rep(c('a','b'),each=2),x=1:4,y=11:14))
exprs<-alist(
 across(x:y,sum),across(x:y,list(s=sum,m=mean)),across(c(z=x),sum),across(c(z=x,w=y),list(s=sum,m=mean)),across(x:y,~sum(.x)),across(x:y,~sum(.x),.names='{.col}_{.fn}'),across(c(z=x),sum,.names='{.col}_{seq_along(.col)}'),
 sum(pick(x:y)),nrow(pick(x:y)),as.list(pick(x:y)),
 sum(if_any(x:y,~.x>2)),sum(if_all(x:y,~.x>2)),
 (function()sum(pick(x:y)))(),(function()sum(across(x:y,sum)))(),
 (function(...)sum(pick(..1)))(x),
 (function(...)sum(across(..1,sum)))(x),
 eval(quote(sum(pick(x:y)))),evalq(sum(pick(x:y))),
 (across(x:y,sum)),{across(x:y,sum)},identity(across(x:y,sum)),
 sum(x),sum(.data$x),sum(.data[['x']]),get('x')[[1L]],
 list(sum(x)),list(c(sum(x),mean(x))),tibble(z=sum(x)),data.frame(z=sum(x))
)
count<-0L
for(i in seq_along(exprs))for(named in c(FALSE,TRUE))for(group in c(FALSE,TRUE)){
 dots<-list(rlang::new_quosure(exprs[[i]],globalenv()));if(named)names(dots)<-'answer'
 a<-tryCatch(suppressWarnings(collect(summarize(d,!!!dots,.by=all_of(if(group)'g'else character())))),error=identity)
 b<-tryCatch(suppressWarnings(collect(summarize_with_margins(d,!!!dots,.by=all_of(if(group)'g'else character())))),error=identity)
 eq<-if(inherits(a,'error')||inherits(b,'error'))inherits(a,'error')&&inherits(b,'error')else isTRUE(all.equal(a,b,check.attributes=FALSE))
 count<-count+1L
 if(!eq){cat('\nDIFF',i,deparse1(exprs[[i]]),'named',named,'group',group,'\n');print(a);print(b)}
}
cat('completed',count,'comparisons\n')
