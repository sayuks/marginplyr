Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all(quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
d<-tibble(g=rep(c('a','b'),each=2),x=1:4,y=11:14)
names_specs<-c('{.col}','{.col}_{.fn}','{.col}_{seq_along(.col)}','{.col}_{sum(x)}','{.col}_{length(x)}','{.col}_{n()}','{.col}_{cur_column()}','{.col}_{length(.data$x)}','{.col}_{length(.env$d$x)}','{.col}_{length(g)}','{.col}_{.col == "x"}','{.col}_{length(.col)}','{.col}_{.fn == "sum"}')
fns<-list(sum, list(sum=sum,mean=mean),~sum(.x),~tibble(z=sum(.x),w=mean(.x)))
count<-0L
for(ns in names_specs) for(fn in fns) for(unpack in list(FALSE,TRUE,'{outer}_{inner}')) for(named in c(FALSE,TRUE)) {
 e<-rlang::expr(across(x:y,!!fn,.names=!!ns,.unpack=!!unpack)); q<-rlang::new_quosure(e,globalenv())
 dots<-list(q);if(named)names(dots)<-'out'
 a<-tryCatch(suppressWarnings(summarize(d,!!!dots,.by=g)),error=identity)
 b<-tryCatch(suppressWarnings(summarize_with_margins(d,!!!dots,.by=g)),error=identity)
 eq<-if(inherits(a,'error')||inherits(b,'error'))inherits(a,'error')&&inherits(b,'error')else isTRUE(all.equal(a,b,check.attributes=FALSE))
 count<-count+1L
 if(!eq){cat('\nDIFF',ns,'fns',deparse1(fn),'unpack',unpack,'named',named,'\n');print(a);print(b)}
}
cat('completed',count,'comparisons\n')
