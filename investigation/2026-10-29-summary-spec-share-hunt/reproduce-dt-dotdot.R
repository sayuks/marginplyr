args <- commandArgs(TRUE)
root_arg <- args[startsWith(args, '--root=')]
root <- if (length(root_arg)) sub('^--root=', '', root_arg[[1L]]) else '.'
pkgload::load_all(root, quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
results <- list()
for(nm in c('..0','..00','..01')) for(kind in c('parent','total')) {
 d<-tibble(fixed=c('f','f'),g=c('a','b'),x=c(1,3));names(d)[1]<-nm
 input<-dtplyr::lazy_dt(d)
 plain<-as.data.frame(summarize_with_margins(input,value=sum(x),.by=all_of(nm),.grouping=rollup(g)))
 stopifnot(identical(plain$value,c(1,3,4)))
 h<-rlang::call2(paste0('share_of_',kind),quote(value))
 query<-summarize_with_margins(input,value=sum(x),share=!!h,.by=all_of(nm),.grouping=rollup(g))
 stopifnot(inherits(query,'dtplyr_step'))
 result<-tryCatch(as.data.frame(query),error=identity)
 id<-paste(nm,kind)
 cat(id,':',if(inherits(result,'error'))conditionMessage(result) else paste(result$share,collapse=','),'\n')
 results[[id]]<-result
}
cat('Completed',length(results),'lazy construction/collection probes\n')
if('--assert'%in%args)for(id in names(results)){
 result<-results[[id]]
 if(inherits(result,'error'))stop(id,' rejected a Margin-supported fixed key: ',conditionMessage(result),call.=FALSE)
 stopifnot(identical(result$share,c(0.25,0.75,1)))
}
