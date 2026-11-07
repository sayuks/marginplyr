args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=3L || !args[[3L]] %in% c('observation','control','contract')) {
 stop('Usage: Rscript share-renamed-safe.R SOURCE OUTPUT_DIR observation|control|contract')
}
Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all(args[[1L]],quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
out <- args[[2L]]
dir.create(out,recursive=TRUE,showWarnings=FALSE)
data <- data.frame(g=c('a','b'),x=c(1,3),prior=c(10,20))
rows <- list()
for(input_name in c('data.frame','tibble','data.table')) {
 input <- switch(input_name,data.frame=data,tibble=tibble::as_tibble(data),data.table=data.table::as.data.table(data))
 before <- serialize(input,NULL)
 for(kind in c('parent','total')) for(direction in c('after_share','source')) for(access in c('name','index')) {
  if(args[[3L]]=='control' && access=='name') next
  read <- substitute((function(prior) {
    carrier <- list(read=function() sum(prior),other=function() 7)
    names(carrier) <- c('other','read')
    rm(prior)
    ACCESS
  })(1),list(ACCESS=if(access=='name') quote(carrier$read()) else quote(carrier[[2L]]())))
  helper <- rlang::call2(paste0('share_of_',kind),if(direction=='after_share') quote(value) else quote(observed))
  result <- tryCatch(if(direction=='after_share') {
   summarize_with_margins(input,value=sum(x),prior=!!helper,observed=!!read,.grouping=rollup(g))
  } else {
   summarize_with_margins(input,prior=sum(x),observed=!!read,ratio=!!helper,.grouping=rollup(g))
  },error=identity)
  correct <- !inherits(result,'error') && identical(as.numeric(result$observed),rep(7,3L))
  refused <- inherits(result,'marginplyr_error')
  rows[[length(rows)+1L]] <- data.frame(input=input_name,kind,direction,access,
    status=if(inherits(result,'error')) class(result)[[1L]] else paste(result$observed,collapse=','),
    correct,observation_ok=if(access=='name') refused else correct)
 }
 stopifnot(identical(before,serialize(input,NULL)))
}
result <- do.call(rbind,rows)
write.csv(result,file.path(out,'results.csv'),row.names=FALSE)
print(table(result$access,result$status))
if(args[[3L]]=='observation') stopifnot(all(result$observation_ok)) else stopifnot(all(result$correct))
