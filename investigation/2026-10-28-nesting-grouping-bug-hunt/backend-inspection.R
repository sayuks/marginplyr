# Run from the repository root; exploratory evidence for 2026-10-28.
pkgload::load_all('.', quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
data <- tibble(a=c(1L,2L,NA), b=c(1.5,2.5,NA), ch=c('x','y',NA), l=c(TRUE,FALSE,NA), f=factor(c('a','b',NA)), d=as.Date(c('2026-01-01','2026-01-02',NA)))
selectors <- list(numeric=expr(where(is.numeric)), integer=expr(where(is.integer)), double=expr(where(is.double)), character=expr(where(is.character)), logical=expr(where(is.logical)), factor=expr(where(is.factor)), date=expr(where(~ inherits(.x,'Date'))), names=expr(c(a,ch)))
queries <- list(root=function(x)x, filtered=function(x) filter(x,a>1), selected=function(x) select(x,ch,a,b), renamed=function(x) rename(x, aa=a), reordered=function(x) relocate(x,ch), arranged=function(x) arrange(x,a), mutated=function(x) mutate(x,z=a+1), grouped=function(x) group_by(x,ch))
probe <- function(source, compare, label) {
 for(qn in names(queries)) {
  q <- tryCatch(queries[[qn]](source),error=identity)
  actual_data <- tryCatch(dplyr::collect(q),error=identity)
  if(inherits(q,'error')||inherits(actual_data,'error')) next
  for(sn in names(selectors)) {
   s<-selectors[[sn]]
   actual <- tryCatch(rlang::eval_tidy(expr(inspect_grouping(!!q,.grouping=grouping_set(!!s),.format='list'))),error=identity)
   expected <- tryCatch(rlang::eval_tidy(expr(inspect_grouping(!!actual_data,.grouping=grouping_set(!!s),.format='list'))),error=identity)
   checked <<- checked + 1L
   ok <- if (inherits(actual, 'error') && inherits(expected, 'error')) identical(class(actual), class(expected)) else identical(actual, expected)
   if(!ok) {failed <<- failed + 1L; cat('DIFF',label,qn,sn,'\n'); print(actual);print(expected)}
  }
 }
}
checked<-0L; failed<-0L
probe(dtplyr::lazy_dt(data),data,'dtplyr')
probe(arrow::Table$create(data),data,'arrow')
con<-DBI::dbConnect(duckdb::duckdb(shared_home=FALSE));
remote<-copy_to(con,data,'probe',temporary=TRUE)
probe(remote,collect(remote),'duckdb')
DBI::dbDisconnect(con,shutdown=TRUE)
cat('CHECKED',checked,'FAILED',failed,'\n');stopifnot(failed==0L)
