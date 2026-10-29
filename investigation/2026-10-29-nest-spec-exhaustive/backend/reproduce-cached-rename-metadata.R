# Investigated: 2026-10-29 against ee4321789ca60b6259c53df2852f1574aaede584.
# Run from the repository root. --assert requires admitted metadata acquisition.
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
observe<-function(f){w<-character();v<-tryCatch(withCallingHandlers(f(),warning=function(c){w<<-c(w,conditionMessage(c));invokeRestart('muffleWarning')}),error=identity);list(v=v,w=w)}
s<-data.table::data.table(g=c('a','b'),v=1:2);before<-data.table::copy(s);r<-dtplyr::lazy_dt(s);renamed<-rename(r,value=v);cached<-compute(renamed);q<-union_all(cached,cached)
control<-observe(function()collect(q));stopifnot(!inherits(control$v,'error'),!length(control$w),nrow(control$v)==4L,identical(s,before))
consumers<-list(inspect=function(x)inspect_grouping(x,.by=where(is.character)),nest=function(x)nest_with_margins(x),nest_by=function(x)nest_by_with_margins(x))
failures<-logical()
for(nm in names(consumers)){
 fun<-consumers[[nm]]
 for(shape in c('cached-shared','uncached','cached-only','collected')){
  input<-switch(shape,'cached-shared'=q,'uncached'=union_all(renamed,renamed),'cached-only'=cached,'collected'=control$v)
  ans<-observe(function()fun(input));failure<-inherits(ans$v,'error');if(shape=='cached-shared')failures<-c(failures,failure)
  cat(nm,shape,'error',failure,'warnings',length(ans$w),if(failure)conditionMessage(ans$v)else'success','\n');stopifnot(identical(s,before));if(shape!='cached-shared')stopifnot(!failure,!length(ans$w))
 }
}
z<-getFromNamespace('zero_row_dtplyr_proxy_input','marginplyr')(q);cat('schema-only query:\n');show_query(head(z,0L));as_dt<-getS3method('as.data.table','dtplyr_step',envir=asNamespace('data.table'));raw<-observe(function()as_dt(head(z,0L)));cat('private evaluation diagnostic:',if(inherits(raw$v,'error'))conditionMessage(raw$v)else'success','\n')
z<-getFromNamespace('zero_row_dtplyr_proxy_input','marginplyr')(q);z[['needs_copy']]<-TRUE;protected<-observe(function()as_dt(head(z,0L)));stopifnot(!inherits(protected$v,'error'),!length(protected$w),identical(s,before));cat('experimental restore-copy-flag: successful typed proxy\n')
if('--assert'%in%commandArgs(trailingOnly=TRUE))stopifnot(!any(failures))
