args <- commandArgs(trailingOnly = TRUE)
if (length(args)) setwd(args[[1L]])
pkgload::load_all(quiet=TRUE)
library(dplyr,warn.conflicts=FALSE)
data<-data.frame(a=c(1,2),b=c('x','y'),c=c(TRUE,FALSE),check.names=FALSE)
cases<-list(quote(a),quote(c(a,b)),quote(-b),quote(a:b),quote(!b),quote(a|b),quote(a&b),quote(c(a,-b)),quote(all_of(c('b','a'))),quote(any_of(c('absent','b'))),quote(starts_with('a')),quote(matches('^[ab]$')),quote(where(is.numeric)),quote(where(is.character)),quote(c(where(is.numeric),b)),quote(names(tidyselect::peek_data())),quote(all_of(names(tidyselect::peek_data()))),quote(all_of(tidyselect::peek_vars())),quote(all_of(rev(tidyselect::peek_vars()))),quote(.data$a),quote(.data[['b']]),quote(a/b),rlang::call2('c',rlang::sym('a'),rlang::missing_arg(),rlang::sym('b')))
backends<-list(local=data,dtplyr=dtplyr::lazy_dt(data),arrow=arrow::arrow_table(data),duckdb={con<-DBI::dbConnect(duckdb::duckdb(shared_home = FALSE));copy_to(con,data,'grouping_hunt',temporary=TRUE)})
successes<-0L; refusals<-0L
options(lifecycle_verbosity='quiet')
for(e in cases){
 expected<-tryCatch(names(tidyselect::eval_select(e,data,env=globalenv())),error=function(e)e)
 for(b in names(backends)){
  result<-tryCatch(inspect_grouping(backends[[b]],.grouping=eval(rlang::call2('grouping_set',e)),.format='list'),error=function(e)e)
  if(!inherits(expected,'error')&&!inherits(result,'error')) { stopifnot(setequal(result$included[[1]],expected)); successes<-successes+1L }
  else if(inherits(expected,'error')&&inherits(result,'error')) {refusals<-refusals+1L}
  else stop('Oracle mismatch: ',b,' ',deparse1(e))
 }
}
DBI::dbDisconnect(con,shutdown=TRUE)
cat('PASS: 23 selections x 4 backends; successful=',successes,' paired-refusals=',refusals,'\n',sep='')
