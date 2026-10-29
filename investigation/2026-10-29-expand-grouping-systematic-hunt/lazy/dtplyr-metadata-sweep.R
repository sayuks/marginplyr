suppressPackageStartupMessages({library(dplyr);library(dtplyr);library(data.table);pkgload::load_all(quiet=TRUE)})
out <- Sys.getenv('MARGINPLYR_HUNT_OUTPUT', unset = file.path(tempdir(), 'marginplyr-expand-hunt-lazy'))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
options(warn=1)
status <- list(); n<-0L
counts<-c(attempted=0L,source_error=0L,plan_comparable=0L,plan_refused=0L,expand_refused=0L,expanded=0L)
attempt <- function(e) tryCatch(e,error=function(e)e)
record <- function(...) {n<<-n+1L; status[[n]]<<-list(...)}
key_names <- c('g','.N','.I','.GRP','.BY','.SD','.EACHI','.','..1','..-1','..+1','.. 1','.env','.data','T','F','NA','TRUE','FALSE','..marginplyr_sort_1')
for(nm in key_names) for(kind in c('integer','double','logical','character','factor','list','Date')) for(rows in c(0L,3L)) {
 vals<-switch(kind,integer=1:3,double=c(1.5,2.5,3.5),logical=c(TRUE,FALSE,NA),character=c('a','b','c'),factor=factor(c('a','b','c')),list=list(1,2,3),Date=as.Date('2020-01-01')+0:2)[seq_len(rows)]
 data<-tibble::tibble(z=seq_len(rows)); data[[nm]]<-vals
 root<-lazy_dt(data)
 exprs<-list(copy=rlang::sym(nm),copy_plus=rlang::expr(!!rlang::sym(nm)+1),reverse_plus=rlang::expr(1+!!rlang::sym(nm)))
 for(en in names(exprs)) {
 counts['attempted']<-counts['attempted']+1L
  q<-attempt(mutate(root,new_g=!!exprs[[en]])); if(inherits(q,'error')) {counts['source_error']<-counts['source_error']+1L; next}
  actual<-attempt(collect(q)); if(inherits(actual,'error')) {counts['source_error']<-counts['source_error']+1L; next}
  gotplan<-attempt(inspect_grouping(q,.grouping=grouping_set(where(is.numeric))))
  wantplan<-attempt(inspect_grouping(actual,.grouping=grouping_set(where(is.numeric))))
  if(!inherits(gotplan,'error')&&!inherits(wantplan,'error')) counts['plan_comparable']<-counts['plan_comparable']+1L else counts['plan_refused']<-counts['plan_refused']+1L
  if(!inherits(gotplan,'error') && !inherits(wantplan,'error') && !identical(gotplan,wantplan)) {
   record(name=nm,kind=kind,rows=rows,expr=en,verdict='PLAN_DIFF',got=gotplan,want=wantplan,actual=actual)
  }
  got<-attempt(expand_with_margins(q,.grouping=rollup(new_g),.margin_label=NULL,.id='sid'))
  if(inherits(got,'error')) {
   counts['expand_refused']<-counts['expand_refused']+1L
   if(!inherits(got,'marginplyr_error')) record(name=nm,kind=kind,rows=rows,expr=en,verdict='RAW_CONSTRUCT_ERROR',error=conditionMessage(got))
   next
  }
  counts['expanded']<-counts['expanded']+1L
  got<-attempt(collect(got)); want<-attempt(expand_with_margins(actual,.grouping=rollup(new_g),.margin_label=NULL,.id='sid'))
  if(inherits(got,'error')) record(name=nm,kind=kind,rows=rows,expr=en,verdict='COLLECT_ERROR',error=conditionMessage(got)) else if(!isTRUE(all.equal(got,want,check.attributes=TRUE))) record(name=nm,kind=kind,rows=rows,expr=en,verdict='VALUE_DIFF',got=got,want=want)
 }
}
saveRDS(status,file.path(out,'dtplyr-metadata-sweep.rds')); print(counts); print(length(status)); for(s in status) {str(s, max.level=2)}
