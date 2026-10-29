# Investigated: 2026-10-29 against ee4321789ca60b6259c53df2852f1574aaede584.
# Run from the repository root. --assert requires faithful typed selections.
pkgload::load_all('.',quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
observe<-function(f){w<-character();v<-tryCatch(withCallingHandlers(f(),warning=function(c){w<<-c(w,conditionMessage(c));invokeRestart('muffleWarning')}),error=identity);list(v=v,w=w)}
source<-data.table::data.table(g=1:2,v=3:4);old<-data.table::copy(source);r<-dtplyr::lazy_dt(source)
l<-compute(mutate(r,new=v+1L));rr<-compute(mutate(r,new='a'));q<-inner_join(l,rr,by='g')
real<-observe(function()collect(q));stopifnot(!inherits(real$v,'error'),!length(real$w),is.character(real$v$new.y))
actual<-observe(function()inspect_grouping(q,.by=where(is.character),.format='list'))
expected<-inspect_grouping(real$v,.by=where(is.character),.format='list')
cat('minimal expected fixed:',paste(expected$fixed[[1L]],collapse=','),'\nminimal actual fixed:',paste(actual$v$fixed[[1L]],collapse=','),'\n')
for(consumer in list(nest_with_margins,nest_by_with_margins)){
 a<-observe(function()collect(consumer(q,.by=where(is.character))));b<-consumer(real$v,.by=where(is.character))
 cat('expected nesting columns:',paste(names(b),collapse=','),'actual nesting columns:',paste(names(a$v),collapse=','),'warnings',length(a$w),'\n')
}
rollup_result<-observe(function()inspect_grouping(q,.grouping=rollup(where(is.character))))
cat('typed rollup result:',if(inherits(rollup_result$v,'error'))conditionMessage(rollup_result$v)else'success','\n')
stopifnot(identical(source,old))
# The matrix varies scalar assignment types independently on two cached branches
# over the same immutable root. Direct collected columns supply the oracle.
values<-list(logical=TRUE,integer=1L,double=1,character='a')
predicates<-list(logical=is.logical,integer=is.integer,double=is.double,character=is.character)
results<-list();n<-0L;metadata<-0L;warnings<-0L
for(lt in names(values))for(rt in names(values))for(jn in c('left_join','right_join','inner_join','full_join')){
 s<-data.table::data.table(g=1:2,v=3:4);old<-data.table::copy(s);root<-dtplyr::lazy_dt(s)
 lhs<-compute(mutate(root,new=!!values[[lt]]));rhs<-compute(mutate(root,new=!!values[[rt]]));step<-get(jn)(lhs,rhs,by='g')
 control<-observe(function()collect(step));stopifnot(!inherits(control$v,'error'),!length(control$w));real<-control$v
 for(pn in names(predicates)){
  pred<-predicates[[pn]];fixed<-names(real)[vapply(real,pred,logical(1))]
  got<-observe(function()inspect_grouping(step,.by=where(pred),.format='list'))
  status<-if(inherits(got$v,'error'))'error'else if(identical(got$v$fixed[[1L]],fixed))'match'else'mismatch'
  n<-n+1L;results[[n]]<-data.frame(lt,rt,join=jn,predicate=pn,consumer='inspect',status,warnings=length(got$w),expected=paste(fixed,collapse=','),actual=if(inherits(got$v,'error'))conditionMessage(got$v)else paste(got$v$fixed[[1L]],collapse=','))
  for(verb in c('nest','nest_by')){
   fun<-if(verb=='nest')nest_with_margins else nest_by_with_margins
   a<-observe(function()collect(fun(step,.by=where(pred))))
   expected_columns<-c(fixed,'data')
   status<-if(inherits(a$v,'error'))'error'else if(identical(names(a$v),expected_columns))'match'else'mismatch'
   n<-n+1L;results[[n]]<-data.frame(lt,rt,join=jn,predicate=pn,consumer=verb,status,warnings=length(a$w),expected=paste(expected_columns,collapse=','),actual=if(inherits(a$v,'error'))conditionMessage(a$v)else paste(names(a$v),collapse=','))
  }
  stopifnot(identical(s,old))
 }
}
res<-bind_rows(results);print(count(res,consumer,status));cat('CASES',nrow(res),'MISMATCHES',sum(res$status=='mismatch'),'ERRORS',sum(res$status=='error'),'WARNING_CASES',sum(res$warnings>0L),'source snapshots unchanged\n')
outdir<-dirname(sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[[1L]]));write.csv(res,file.path(outdir,'shared-cache-types-results.csv'),row.names=FALSE)
if('--assert'%in%commandArgs(trailingOnly=TRUE))stopifnot(identical(actual$v$fixed[[1L]],expected$fixed[[1L]]),!inherits(rollup_result$v,'error'),all(res$status=='match'),!any(res$warnings))
