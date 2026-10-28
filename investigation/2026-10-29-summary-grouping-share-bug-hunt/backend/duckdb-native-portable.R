args <- commandArgs(TRUE)
root <- if(length(args)) args[[1]] else getwd()
pkgload::load_all(root,quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
con <- DBI::dbConnect(duckdb::duckdb(shared_home=FALSE), array='matrix')
exprs <- list(
 decimal='CAST(v AS DECIMAL(18,2))', hugeint='CAST(v AS HUGEINT)', uhugeint='CAST(v AS UHUGEINT)',
 tinyint='CAST(v AS TINYINT)', utinyint='CAST(v AS UTINYINT)', float='CAST(v AS FLOAT)',
 timestamp="CAST('2026-10-29' AS TIMESTAMP) + v * INTERVAL '1 hour'", timestamptz="CAST('2026-10-29' AS TIMESTAMPTZ) + v * INTERVAL '1 hour'",
 date="CAST('2026-10-29' AS DATE) + CAST(v AS INTEGER)", time="CAST('01:02:03' AS TIME) + v * INTERVAL '1 hour'",
 interval="v * INTERVAL '1 day'", bool='v = 1', uuid="CAST(CASE WHEN v=1 THEN '00000000-0000-0000-0000-000000000001' ELSE '00000000-0000-0000-0000-000000000002' END AS UUID)",
 blob="CAST(CAST(v AS VARCHAR) AS BLOB)", struct="struct_pack(a := v, b := CAST(v AS VARCHAR))", list='[v,v+1]', array='CAST([v,v+1] AS BIGINT[2])', map="MAP(['a','b'], [v,v+1])"
)
failures <- list(); cases <- 0L; refused <- 0L
for (kind in names(exprs)) {
  sql <- paste0('CREATE OR REPLACE TABLE inp AS SELECT ',exprs[[kind]],' AS g, POW(2, ROW_NUMBER() OVER ()) AS x FROM (VALUES (1),(2),(1),(NULL)) t(v)')
  e <- tryCatch(DBI::dbExecute(con,sql),error=identity)
  if(inherits(e,'error')) {cat('INPUT_REFUSED',kind,conditionMessage(e),'\n'); next}
  src <- tryCatch(tbl(con,'inp'),error=identity); if(inherits(src,'error')) {cat('SOURCE_REFUSED',kind,conditionMessage(src),'\n');next}
  for(label in list(NULL,'Total')) for(s in c('none','last','first')) for(empty in c(FALSE,TRUE)) {
    cases <- cases + 1L
    input <- if(empty) filter(src,x<0) else src
    f <- function(dup) collect(summarize_with_margins(input,value=sum(x,na.rm=TRUE),bit=grouping_bit(g),.grouping=rollup(g),.margin_label=label,.duplicates=dup,.id='set',.sort=s))
    native <- tryCatch(f('error'),error=identity); portable <- tryCatch(f('keep'),error=identity)
    if(inherits(native,'error') && inherits(portable,'error')) { refused<-refused+1L;cat('PAIRED_REFUSAL',kind,if(is.null(label))'NULL' else label,s,empty,'\n');next }
    norm <- function(z){ if(s=='none') z <- arrange(z,set,value); z }
    if(inherits(native,'error')||inherits(portable,'error')||!isTRUE(all.equal(norm(native),norm(portable)))) {
      id<-paste(kind,is.null(label),s,empty,sep=':');failures[[id]]<-list(native=native,portable=portable)
      cat('FAIL',id,if(inherits(native,'error')) conditionMessage(native) else if(inherits(portable,'error')) conditionMessage(portable) else paste(all.equal(norm(native),norm(portable)),collapse=';'),'\n')
    }
  }
}
DBI::dbDisconnect(con,shutdown=TRUE)
cat('CASES',cases,'PAIRED_REFUSALS',refused,'FAILURES',length(failures),'\n')
stopifnot(length(failures)==0L)
