# Independent public witness with a different value and a FALSE spelling.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
DBI::dbWriteTable(con,'source',stats::setNames(data.frame(g='b',payload=19L),c('g','FALSE')))
src <- tbl(con,'source')
expanded <- expand_with_margins(src,.grouping=rollup(g))
materialized <- compute(expanded,name='materialized')
key <- select(src,g)
observe <- function(expr) tryCatch(list(ok=TRUE,value=force(expr)),error=function(e) list(ok=FALSE,message=conditionMessage(e)))
results <- list()
for(nm in c('source','materialized','expanded')) {
 x <- switch(nm,source=src,materialized=materialized,expanded=expanded)
 results[[paste0(nm,'_left_join_rhs')]] <- observe(collect(left_join(key,x,by='g')))
 results[[paste0(nm,'_left_join_lhs')]] <- observe(collect(left_join(x,key,by='g')))
 results[[paste0(nm,'_union_rhs')]] <- observe(collect(union_all(src,x)))
 results[[paste0(nm,'_union_lhs')]] <- observe(collect(union_all(x,src)))
 results[[paste0(nm,'_intersect_rhs')]] <- observe(collect(intersect(src,x)))
}
for(nm in names(results)) {
 cat(nm,': ',if(results[[nm]]$ok) 'OK' else results[[nm]]$message,'\n',sep='')
 if(results[[nm]]$ok) print(results[[nm]]$value)
}
stopifnot(all(vapply(results[!grepl('^expanded',names(results))],`[[`,logical(1),'ok')))
stopifnot(identical(collect(expanded)[['FALSE']],c(19L,19L)))
cat('join LHS special class: ',inherits(left_join(expanded,key,by='g'),'marginplyr_sqlite_typed_result'),'\n',sep='')
cat('union RHS special class: ',inherits(union_all(src,expanded),'marginplyr_sqlite_typed_result'),'\n',sep='')
cat('union LHS special class: ',inherits(union_all(expanded,src),'marginplyr_sqlite_typed_result'),'\n',sep='')
DBI::dbDisconnect(con)
if('--assert' %in% commandArgs(trailingOnly=TRUE)) {
 stopifnot(all(vapply(results,`[[`,logical(1),'ok')))
}
