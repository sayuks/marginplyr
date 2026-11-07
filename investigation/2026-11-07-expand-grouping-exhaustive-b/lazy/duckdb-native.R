args <- commandArgs(trailingOnly=TRUE)
source_dir <- normalizePath(args[[1L]],mustWork=TRUE)
output <- args[[2L]]
dir.create(output,recursive=TRUE,showWarnings=FALSE)
Sys.setenv(TESTTHAT_PARALLEL='false')
pkgload::load_all(source_dir,quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
con <- DBI::dbConnect(duckdb::duckdb(shared_home=FALSE))
exprs <- list(arr="[1,2,NULL]",st="{'x': 1, 'y': 'a'}",mp="map(['a'],[1])",bytes="'abc'::BLOB",day="DATE '2020-01-01'",stamp="TIMESTAMP '2020-01-01 01:02:03.123456'",iv="INTERVAL 1 DAY",decimal="1.234::DECIMAL(20,3)",enum="'duck'::ENUM('duck','goose')")
families <- list(detail=grouping_set(g),rollup=rollup(g),omitted_first=grouping_sets(grouping_set(),grouping_set(g)),repeated=grouping_sets(grouping_set(g),grouping_set(),grouping_set(g)),product=grouping_spec(rollup(g),grouping_set()))
rows <- list()
canon <- function(x) arrange(x,rid,sid)
for(nm in names(exprs)) for(empty in c(FALSE,TRUE)) {
 query <- paste0('SELECT 1::INTEGER AS rid,',exprs[[nm]],' AS g,',exprs[[nm]],' AS payload UNION ALL SELECT 2, NULL, NULL')
 if(empty) query <- paste0('SELECT * FROM (',query,') AS src WHERE FALSE')
 input <- tbl(con,sql(query))
 source <- collect(input)
 for(family in names(families)) for(sort in c('none','first','last')) {
  spec <- families[[family]]
  expected <- expand_with_margins(source,.by=rid,.grouping=spec,.margin_label=NULL,.id='sid',.sort=sort,.duplicates='keep')
  result <- expand_with_margins(input,.by=rid,.grouping=spec,.margin_label=NULL,.id='sid',.sort=sort,.duplicates='keep')
  for(route in c('collect','compute','zero')) {
   got <- tryCatch(switch(route,collect=collect(result),compute={tmp<-compute(result,name='hunt_result',temporary=TRUE,overwrite=TRUE);collect(tmp)},zero=collect(result,n=0L)),error=identity)
   target <- if(route=='zero') expected[integer(),] else expected
   match <- !inherits(got,'error') && identical(canon(got),canon(target))
   physical <- if(route=='zero'||sort=='none'||inherits(got,'error')) NA else identical(got,target)
   rows[[length(rows)+1L]] <- data.frame(type=nm,empty,family,sort,route,match,physical,error=if(inherits(got,'error'))conditionMessage(got)else'')
  }
 }
}
result <- bind_rows(rows)
write.csv(result,file.path(output,'duckdb-native.csv'),row.names=FALSE)
print(count(result,match,physical,.drop=FALSE)); print(filter(result,!match|(!is.na(physical)&!physical)))
DBI::dbDisconnect(con,shutdown=TRUE)
stopifnot(all(result$match),all(result$physical,na.rm=TRUE))
cat('PASS:',nrow(result),'native DuckDB comparisons\n')
