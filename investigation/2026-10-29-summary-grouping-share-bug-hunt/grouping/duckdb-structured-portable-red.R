# Desired-behavior assertions for the shared omitted-dimension labeling seam.
# Read-only to package source; intentionally exits 1 at the investigated commit.
args <- commandArgs(trailingOnly = TRUE)
pkgload::load_all(if (length(args)) args[[1L]] else getwd(), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), array = 'matrix')
fixtures <- list(
 struct = list(expr='struct_pack(a := v)', type='STRUCT(a INTEGER)'),
 array = list(expr='CAST([v, v+1] AS INTEGER[2])', type='INTEGER[2]'),
 scalar = list(expr='v', type='INTEGER')
)
failed <- character()
for (kind in names(fixtures)) {
 fixture <- fixtures[[kind]]
 DBI::dbExecute(con,paste0('CREATE OR REPLACE TABLE inp AS SELECT ',fixture$expr,' AS g FROM (VALUES (1),(2)) t(v)'))
 source <- tbl(con,'inp')
 cat('KIND',kind,'\n')
 ordinary <- collect(summarise(source,n=n(),.by=g))
 cat('Ordinary grouping rows:',nrow(ordinary),'\n')
 stopifnot(nrow(ordinary)==2L,all(ordinary$n==1))
 native <- collect(summarize_with_margins(source,n=n(),.grouping=rollup(g),.margin_label=NULL,.id='set',.duplicates='error'))
 cat('Native row counts / ids:',paste(native$n,collapse=','),'/',paste(native$set,collapse=','),'\n')
 stopifnot(nrow(native)==3L,sum(native$n[native$set==1L])==2,native$n[native$set==2L]==2)
 control_sql <- paste0('SELECT g, COUNT(*) AS n FROM inp GROUP BY g UNION ALL SELECT NULL::',fixture$type,' AS g, COUNT(*) AS n FROM inp')
 direct <- DBI::dbGetQuery(con,control_sql)
 cat('Explicit typed-NULL UNION rows:',nrow(direct),'\n')
 stopifnot(nrow(direct)==3L,identical(sort(as.numeric(direct$n)),c(1,1,2)))
 proxy <- marginplyr:::grouping_selection_proxy(source)
 cat('Schema proxy:',typeof(proxy$g),paste(class(proxy$g),collapse='/'),'dim=',paste(dim(proxy$g),collapse='x'),'\n')
 query <- tryCatch(summarize_with_margins(source,n=n(),.grouping=rollup(g),.margin_label=NULL,.id='set',.duplicates='keep'),error=identity)
 if (!inherits(query,'error')) {
  sql <- dbplyr::sql_render(query)
  cat('Portable omitted-value SQL:\n',as.character(sql),'\n')
  portable <- tryCatch(collect(query),error=identity)
 } else portable <- query
 if (inherits(portable,'error')) {
  failed <- c(failed,kind)
  cat('DESIRED BEHAVIOR FAILED:',conditionMessage(portable),'\n')
 } else {
  cat('Portable rows:',nrow(portable),'\n')
  stopifnot(nrow(portable)==3L,identical(sort(as.numeric(portable$n)),c(1,1,2)),identical(sort(as.integer(portable$set)),c(1L,1L,2L)))
 }
}
DBI::dbDisconnect(con,shutdown=TRUE)
cat('Failed accepted dimension types:',paste(failed,collapse=','),'\n')
stopifnot(length(failed)==0L)
