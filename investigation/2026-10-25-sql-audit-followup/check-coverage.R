norm <- function(x) gsub('marginplyr_share_[[:alnum:]]+','marginplyr_share_<id>',x)
for(backend in c('sqlite','duckdb','postgres')) {
 all <- readRDS(file.path('investigation/2026-10-25-sql-audit-followup/evidence',paste0(backend,'-on.rds')))
 for(x in all) {
  internal <- x$record[x$record$purpose!='result',]
  observed <- norm(x$sql)
  if(backend=='postgres'&&x$label=='share_transaction_cold') observed<-observed[c(TRUE,observed[-1]!=observed[-length(observed)])]
  stopifnot(identical(norm(internal$sql),observed))
  cat('PASS',backend,x$label,'internal SQL coverage',nrow(internal),'\n')
 }
}
