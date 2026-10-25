norm <- function(x) {
  if(is.character(x)) return(gsub('marginplyr_share_[[:alnum:]]+','marginplyr_share_<id>',x))
  if(is.list(x)) return(lapply(x,norm))
  x
}
for(backend in c('sqlite','duckdb','postgres')) {
 off <- readRDS(file.path('investigation/2026-10-25-sql-audit-followup/evidence',paste0(backend,'-off.rds')))
 on <- readRDS(file.path('investigation/2026-10-25-sql-audit-followup/evidence',paste0(backend,'-on.rds')))
 stopifnot(length(off)==length(on))
 for(i in seq_along(off)) {
   a <- off[[i]]; b <- on[[i]]
   ar <- a$record; br <- b$record; a$record <- b$record <- NULL
   if(!identical(norm(a),norm(b))) {print(all.equal(norm(a),norm(b))); stop(backend,':',a$label)}
   stopifnot(!any(br$status %in% c('invoking','unknown')))
   if(nrow(br)) {
      stopifnot(all(br$status[br$purpose=='result']=='prepared'))
      stopifnot(all(br$source==ifelse(br$purpose=='share_dialect_transaction','literal','render')))
   }
   cat('PASS',backend,a$label,'\n')
 }
}
