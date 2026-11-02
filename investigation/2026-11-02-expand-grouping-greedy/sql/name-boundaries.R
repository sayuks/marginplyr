# Boundary names against independent source-row copies, direct and derived SQL.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
observe <- function(expr) tryCatch(list(ok=TRUE,value=force(expr)),error=function(e) list(ok=FALSE,error=conditionMessage(e)))
key_names <- c('NA','NaN','Inf','NULL','NA_integer_','NA_real_','NA_character_',
 '.data','.env','...', '..1','..0','..01','..-1','..marginplyr_key_1','..MARGINPLYR_KEY_1',
 '..marginplyr_set_id','..marginplyr_sort_1','..marginplyr_input_1','..marginplyr_boolean_1',
 'TRUE','FALSE','true','rowid','_rowid_','oid','x\ny','x\ry','x\ty','x\\y','x`y','x"y',
 'x y','é','É','a.b','1',':x','?','SELECT')
bag <- function(x) sort(vapply(seq_len(nrow(x)),function(i) paste(vapply(x,function(col) if(is.na(col[[i]])) "<NA>" else as.character(col[[i]]),character(1)),collapse="\034"),character(1)))
rows <- list()
conns <- list(sqlite=DBI::dbConnect(RSQLite::SQLite(),':memory:'),duckdb=DBI::dbConnect(duckdb::duckdb(shared_home=FALSE),dbdir=':memory:'))
for (backend in names(conns)) {
 con <- conns[[backend]]
 for(nm in key_names) for(role in c('dimension','fixed','payload','id')) for(sort in c('none','last')) {
  names_ <- c(f='f',g='g',p='p',sid='sid'); names_[switch(role,dimension='g',fixed='f',payload='p',id='sid')] <- nm
  dat <- tibble(f=c('b',NA,'a'),g=c('a','b',NA),p=c(7L,9L,11L)); names(dat) <- names_[c('f','g','p')]
  DBI::dbWriteTable(con,'src',dat,overwrite=TRUE)
  src <- observe(tbl(con,'src'))
  if(!src$ok) next
  src <- src$value
  for(label in c('null','text')) {
   expected <- dat; expected[[names_[['sid']]]] <- 1L
   omitted <- dat; omitted[[names_[['g']]]] <- if(label=='null') NA_character_ else 'Total'; omitted[[names_[['sid']]]] <- 2L
   expected <- structure(Map(c,expected,omitted),class='data.frame',row.names=seq_len(6))[names_[c('f','g','sid','p')]]
   result <- observe(expand_with_margins(src,.by=all_of(names_[['f']]),.grouping=rollup(all_of(names_[['g']])),.id=names_[['sid']],.margin_label=if(label=='null') NULL else 'Total',.sort=sort))
   paths <- if(result$ok) list(direct=function() collect(result$value),head=function() collect(head(result$value,2)),filter=function() collect(filter(result$value,TRUE)),compute=function() collect(compute(result$value,name='dst',temporary=TRUE,overwrite=TRUE))) else list(construction=function() stop(result$error))
   for(path in names(paths)) {
    actual <- observe(paths[[path]]())
    # Head has unspecified selection under sort=none: compare to direct head.
    comparison <- if(path=='head') observe(head(collect(result$value),2)) else list(ok=TRUE,value=expected)
    eq <- actual$ok && comparison$ok && identical(names(actual$value),names(comparison$value)) && identical(bag(actual$value),bag(comparison$value))
    rows[[length(rows)+1]] <- data.frame(backend=backend,name=nm,role=role,sort=sort,label=label,path=path,status=if(!actual$ok) 'error' else if(eq) 'match' else 'difference',detail=if(!actual$ok) actual$error else '')
   }
  }
 }
}
out <- do.call(rbind,rows); write.csv(out,'investigation/2026-11-02-expand-grouping-greedy/sql/name-boundaries.csv',row.names=FALSE)
print(with(out,table(backend,status)))
print(unique(out[out$status=='error',c('backend','name','role','path','status','detail')]),row.names=FALSE)
for(con in conns) DBI::dbDisconnect(con)
