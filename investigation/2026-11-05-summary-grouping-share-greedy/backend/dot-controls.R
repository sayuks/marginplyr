suppressPackageStartupMessages(library(dplyr))
pkgload::load_all('.',quiet=TRUE)
for (backend in c('sqlite','duckdb')) {
 con <- if(backend=='sqlite') DBI::dbConnect(RSQLite::SQLite()) else DBI::dbConnect(duckdb::duckdb(shared_home=FALSE))
 src <- copy_to(con,tibble(g=c('a','b'),v=c(1,3)),'facts',temporary=TRUE)
 for(nm in c('...', '..1', '..0','..00','..01','..-1')) {
  ordinary <- tryCatch(collect(summarise(src,!!nm:=sum(v))),error=identity)
  margin <- tryCatch(collect(summarize_with_margins(src,!!nm:=sum(v), .grouping=rollup(g),.id='set',.sort='last')),error=identity)
  shares <- tryCatch(collect(summarize_with_margins(src,n=sum(v), !!nm:=share_of_total(n),.grouping=rollup(g),.id='set',.sort='last',.check_share_source=FALSE)),error=identity)
  cat(backend, nm, 'ordinary=',if(inherits(ordinary,'error')) 'upstream-refusal' else 'ok', 'margin=', if(inherits(margin,'error')) 'refusal' else 'ok', 'share=',if(inherits(shares,'error')) 'refusal' else 'ok','\n')
  if(nm %in% c('...','..1')) stopifnot(inherits(ordinary,'error'),inherits(margin,'error'),inherits(shares,'error')) else stopifnot(!inherits(ordinary,'error'),!inherits(margin,'error'),!inherits(shares,'error'),isTRUE(all.equal(shares[[nm]],c(0.25, 0.75, 1))))
 }
 DBI::dbDisconnect(con)
}
