pkgload::load_all('.', quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
counts <- c(attempt=0L,both_error=0L,local_only_error=0L,dt_only_error=0L,difference=0L,equal=0L)
ns <- c('...','..1','..2','..x','..g','.env','.data','NA','NaN','Inf','TRUE','FALSE','NULL','function','if','a b','a`b','x\ny','%','')
for (nm in ns) for (role in c('payload','fixed','dimension','key','id')) {
  x <- tibble(g=c('a','b','a'),value=1:3)
  if(role=='payload') names(x)[2] <- nm
  if(role %in% c('fixed','dimension')) names(x)[1] <- nm
  run <- function(z) nest_with_margins(z, .by=if(role=='fixed') all_of(nm), .grouping=if(role=='dimension') rollup(all_of(nm)) else if(role=='fixed') grouping_set() else rollup(g), .id=if(role=='id') nm else 'set', .key=if(role=='key') nm else 'data', .keep=TRUE, .sort='last', .margin_label=NULL)
  counts[['attempt']] <- counts[['attempt']] + 1L
  a <- tryCatch(run(x),error=identity)
  b <- tryCatch(tibble::as_tibble(run(dtplyr::lazy_dt(x)), .name_repair='minimal'),error=identity)
  norm <- function(z) { v <- as.list(z); k <- if(role=='key') nm else 'data'; v[[k]]<-lapply(v[[k]],as.list); v }
  if (inherits(a,'error') || inherits(b,'error')) {
    key <- if(inherits(a,'error') && inherits(b,'error')) 'both_error' else if(inherits(a,'error')) 'local_only_error' else 'dt_only_error'; counts[[key]] <- counts[[key]] + 1L
    cat('ERROR',dQuote(nm),role,'local=',inherits(a,'error'),'dt=',inherits(b,'error'),'\n')
  } else if(!identical(norm(a),norm(b))) { counts[['difference']]<-counts[['difference']]+1L; cat('DIFF',dQuote(nm),role,'\n') } else counts[['equal']]<-counts[['equal']]+1L
}
print(counts)
