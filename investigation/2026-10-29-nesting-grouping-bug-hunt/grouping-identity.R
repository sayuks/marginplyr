pkgload::load_all(commandArgs(TRUE)[[1]],quiet=TRUE)
library(dplyr)
set.seed(91083)
d <- tibble(fixed='F',a=c('x','x','y'),b=c('q','r','q'),c=c('s','s','t'),d=c('u','u','v'),v=1:3)
cs <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
cd <- DBI::dbConnect(duckdb::duckdb())
inputs <- list(local=d,dt=dtplyr::lazy_dt(d),sqlite=copy_to(cs,d,'d'),duck=copy_to(cd,d,'d'),arrow=arrow::Table$create(d),batch=arrow::RecordBatch$create(d))
cols <- letters[1:4]
for(i in 1:80) {
  chosen <- sample(cols,sample(1:4,1))
  spec <- if(i%%2) rollup(!!!rlang::syms(chosen)) else cube(!!!rlang::syms(chosen))
  perm <- sample(c(chosen,'fixed'))
  plan <- inspect_grouping(d,.by=fixed,.grouping=spec,.format='list')
  expected <- vapply(plan$included, function(x) as.integer(sum(as.integer(!perm %in% c(x,'fixed')) * 2^rev(seq_along(perm)-1L))), integer(1))
  for(n in names(inputs)) {
    out <- rlang::inject(summarize_with_margins(inputs[[n]],gid=grouping_id(!!!rlang::syms(perm)),default=grouping_id(),bit=grouping_bit(!!rlang::sym(chosen[[1]])),n=dplyr::n(),.by=fixed,.grouping=spec,.margin_label=NULL,.id='set')) |> collect()
    stopifnot(all(out$gid==expected[out$set]),all(out$default==plan$grouping_id[out$set]),all(out$bit==as.integer(vapply(plan$included,function(x)!chosen[[1]]%in%x,logical(1)))[out$set]))
  }
}
DBI::dbDisconnect(cs);DBI::dbDisconnect(cd,shutdown=TRUE)
cat('PASS: 80 generated plans x 6 input forms; reordered/fixed/default Grouping identifiers and Grouping bits\n')
