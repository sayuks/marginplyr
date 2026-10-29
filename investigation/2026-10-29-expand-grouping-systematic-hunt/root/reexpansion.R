pkgload::load_all(quiet=TRUE)
set.seed(291047)
conns <- list(sqlite=DBI::dbConnect(RSQLite::SQLite(), ':memory:'),duckdb=DBI::dbConnect(duckdb::duckdb(shared_home=FALSE)))
finish <- function() {for (k in names(conns)) {if(k=='duckdb') DBI::dbDisconnect(conns[[k]],shutdown=TRUE) else DBI::dbDisconnect(conns[[k]])}}
canon <- function(z) { z <- as.data.frame(dplyr::collect(z)); z[] <- lapply(z,as.character); z <- z[do.call(order,c(unname(z),list(na.last=TRUE))),,drop=FALSE]; rownames(z)<-NULL;z }
run <- function(expr) tryCatch(list(value=eval.parent(substitute(expr))),error=function(e)list(error=conditionMessage(e)))
passed <- 0L; raw <- list(); tested <- 0L; refused <- 0L
layouts <- list(c('g','h','v'),c('.env','h','v'),c('.data','h','v'),c('true','false','v'),c('TRUE','FaLsE','.env'),c('..marginplyr_order_1','..marginplyr_input_1','..marginplyr_boolean_1'),c('g','rowid','oid'))
for (layout in layouts) {
  d <- tibble::as_tibble(stats::setNames(list(c('b','a',NA),c('x','y','z'),c('r1','r2','r3')),layout))
  g <- layout[[1L]]; h <- layout[[2L]]
  for (sort in c('none','first','last')) for (id in c('sid','.env','true')) {
    if(id %in% layout) next
    for(empty in c(FALSE,TRUE)) {
      src <- if(empty) d[0,] else d
      local <- expand_with_margins(src,.grouping=rollup(dplyr::all_of(g)),.margin_label=NULL,.sort=sort,.id=id)
      oracle <- canon(expand_with_margins(local,.grouping=rollup(dplyr::all_of(h)),.margin_label=NULL,.id='second',.sort='last'))
      for (backend in names(conns)) {
        tested<-tested+1L
        out <- run({
          x <- dplyr::copy_to(conns[[backend]],src,overwrite=TRUE,name='src')
          first <- expand_with_margins(x,.grouping=rollup(dplyr::all_of(g)),.margin_label=NULL,.sort=sort,.id=id)
          canon(expand_with_margins(first,.grouping=rollup(dplyr::all_of(h)),.margin_label=NULL,.id='second',.sort='last'))
        })
        if (tolower(id) %in% tolower(layout) && !is.null(out$error) && grepl('equivalent identifiers',out$error,fixed=TRUE)) {refused<-refused+1L; next}
        if(!is.null(out$value)&&identical(out$value,oracle)) {passed<-passed+1L} else {
          raw[[length(raw)+1L]] <- list(layout=layout,sort=sort,id=id,empty=empty,backend=backend,observed=out,expected=oracle)
        }
      }
    }
  }
}
finish();cat('tested',tested,'passed',passed,'refused',refused,'raw',length(raw),'\n');dput(raw)
stopifnot(length(raw)==0L)
