pkgload::load_all(quiet=TRUE)
probe<-function(label,expr) tryCatch({force(expr);cat('PASS',label,'\n')},error=function(e)cat('FAIL',label,paste(class(e),collapse='/'),conditionMessage(e),'\n'))
# Zero-column identity and repeated-occurrence cardinality.
for(cls in c('data.frame','tbl_df'))for(n in c(0L,1L,5L))for(id in list(NULL,'sid','..0','..-1','.. 1','.data','.env'))for(sort in c('none','first','last')) {
 x<-data.frame(row.names=seq_len(n)); if(cls=='tbl_df')x<-tibble::as_tibble(x);if(cls=='data.table')x<-data.table::as.data.table(x)
 actual_n<-nrow(x)
 z<-tryCatch(expand_with_margins(x,.grouping=grouping_sets(grouping_set(),grouping_set()),.duplicates='keep',.id=id,.sort=sort),error=identity)
 probe(paste('zero',cls,n,id,sort),{if(inherits(z,'error'))stop(z);stopifnot(nrow(z)==2L*actual_n,identical(names(z),if(is.null(id))character()else id));if(!is.null(id))stopifnot(identical(sort(z[[id]]),rep(1:2,each=actual_n)))})
}
# Array shape, missingness, and ownership through all column roles.
for(shape in list(c(3L,0L),c(3L,0L,2L),c(3L,2L,0L),c(3L,2L,1L,2L),c(3L,1L,0L,2L)))for(role in c('dimension','fixed','payload'))for(sort in c('none','first','last')) {
 val<-array(seq_len(prod(shape)),dim=shape); d<-tibble::tibble(g=c('b',NA,'a'),rid=1:3);d[[switch(role,dimension='g',fixed='f',payload='v')]]<-val
 z<-tryCatch(expand_with_margins(d,.by=dplyr::all_of(if(role=='fixed')'f'else character()),.grouping=rollup(g),.margin_label=NULL,.id='sid',.sort=sort),error=identity)
 probe(paste('array',paste(shape,collapse='x'),role,sort),{if(inherits(z,'error'))stop(z);stopifnot(nrow(z)==6L);for(sid in 1:2){sl<-z[z$sid==sid,,drop=FALSE];sl<-sl[order(sl$rid),,drop=FALSE];for(col in names(d)){want<-d[[col]];if(col=='g'&&sid==2L)want<-vctrs::vec_init(want,3L);stopifnot(identical(sl[[col]],want))}}})
}
# Direct data.table input ownership survives key/index metadata and dot-dot aliases.
for(nm in c('g','..0','..-1','.. 1','.data','.env','..marginplyr_column_1'))for(sort in c('none','first','last')) {
 x<-data.table::data.table(g=c('b',NA,'a'),fixed=1:3,payload=3:1);data.table::setnames(x,'g',nm);data.table::setkeyv(x,'fixed');data.table::setindexv(x,'payload');before<-data.table::copy(x)
 z<-tryCatch(expand_with_margins(x,.by=fixed,.grouping=rollup(dplyr::all_of(nm)),.margin_label=NULL,.id='sid',.sort=sort),error=identity)
 probe(paste('data.table',nm,sort),{if(inherits(z,'error'))stop(z);stopifnot(identical(x,before),nrow(z)==6L)})
}
