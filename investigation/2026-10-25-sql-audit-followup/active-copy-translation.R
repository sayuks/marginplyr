# A legitimate dbplyr extension translator reads/copies the current public view.
# No marginplyr or dbplyr implementation binding is replaced.
pkgload::load_all('.',quiet=TRUE)
options(marginplyr.audit_sql=TRUE)
snapshots <- list(); serialized <- list(); touches <- 0L
translate_touch <- function(x) {
  touches <<- touches + 1L
  before <- last_sent_queries(details=TRUE)
  snapshots[[length(snapshots)+1L]] <<- before
  serialized[[length(serialized)+1L]] <<- serialize(before,NULL)
  inspect_grouping(data.frame(z=1))
  stopifnot(identical(last_sent_queries(details=TRUE),before))
  warning('translation owns this warning',call.=FALSE)
  dbplyr::build_sql('TOUCH(',x,')')
}
translation <- function(con) {
  dbplyr::sql_variant(
    dbplyr::sql_translator(.parent=dbplyr::base_scalar,touch=translate_touch),
    dbplyr::base_agg,dbplyr::base_win)
}
registerS3method('sql_translation','ScopeSql',translation,envir=asNamespace('dbplyr'))
remote <- dbplyr::lazy_frame(g='a',v=1,con=dbplyr::simulate_dbi('ScopeSql'))
query <- summarize_with_margins(remote,total=sum(touch(v),na.rm=TRUE),.grouping=rollup(g))
rows <- last_sent_queries(details=TRUE)
stopifnot(touches>0L, nrow(rows)==1L, rows$purpose=='result', rows$status=='prepared', !is.na(rows$sql))
stopifnot(nrow(rows$conditions[[1L]])==touches)
for(i in seq_along(snapshots)) stopifnot(identical(serialize(snapshots[[i]],NULL),serialized[[i]]))
# Replace record, modify newly returned views, read all projections.
inspect_grouping(data.frame(z=1))
for(i in seq_along(snapshots)) stopifnot(identical(serialize(snapshots[[i]],NULL),serialized[[i]]))
cat('PASS active render copy snapshots:',length(snapshots),'; later render conditions:',nrow(rows$conditions[[1L]]),'\n')
