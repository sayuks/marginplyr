# Dated investigation evidence: 2026-10-26, source bfcdaf76a36d102132cd039c68ba8bc05a4fd465.
args <- commandArgs(trailingOnly=TRUE)
repo_root <- if(length(args)) args[[1L]] else '.'
pkgload::load_all(repo_root,quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
# Private readers are diagnostic probes; the separate reproduction asserts
# the public behavior. Their representation is not a compatibility promise.
isolated_proxy <- utils::getFromNamespace("isolated_dtplyr_proxy", "marginplyr")
metadata_safe <- utils::getFromNamespace("dtplyr_metadata_safe_step", "marginplyr")
rows_read<-0L
tick<-function(x){rows_read<<-rows_read+length(x);rep(TRUE,length(x))}
schema<-function(x) lapply(as.list(x),function(col)list(typeof=typeof(col),class=class(col),levels=levels(col)))
for(immutable in c(TRUE,FALSE)){
 src<-data.table::data.table(g=factor(c('b','a'),levels=c('a','b','unused')),v=1:2,d=as.Date('2026-01-01')+0:1)
 snapshot<-data.table::copy(src)
 x<-dtplyr::lazy_dt(src,immutable=immutable)
 variants<-list(root=x,filter=filter(x,tick(v)),arrange=arrange(x,g),filter_arrange=arrange(filter(x,tick(v)),g))
 for(n in names(variants)){
  s<-variants[[n]]; before<-rows_read
  ans<-tryCatch(inspect_grouping(s,.grouping=grouping_set(where(is.factor))),error=identity)
  stopifnot(rows_read==before,identical(src,snapshot))
  cat(if(immutable)'immutable' else 'mutable',n,':',if(inherits(ans,'error'))class(ans)[[1L]] else 'accepted','\n')
 }
 s<-variants$filter_arrange
 before<-rows_read
 p<-isolated_proxy(s)
 stopifnot(rows_read==before,identical(src,snapshot),identical(schema(p),schema(src)))
 cat('isolated sorted-filter proxy faithful; source reads:',rows_read-before,'\n')
 # Explicit collection operates on a deep-copied root so the mutable control
 # cannot change the source subsequently inspected by this harness.
 s[['parent']][['parent']][['parent']]<-data.table::copy(src)
 typed<-collect(s)
 stopifnot(rows_read==before+2L,identical(src,snapshot),identical(schema(p),schema(typed)))
 cat('explicit collect source reads:',rows_read-before,'; schema matches proxy\n')
}
root<-dtplyr::lazy_dt(tibble(g=c('b','a'),v=1:2,z=c(TRUE,FALSE)))
parents<-list(filter=filter(root,v>0),select=select(root,g,v),rename=rename(root,new_v=v),relocate=relocate(root,z),mutate=mutate(root,j=v+1L),group=ungroup(group_by(root,g)),join=left_join(root,tibble(g=c('a','b'),extra=1:2),by='g'))
for(n in names(parents)){
 s<-arrange(parents[[n]],g)
 cat('parent',n,':',paste(class(s),collapse='/'),'fun=',s[['fun']],'; safe=',metadata_safe(s),'\n')
}
