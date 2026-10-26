# Dated investigation evidence: 2026-10-26, source bfcdaf76a36d102132cd039c68ba8bc05a4fd465.
args <- commandArgs(trailingOnly=TRUE)
repo_root <- if(length(args)) args[[1L]] else '.'
pkgload::load_all(repo_root,quiet=TRUE)
suppressPackageStartupMessages(library(dplyr))
source <- data.table::data.table(
 i=c(1L,2L,NA_integer_), d=c(0.5,1.5,NA_real_), c=c('x','y',NA_character_),
 l=c(TRUE,FALSE,NA), f=factor(c('u','v',NA),levels=c('u','v','unused')),
 day=as.Date('2026-01-01')+0:2)
source_copy<-data.table::copy(source)
reads<-0L
tick<-function(x){reads<<-reads+length(x);rep(TRUE,length(x))}
steps<-list()
for(immutable in c(TRUE,FALSE)){
 root<-dtplyr::lazy_dt(source,immutable=immutable)
 prefix<-if(immutable)'immutable' else 'mutable'
 variants<-list(root=root, filter=filter(root,tick(i)),
  select=select(filter(root,tick(i)),c,i,f), rename=rename(filter(root,tick(i)),new_c=c),
  reorder=relocate(filter(root,tick(i)),f),arrange=arrange(filter(root,tick(i)),desc(i)),
  int_to_double=mutate(root,i=i/2),double_to_int=mutate(root,d=i),
  factor_copy=mutate(root,c=f),char_copy=mutate(root,f=c),
  arithmetic_new=mutate(root,j=i+d),constant_new=mutate(root,j=1L),
  constant_overwrite=mutate(root,i='x'),value_dependent=mutate(root,j=ifelse(i>1,'a',2)))
 for(n in names(variants))steps[[paste(prefix,n,sep='_')]]<-variants[[n]]
}
preds<-list(integer=is.integer,numeric=is.numeric,character=is.character,factor=is.factor,logical=is.logical)
clone_graph<-function(s){for(field in names(s)){if(inherits(s[[field]],'dtplyr_step'))s[[field]]<-clone_graph(s[[field]])};if(inherits(s,'dtplyr_step_first'))s[['parent']]<-data.table::copy(s[['parent']]);s}
comparisons<-0L;refusals<-0L
for(n in names(steps)){
 s<-steps[[n]]; before_reads<-reads
 ans<-lapply(preds,function(p)tryCatch(inspect_grouping(s,.grouping=grouping_set(where(p)),.format='list'),error=identity))
 stopifnot(reads==before_reads,identical(source,source_copy))
 if(all(vapply(ans,inherits,logical(1),'marginplyr_error'))){refusals<-refusals+1L;cat('REFUSED:',n,'\n');next}
 dat<-collect(clone_graph(s))
 for(p in names(preds)){
  exp<-inspect_grouping(dat,.grouping=grouping_set(where(preds[[p]])),.format='list')
  stopifnot(identical(ans[[p]],exp))
  comparisons<-comparisons+1L
 }
 stopifnot(identical(source,source_copy))
}
# Plain join and set-operation graphs with both roots mutable.
left<-data.table::data.table(k=1:3,a=c('a','b','c'),v=1:3)
right<-data.table::data.table(k=2:4,b=c(TRUE,FALSE,TRUE),w=c(2,3,4))
left_copy<-data.table::copy(left);right_copy<-data.table::copy(right)
l<-dtplyr::lazy_dt(left,immutable=FALSE);r<-dtplyr::lazy_dt(right,immutable=FALSE)
graphs<-list(inner=inner_join(l,r,by='k'),left=left_join(l,r,by='k'),full=full_join(l,r,by='k'),semi=semi_join(l,r,by='k'),anti=anti_join(l,r,by='k'),union=union_all(l,l),intersect=intersect(l,l),setdiff=setdiff(l,l))
for(n in names(graphs)){
 s<-graphs[[n]]
 for(p in preds){
  actual<-inspect_grouping(s,.by=where(p),.format='list')
  stopifnot(identical(left,left_copy),identical(right,right_copy))
  expected<-inspect_grouping(collect(clone_graph(s)),.by=where(p),.format='list')
  stopifnot(identical(actual,expected),identical(left,left_copy),identical(right,right_copy))
  comparisons<-comparisons+1L
 }
}
cat('PASS:',length(steps),'derived steps;',length(graphs),'multi-input graphs;',comparisons,'typed proxy comparisons;',refusals,'refusals (including two setorder gaps); no inspection source reads or caller mutations\n')
