pkgload::load_all('.',quiet=TRUE)
options(lifecycle_verbosity='quiet')
set.seed(314159)
cols<-letters[1:5]
data<-as.data.frame(setNames(lapply(seq_along(cols),function(i)c(i,i+1L)),cols))
leaf<-function(nonempty=FALSE) {
  selected<-sample(cols,sample(if(nonempty)1:5 else 0:5,1))
  list(expr=rlang::call2('c',!!!rlang::syms(selected)), sets=list(selected), unit=selected)
}
gen<-function(depth=0L) {
 kind<-if(depth>3L)'set' else sample(c('set','sets','rollup','cube','product'),1)
 n<-sample(if(kind%in%c('set','product'))0:3 else 1:3,1)
 if(kind%in%c('rollup','cube')) {
  children<-replicate(n,{x<-leaf(TRUE);if(runif(1)<0.4)x$expr<-rlang::call2('grouping_set',x$expr);x},simplify=FALSE)
  units<-unlist(lapply(children,function(x)if(rlang::call_name(x$expr)=='grouping_set')list(x$unit) else as.list(x$unit)),recursive=FALSE)
  if(kind=='rollup') sets<-lapply(rev(seq.int(0,length(units))),function(i)unique(unlist(units[seq_len(i)],use.names=FALSE)))
  else { indices<-unlist(lapply(rev(seq.int(0,length(units))),function(i)combn(seq_along(units),i,simplify=FALSE)),recursive=FALSE);sets<-lapply(indices,function(ii)unique(unlist(units[ii],use.names=FALSE))) }
 } else {
  children<-replicate(n,if(kind=='set'||runif(1)<0.4)leaf()else gen(depth+1L),simplify=FALSE)
  if(kind=='set')sets<-list(unique(unlist(lapply(children,function(x)x$sets[[1L]]),use.names=FALSE)))
  if(kind=='sets')sets<-unlist(lapply(children,function(x)x$sets),recursive=FALSE)
  if(kind=='product') {sets<-list(character());for(x in children)sets<-unlist(lapply(sets,function(a)lapply(x$sets,function(b)unique(c(a,b)))),recursive=FALSE)}
 }
 list(expr=rlang::call2(switch(kind,set='grouping_set',sets='grouping_sets',product='grouping_spec',kind),!!!lapply(children,function(x)x$expr)),sets=sets)
}
for(i in seq_len(1500)) {
  tree<-gen();s<-eval(tree$expr)
  actual<-inspect_grouping(data,.grouping=s,.duplicates='keep',.format='list')
  dims<-unique(unlist(tree$sets,use.names=FALSE))
  expected<-lapply(tree$sets,function(x)as.character(dims[dims%in%x]))
  if(!identical(actual$included,expected)) {print(tree$expr);str(actual$included);str(expected);stop('mismatch')}
  dropped<-inspect_grouping(data,.grouping=s,.duplicates='drop',.format='list')
  stopifnot(identical(dropped$included,expected[!duplicated(vapply(expected,paste,'',collapse=','))]))
}
cat('1500 generated legal Grouping specifications matched independent expansion/dedup oracle\n')
