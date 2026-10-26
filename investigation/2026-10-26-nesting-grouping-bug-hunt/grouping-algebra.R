# Dated investigation evidence, investigated 2026-10-26 at 6b91254016dbcce60cc2cc69071f01f69eb8084e.
# Run from the repository root, or pass its path as the first argument.
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1L]] else "."
pkgload::load_all(repo, quiet = TRUE)
set.seed(20261026)
cols <- letters[1:5]
d <- as.data.frame(setNames(rep(list(1:3),length(cols)),cols))
seq0 <- function(n) if(n) seq_len(n) else integer()
unit <- function(x) list(expr=rlang::call2('grouping_set',!!!rlang::syms(x)),fam=list(x))
leaf <- function() {x <- sample(cols,sample(0:5,1)); list(expr=rlang::call2('c',!!!rlang::syms(x)),fam=list(x))}
gen <- function(depth=0) {
  kind <- if(depth>=3) sample(c('leaf','set'),1) else sample(c('leaf','set','sets','rollup','cube','product'),1)
  if(kind=='leaf') return(leaf())
  if(kind=='set') return(unit(sample(cols,sample(0:5,1))))
  k <- sample(if(kind=='product') 0:3 else 1:3,1)
  if(kind %in% c('rollup','cube')) {
    u <- lapply(seq0(k),function(i) sample(cols,sample(1:3,1)))
    expr <- rlang::call2(kind,!!!lapply(u,function(x) unit(x)$expr))
    idx <- if(kind=='rollup') lapply(k:0,seq0) else unlist(lapply(k:0,function(s) utils::combn(seq_len(k),s,simplify=FALSE)),recursive=FALSE)
    fam <- lapply(idx,function(i) unique(unlist(u[i],use.names=FALSE)))
    return(list(expr=expr,fam=fam))
  }
  kids <- lapply(seq0(k),function(i) gen(depth+1))
  expr <- rlang::call2(if(kind=='sets') 'grouping_sets' else 'grouping_spec',!!!lapply(kids,`[[`,'expr'))
  fam <- if(kind=='sets') unlist(lapply(kids,`[[`,'fam'),recursive=FALSE) else Reduce(function(l,r) unlist(lapply(l,function(a)lapply(r,function(b)unique(c(a,b)))),recursive=FALSE),lapply(kids,`[[`,'fam'),init=list(character()))
  list(expr=expr,fam=fam)
}
for(i in 1:1500) {
  x <- gen()
  spec <- if(rlang::call_name(x$expr)=='c') rlang::call2('grouping_set',x$expr) else x$expr
  dims <- unique(unlist(x$fam,use.names=FALSE)); if(is.null(dims))dims <- character()
  expected <- lapply(x$fam,function(f) dims[dims %in% f])
  p <- inspect_grouping(d,.grouping=eval(spec),.duplicates='keep',.format='list')
  if(!identical(p$included,expected)) {print(spec);str(expected);str(p);stop(i)}
  keys <- vapply(expected,paste,character(1),collapse=',')
  drop <- inspect_grouping(d,.grouping=eval(spec),.duplicates='drop',.format='list')
  stopifnot(identical(drop$included,expected[!duplicated(keys)]),identical(drop$set_id,seq_len(nrow(drop))))
  if(i %% 250==0) cat('passed',i,'\n')
}
