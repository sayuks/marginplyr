pkgload::load_all(quiet=TRUE)
set.seed(6539)
data <- as.data.frame(setNames(rep(list(c(1,2)), 6), c('a','b','c','d','e','f')))
ctor <- c(set='grouping_set', sets='grouping_sets', rollup='rollup', cube='cube', product='grouping_spec')
canon <- function(xs) { dims <- unique(unlist(xs, use.names=FALSE)); lapply(xs, function(x) dims[dims %in% x]) }
leaf <- function(cols) { list(expr=rlang::call2('c', !!!rlang::syms(cols)), sets=list(cols), units=as.list(cols)) }
make <- function(depth=0L) {
 kind <- sample(if(depth>2) c('set','rollup','cube') else names(ctor), 1)
 n <- sample(if(kind %in% c('set','product')) 0:3 else 1:3,1)
 args <- lapply(seq_len(n),function(i) {
   if(kind %in% c('sets','product') && depth<3 && runif(1)<0.7) make(depth+1L)
   else if(kind %in% c('rollup','cube') && runif(1)<0.4) {
    cols <- sample(names(data),sample(1:3,1)); list(expr=rlang::call2('grouping_set', !!!rlang::syms(cols)),sets=list(cols),units=list(cols))
   } else leaf(sample(names(data), sample(1:3,1)))
 })
 expr <- rlang::call2(ctor[[kind]], !!!lapply(args,`[[`,'expr'))
 if(kind=='set') sets <- list(unique(unlist(lapply(args,function(a) a$sets[[1]]), use.names=FALSE)))
 else if(kind=='sets') sets <- unlist(lapply(args,`[[`,'sets'),recursive=FALSE)
 else if(kind=='product') {
   sets <- list(character())
   for(a in args) sets <- unlist(lapply(sets,function(x) lapply(a$sets,function(y) unique(c(x,y)))),recursive=FALSE)
 } else {
   units <- unlist(lapply(args,`[[`,'units'),recursive=FALSE)
   if(kind=='rollup') sets <- lapply(rev(0:length(units)),function(i) unique(unlist(units[seq_len(i)],use.names=FALSE)))
   else sets <- unlist(lapply(rev(seq_along(units)), function(k) lapply(combn(seq_along(units), k, simplify=FALSE), function(ii) unique(unlist(units[ii], use.names=FALSE)))), recursive=FALSE); if(kind=='cube') sets <- c(sets,list(character()))
 }
 sets <- lapply(sets,function(s) if(is.null(s)) character() else s)
 list(expr=expr,sets=canon(sets),units=NULL)
}
for (i in 1:2000) {
 a <- make(); spec <- eval(a$expr)
 expected <- canon(a$sets)
 actual <- inspect_grouping(data,.grouping=spec,.duplicates='keep',.format='list')
 if(!identical(actual$included,expected)) {print(i); print(a$expr); bad <- which(!vapply(seq_along(expected),function(j) identical(expected[[j]],actual$included[[j]]),logical(1))); print(head(bad)); print(expected[head(bad)]); print(actual$included[head(bad)]); stop('oracle mismatch')}
 dims <- unique(unlist(expected, use.names=FALSE))
 ids <- vapply(expected,function(x) as.integer(sum(2^rev(seq_along(dims)-1)*(!dims %in% x))),integer(1))
 stopifnot(identical(actual$grouping_id,ids),identical(actual$set_id,seq_along(expected)))
 if(i%%200==0) cat(i,'plans PASS\n')
}
