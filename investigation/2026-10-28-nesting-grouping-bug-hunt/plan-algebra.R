# Run from the repository root; exploratory evidence for 2026-10-28.
pkgload::load_all('.',quiet=TRUE)
set.seed(4306)
cols <- c('a','b','c','x,y','x=y','a`b','p\nq','é','É','x\ty','A')
d <- tibble::as_tibble(stats::setNames(rep(list('v'),length(cols)), cols))
leaf <- function(n) list(type='leaf', cols=sample(cols,n))
gen <- function(depth=0) {
 if(depth>2 || runif(1)<0.30) return(leaf(sample(0:3,1)))
 type <- sample(c('set','sets','rollup','cube','product'),1)
 if(type=='set') return(list(type=type,args=lapply(seq_len(sample(0:3,1)),function(i) leaf(sample(0:3,1)))))
 nargs <- if(type %in% c('rollup','cube','sets')) sample(1:3,1) else sample(0:3,1)
 args <- lapply(seq_len(nargs), function(i) {
   if(type %in% c('rollup','cube')) {
    if(runif(1)<0.3) list(type='set',args=list(leaf(sample(1:3,1)))) else leaf(sample(1:3,1))
   } else gen(depth+1)
 })
 list(type=type,args=args)
}
expand <- function(x) {
 if(x$type=='leaf') return(list(x$cols))
 if(x$type=='set') return(list(unique(unlist(lapply(x$args, `[[`, 'cols'),use.names=FALSE))))
 if(x$type=='sets') return(unlist(lapply(x$args,expand),recursive=FALSE))
 if(x$type=='product') {
   y<-list(character())
   for(arg in x$args) y<-unlist(lapply(y,function(l) lapply(expand(arg), function(r) unique(c(l,r)))),recursive=FALSE)
   return(y)
 }
 units<-unlist(lapply(x$args,function(arg) if(arg$type=='leaf') lapply(arg$cols,identity) else expand(arg)),recursive=FALSE)
 if(x$type=='rollup') return(lapply(rev(0:length(units)),function(i) if(i==0) character() else unique(unlist(units[seq_len(i)],use.names=FALSE))))
 indices<-unlist(lapply(rev(0:length(units)),function(n)utils::combn(seq_along(units),n,simplify=FALSE)),recursive=FALSE)
 lapply(indices,function(i)if(!length(i))character() else unique(unlist(units[i],use.names=FALSE)))
}
construct <- function(x, env) {
 if(x$type=='leaf') return(rlang::call2('all_of',x$cols,.ns='tidyselect'))
 ctor<-switch(x$type,set='grouping_set',sets='grouping_sets',product='grouping_spec',x$type)
 args<-lapply(x$args,function(arg) {
   e<-construct(arg,env)
   if(arg$type!='leaf' && runif(1)<0.4) {
     key<-paste0('spec',length(ls(env)))
     assign(key,eval(e,env),env)
     e<-rlang::sym(key)
   }
   for(j in seq_len(sample(0:3,1)))e<-rlang::call2('(',e)
   e
 })
 rlang::call2(ctor,!!!args,.ns='marginplyr')
}
count<-0L
for(i in 1:1000) {
 x<-gen(); if(x$type=='leaf')x<-list(type='set',args=list(x))
 expected<-expand(x); dims<-as.character(unique(unlist(expected,use.names=FALSE))); expected<-lapply(expected,function(s)dims[dims %in% s])
 env<-rlang::env(globalenv()); expr<-construct(x,env); spec<-eval(expr,env)
 for(policy in c('keep','drop')) {
   oracle<-if(policy=='drop')unique(expected)else expected
   actual<-inspect_grouping(d,.grouping=spec,.duplicates=policy,.format='list')
   if(!identical(actual$included,oracle)) {dput(x);print(expr);dput(expected);dput(actual);stop('membership mismatch ',i)}
   masks<-lapply(oracle,function(s)if(length(dims))stats::setNames(as.integer(!dims %in% s),dims)else integer())
   if(!identical(actual$grouping_bits,masks))stop('mask mismatch ',i)
   if(!identical(actual$set_id,seq_along(oracle)))stop('id mismatch ',i)
 }
 count<-count+1L
}
cat('PASS: ',count,' generated trees x keep/drop with parentheses/name binding, unusual names and mask oracle\n',sep='')
