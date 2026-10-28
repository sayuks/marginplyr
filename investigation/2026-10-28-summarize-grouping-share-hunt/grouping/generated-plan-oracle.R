args <- commandArgs(trailingOnly = TRUE)
if (length(args)) setwd(args[[1L]])
pkgload::load_all(quiet=TRUE)
set.seed(20261028)
vars <- c('a','b','c','a,b','a`b','日本語','a\nb')
data <- setNames(as.data.frame(rep(list(1:2), length(vars))), vars)
data$fixed <- c('x','y')
leaf <- function(v=sample(vars,sample(0:3,1))) list(expr=rlang::call2('grouping_set',rlang::call2('all_of',v)), sets=list(v), unit=v)
combine <- function(kind, children) {
 expr <- rlang::call2(kind, !!!lapply(children,`[[`,'expr'))
 sets <- switch(kind,
 grouping_sets=unlist(lapply(children,`[[`,'sets'),recursive=FALSE),
 grouping_spec={p<-list(character()); for(ch in children) p<-unlist(lapply(p,function(l)lapply(ch$sets,function(r)unique(c(l,r)))),recursive=FALSE);p},
 rollup=lapply(rev(seq.int(0L,length(children))),function(n)unique(unlist(lapply(children[seq_len(n)],`[[`,'unit'),use.names=FALSE))),
 cube={ix<-unlist(lapply(rev(seq.int(0L,length(children))),function(n)combn(seq_along(children),n,simplify=FALSE)),recursive=FALSE);lapply(ix,function(ii)unique(unlist(lapply(children[ii],`[[`,'unit'),use.names=FALSE)))})
 list(expr=expr,sets=sets)
}
node <- function(depth=0) {
 k<-if(depth>2) 'grouping_set' else sample(c('grouping_set','grouping_sets','grouping_spec','rollup','cube'),1)
 if(k=='grouping_set') return(leaf())
 n<-sample(if(k=='grouping_spec')0:3 else 1:3,1)
 ch<-if(k %in% c('rollup','cube')) lapply(seq_len(n),function(i)leaf(sample(vars,sample(1:3,1)))) else lapply(seq_len(n),function(i)node(depth+1))
 combine(k,ch)
}
accepted <- 0L; skipped <- 0L
for(i in 1:1500) {
 n<-node(); if(length(n$sets)>1000) { skipped <- skipped + 1L; next }; accepted <- accepted + 1L
 spec<-eval(n$expr)
 actual<-inspect_grouping(data,.by=fixed,.grouping=spec,.duplicates='keep',.format='list')
 dims<-unique(unlist(n$sets,use.names=FALSE))
 expected<-lapply(n$sets,function(s)dims[dims %in% s])
 if(!identical(actual$included,expected)) {print(n$expr);print(expected);print(actual);stop('Mismatch at ',i)}
 stopifnot(identical(actual$fixed,rep(list('fixed'),length(n$sets))))
 for(j in seq_along(n$sets))stopifnot(identical(unname(actual$grouping_bits[[j]]),as.integer(!dims %in% n$sets[[j]])))
}
cat('PASS: attempts=1500 accepted=', accepted, ' skipped=', skipped, '; independent specification oracle\n',sep='')
