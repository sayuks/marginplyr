pkgload::load_all('.', quiet=TRUE)
set.seed(20261028)
data <- data.frame(a=1L,b='b',c=TRUE,d=1.5)
cols <- names(data)
mk <- function(depth=0L) {
  kind <- sample(c('set','sets','rollup','cube','product'),1)
  n <- sample(if(kind %in% c('sets','rollup','cube')) 1:3 else 0:3,1)
  args <- lapply(seq_len(n), function(i) {
    if (depth < 3L && kind %in% c('sets','product') && runif(1) < 0.6) return(mk(depth+1L))
    if (kind %in% c('rollup','cube') && runif(1) < 0.35) {
      return(list(kind='set',args=list(sample(cols,sample(1:4,1)))))
    }
    sample(cols, sample(if(kind %in% c('rollup','cube')) 1:4 else 0:4,1))
  })
  list(kind=kind,args=args)
}
cn <- c(set='grouping_set',sets='grouping_sets',rollup='rollup',cube='cube',product='grouping_spec')
expr <- function(node) rlang::call2(cn[[node$kind]], !!!lapply(node$args,function(a) if(is.list(a)) expr(a) else a))
expand <- function(node) {
  family <- lapply(node$args,function(a) if(is.list(a)) expand(a) else list(a))
  if(node$kind=='set') return(list(unique(unlist(family,use.names=FALSE))))
  if(node$kind=='sets') return(unlist(family,recursive=FALSE))
  if(node$kind=='product') {
    out <- list(character())
    for(f in family) out <- unlist(lapply(out,function(l)lapply(f,function(r)unique(c(l,r)))),recursive=FALSE)
    return(out)
  }
  units<-unlist(lapply(node$args,function(a)if(is.list(a))expand(a) else lapply(a,identity)),recursive=FALSE)
  if(node$kind=='rollup') return(lapply(rev(0:length(units)),function(n) unique(unlist(units[seq_len(n)],use.names=FALSE))))
  ids<-unlist(lapply(rev(0:length(units)),function(n)combn(seq_along(units),n,simplify=FALSE)),recursive=FALSE)
  lapply(ids,function(idx)unique(unlist(units[idx],use.names=FALSE)))
}
for(i in 1:1000) {
  node<-mk(); ex<-expr(node); expected<-expand(node)
  dimensions<-unique(unlist(expected,use.names=FALSE))
  expected<-lapply(expected,function(s) dimensions[dimensions %in% s])
  expected<-lapply(expected,function(s)if(is.null(s))character() else s)
  actual<-tryCatch(inspect_grouping(data,.grouping=eval(ex),.duplicates='keep',.format='list')$included,error=identity)
  if(!identical(actual,expected)) {print(list(i=i,ex=ex,actual=actual,expected=expected));quit(status=1)}
}
cat('1000 random valid specification trees matched independent oracle\n')
