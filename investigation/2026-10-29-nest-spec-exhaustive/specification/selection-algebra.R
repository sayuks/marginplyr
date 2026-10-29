pkgload::load_all('.', quiet = TRUE)
set.seed(291029701)
data <- tibble::tibble(a = 1L, b = 'b', c = TRUE, d = 1.2)
constructors <- c(set='grouping_set',sets='grouping_sets',rollup='rollup',cube='cube',product='grouping_spec')
selectors <- list(quote(a), quote(b), quote(c(a,c)), quote(c(d,b,a)), quote(-a), quote(a:d), quote(!c(a,d)), quote(a:d & c(a,c)), quote(c(a,b)/b), quote(tidyselect::all_of(c('d','b'))), quote(tidyselect::where(is.character)), quote(tidyselect::matches('[ac]')), quote(NULL), quote(integer()), quote(c(1L,3L)))
leaf <- function() {
  i <- sample(seq_along(selectors), 1L)
  list(expr=selectors[[i]], cols=names(tidyselect::eval_select(selectors[[i]], data)))
}
mk <- function(depth=0L) {
  kind <- sample(names(constructors), 1L)
  n <- sample(if (kind %in% c('sets','rollup','cube')) 1:3 else 0:3,1L)
  args <- lapply(seq_len(n), function(i) {
    if(depth < 3L && kind %in% c('sets','product') && runif(1L)<0.55) return(mk(depth+1L))
    if(kind %in% c('rollup','cube') && runif(1L)<0.35) return(list(kind='set', args=list(leaf())))
    leaf()
  })
  list(kind=kind,args=args)
}
expr <- function(node) if(is.null(node$kind)) node$expr else rlang::call2(constructors[[node$kind]], !!!lapply(node$args,expr))
expand <- function(node) {
 if(is.null(node$kind)) return(list(node$cols))
 fs <- lapply(node$args,expand)
 if(node$kind=='set') return(list(unique(unlist(fs,use.names=FALSE))))
 if(node$kind=='sets') return(unlist(fs,recursive=FALSE))
 if(node$kind=='product') {
   out<-list(character());for(f in fs) out<-unlist(lapply(out,function(l) lapply(f,function(r) unique(c(l,r)))),recursive=FALSE)
   return(out)
 }
 units<-unlist(lapply(node$args,function(a) if(is.null(a$kind)) lapply(a$cols,identity) else expand(a)),recursive=FALSE)
 if(length(units)==0L || any(lengths(units)==0L)) stop('invalid resolved empty unit')
 if(node$kind=='rollup') return(lapply(rev(seq.int(0L,length(units))),function(n) unique(unlist(units[seq_len(n)],use.names=FALSE))))
 idx<-unlist(lapply(rev(seq.int(0L,length(units))),function(n) combn(seq_along(units),n,simplify=FALSE)),recursive=FALSE)
 lapply(idx,function(i) unique(unlist(units[i],use.names=FALSE)))
}
passed<-rejected<-bounded<-0L
for(i in seq_len(5000L)) {
 node<-mk(); code<-expr(node); ex<-tryCatch(expand(node),error=identity)
 if(!inherits(ex,'error') && length(ex)>2048L) { bounded<-bounded+1L; next }
 spec<-eval(code)
 actual<-tryCatch(inspect_grouping(data,.grouping=spec,.duplicates='keep',.format='list'),error=identity)
 if(inherits(ex,'error')) {
   if(!inherits(actual,'marginplyr_error')) {print(list(i=i,code=code,expected=ex,actual=actual));quit(status=1L)}
   rejected<-rejected+1L;next
 }
 dims<-unique(unlist(ex,use.names=FALSE)); if(is.null(dims)) dims<-character(); ex<-lapply(ex,function(s) dims[dims %in% s]);ex<-lapply(ex,function(s) if(is.null(s)) character() else s)
 if(inherits(actual,'error') || !identical(actual$included,ex)) {print(list(i=i,code=code,expected=ex,actual=actual));quit(status=1L)}
 masks<-lapply(ex,function(s) setNames(as.integer(!dims %in% s),dims))
 if(length(dims)==0L) masks<-rep(list(integer()),length(ex))
 stopifnot(identical(actual$grouping_bits,masks), identical(actual$set_id,seq_along(ex)), identical(actual$omitted,lapply(ex,function(s) dims[!dims %in% s])))
 dropped<-inspect_grouping(data,.grouping=spec,.duplicates='drop',.format='list')
 stopifnot(identical(dropped$included,unique(ex)))
 passed<-passed+1L
}
cat('seed=291029701 attempts=5000 passed=',passed,' expected_empty_unit_refusals=',rejected,' size_bound_exclusions=',bounded,'\n',sep='')
