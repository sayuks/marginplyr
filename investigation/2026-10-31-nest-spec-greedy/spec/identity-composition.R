pkgload::load_all('.',quiet=TRUE)
set.seed(31103147)
dim_names <- c('a','b','日本語','x,y','x=y','odd`name')
d <- tibble::as_tibble(setNames(rep(list(c('u','v','u')),length(dim_names)),dim_names),.name_repair='minimal')
d$fixed <- c('f','f','g')
d$payload <- 1:3
leaf <- function() sample(dim_names,sample(0:3,1L))
node <- function(depth=0L) {
  kind <- if(depth>=2L) 'set' else sample(c('set','sets','rollup','cube','product'),1L)
  if(kind=='set') return(list(kind=kind,args=replicate(sample(0:2,1L),leaf(),simplify=FALSE)))
  if(kind %in% c('rollup','cube')) {
    args <- replicate(sample(1:3,1L), {
      cols <- sample(dim_names,sample(1:2,1L))
      if(runif(1)<0.4) list(kind='set',args=list(cols)) else cols
    },simplify=FALSE)
  } else args <- replicate(sample(if(kind=='sets') 1:3 else 0:3,1L),if(runif(1)<0.7) node(depth+1L) else leaf(),simplify=FALSE)
  list(kind=kind,args=args)
}
expand <- function(n) {
  kind <- n$kind
  families <- lapply(n$args,function(x) if(is.list(x)) expand(x) else list(x))
  if(kind=='set') return(list(unique(unlist(n$args,use.names=FALSE))))
  if(kind=='sets') return(unlist(families,recursive=FALSE))
  if(kind=='product') {
    result <- list(character())
    for(family in families) result <- unlist(lapply(result,function(l) lapply(family,function(r) unique(c(l,r)))),recursive=FALSE)
    return(result)
  }
  units <- unlist(lapply(n$args,function(x) if(is.list(x)) list(expand(x)[[1L]]) else lapply(x,identity)),recursive=FALSE)
  if(kind=='rollup') return(lapply(rev(seq.int(0L,length(units))),function(k) unique(unlist(units[seq_len(k)],use.names=FALSE))))
  indices <- unlist(lapply(rev(seq.int(0L,length(units))),function(k) combn(seq_along(units),k,simplify=FALSE)),recursive=FALSE)
  lapply(indices,function(ix) unique(unlist(units[ix],use.names=FALSE)))
}
syntax <- function(n) {
  constructors <- c(set='grouping_set',sets='grouping_sets',rollup='rollup',cube='cube',product='grouping_spec')
  args <- lapply(n$args,function(x) if(is.list(x)) syntax(x) else rlang::expr(tidyselect::all_of(!!x)))
  rlang::call2(constructors[[n$kind]],!!!args)
}
counts <- c(plans=0L,helper_rows=0L,duplicate_errors=0L)
for(iteration in 1:300) {
  n <- node()
  family <- lapply(expand(n),function(x) if(is.null(x)) character() else x)
  if(length(family)>1024L) next
  dimensions <- unique(unlist(family,use.names=FALSE))
  if(is.null(dimensions)) dimensions <- character()
  family <- lapply(family,function(s) dimensions[dimensions %in% s])
  spec <- eval(syntax(n))
  for(policy in c('drop','keep','error')) {
    actual <- tryCatch(inspect_grouping(d,.by=fixed,.grouping=spec,.format='list',.duplicates=policy),error=identity)
    if(policy=='error' && anyDuplicated(family)) {
      stopifnot(inherits(actual,'marginplyr_error')); counts['duplicate_errors'] <- counts['duplicate_errors']+1L; next
    }
    expected <- if(policy=='drop') unique(family) else family
    if(inherits(actual,'error') || !identical(actual$included,expected)) {print(n);print(expected);print(actual);stop('plan mismatch')}
    stopifnot(identical(actual$fixed,rep(list('fixed'),length(expected))))
    expected_bits <- lapply(expected,function(s) if(length(dimensions)) setNames(as.integer(!dimensions %in% s),dimensions) else integer())
    stopifnot(identical(actual$grouping_bits,expected_bits))
    counts['plans'] <- counts['plans']+1L
    if(policy!='drop') next
    vars <- sample(c('fixed',dimensions),sample(seq_along(c('fixed',dimensions)),1L))
    helper <- rlang::call2('grouping_id',!!!lapply(vars,rlang::sym))
    bits <- setNames(lapply(c('fixed',dimensions),function(x) rlang::call2('grouping_bit',rlang::sym(x))),paste0('bit',seq_along(c('fixed',dimensions))))
    result <- summarize_with_margins(d,count=dplyr::n(),chosen=!!helper,full=grouping_id(),!!!bits,.by=fixed,.grouping=spec,.duplicates='drop',.id='set',.margin_label=NULL)
    for(row in seq_len(nrow(result))) {
      set <- result$set[[row]]
      included <- c('fixed',expected[[set]])
      mask <- as.integer(!vars %in% included)
      wanted <- as.integer(sum(mask*2^rev(seq.int(0L,length(vars)-1L))))
      stopifnot(identical(result$chosen[[row]],wanted),identical(result$full[[row]],actual$grouping_id[[set]]))
      stopifnot(identical(as.integer(result[row,names(bits)]),as.integer(!c('fixed',dimensions) %in% included)))
      counts['helper_rows'] <- counts['helper_rows']+1L
    }
  }
}
print(counts)
cat('seed=31103147; 300 generated AST attempts; helper columns were selected in independently permuted order and could include the fixed key.\n')
