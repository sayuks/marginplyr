# Dated investigation evidence: 2026-10-26, source bfcdaf76a36d102132cd039c68ba8bc05a4fd465.
# Investigation harness; repository remains read-only.
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args)) args[[1L]] else '.'
pkgload::load_all(repo, quiet = TRUE)
set.seed(104728)
x <- tibble::tibble(a = 1:3, b = c('x','y','z'), c = factor(c('q','r','q')), d = c(TRUE,FALSE,NA), aa = c(1,2,3), ab = c('s','s','t'), `a b` = list(1,2,3), `a,b` = as.Date('2020-01-01') + 0:2, `a|b` = 3:1, `a\nb` = letters[1:3], `Ω` = 1:3, `a\\b` = complex(real = 1:3))
cn <- names(x)
selection_pool <- list(quote(a), quote(c), quote(a:aa), quote(-b), quote(!b), quote(c(a,b,a)), quote(c()), quote(NULL), quote(1:3), quote(c(1L,3L)), quote(everything()), quote(starts_with('a')), quote(ends_with('b')), quote(contains('a')), quote(matches('^[ab]$')), quote(where(is.numeric)), quote(where(is.factor)), quote(where(is.list)), quote(where(is.logical)), quote(where(is.character)), quote(where(is.complex)), quote(all_of(c('a b','a,b','a|b','a\nb'))), quote(any_of(c('absent','Ω','a\\b'))), quote(all_of(character())), quote(1 & everything()), quote(c(a:b) | where(is.factor)), quote(everything() & !where(is.character)), quote(a:ab / b:c), quote((where(is.numeric))), quote(c(a = a)), quote(dplyr::all_of(c(a='a'))), quote(c(a, where(is.factor))), quote(last_col(1)), quote(num_range('a', 1:3)))
sel <- function() {e <- sample(selection_pool, 1)[[1]]; v <- tidyselect::eval_select(rlang::new_quosure(e, globalenv()), x); list(expr=e, fam=list(unname(names(v))), unit=FALSE)}
set_node <- function() {k <- sample(0:3,1); ch <- lapply(seq_len(k), function(i) sel()); cols <- unique(unlist(lapply(ch, function(z) z$fam[[1]]), use.names=FALSE)); if(is.null(cols)) cols <- character(); list(expr=rlang::call2('grouping_set', !!!lapply(ch, `[[`, 'expr')), fam=list(cols), unit=TRUE)}
random_tree <- function(depth=0L) {
  kind <- sample(if(depth > 2L) c('set','rollup','cube') else c('set','sets','rollup','cube','product'), 1)
  if(kind=='set') return(set_node())
  k <- sample(if(kind=='product') 0:3 else 1:3, 1)
  if(kind %in% c('rollup','cube')) {
    ch <- lapply(seq_len(k), function(i) if(sample(c(TRUE,FALSE),1)) set_node() else sel())
    units <- unlist(lapply(ch, function(z) if(z$unit) z$fam else lapply(z$fam[[1]], function(v) v)), recursive=FALSE)
    invalid <- any(vapply(ch, function(z) z$unit && !length(z$fam[[1]]), logical(1))) || !length(units)
    if(invalid) return(list(expr=rlang::call2(kind, !!!lapply(ch,`[[`,'expr')), invalid=TRUE))
    if(kind=='cube' && length(units)>8L) return(random_tree(depth))
    idx <- if(kind=='rollup') lapply(length(units):0, seq_len) else unlist(lapply(length(units):0, function(n) combn(seq_along(units),n,simplify=FALSE)),recursive=FALSE)
    fam <- lapply(idx, function(i) {v <- unique(unlist(units[i],use.names=FALSE)); if(is.null(v)) character() else v})
    return(list(expr=rlang::call2(kind, !!!lapply(ch,`[[`,'expr')),fam=fam))
  }
  ch <- lapply(seq_len(k), function(i) if(sample(0:3,1)) random_tree(depth+1L) else sel())
  expr <- rlang::call2(if(kind=='sets') 'grouping_sets' else 'grouping_spec', !!!lapply(ch,`[[`,'expr'))
  if(any(vapply(ch, function(z) isTRUE(z$invalid), logical(1)))) return(list(expr=expr, invalid=TRUE))
  if(kind=='product' && prod(vapply(ch,function(z) length(z$fam),integer(1)))>1000) return(random_tree(depth))
  fam <- if(kind=='sets') unlist(lapply(ch,`[[`,'fam'),recursive=FALSE) else Reduce(function(l,r) unlist(lapply(l,function(a) lapply(r,function(b) unique(c(a,b)))),recursive=FALSE),lapply(ch,`[[`,'fam'),init=list(character()))
  list(expr=expr, fam=fam)
}
checks <- 0L; rejected <- 0L
for(i in 1:2000) {
  z <- random_tree(); spec <- eval(z$expr)
  if(isTRUE(z$invalid)) {got <- tryCatch(inspect_grouping(x,.grouping=spec,.duplicates='keep',.format='list'),error=identity); if(!inherits(got,'marginplyr_error')) {print(z$expr);stop('invalid accepted / wrong error')}; rejected <- rejected+1L; next}
  dims <- unique(unlist(z$fam,use.names=FALSE)); if(is.null(dims)) dims <- character()
  exp <- lapply(z$fam,function(s) dims[dims %in% s])
  for(policy in c('keep','drop')) {
    keep <- if(policy=='keep') rep(TRUE,length(exp)) else !duplicated(lapply(exp,sort))
    got <- inspect_grouping(x,.grouping=spec,.duplicates=policy,.format='list')
    if(!identical(got$included,exp[keep]) || !identical(got$set_id,seq_len(sum(keep)))) {print(z$expr); print(exp); print(got); stop(i)}
    checks <- checks+1L
  }
  if(i %% 100==0) cat('passed AST',i,'inspection checks',checks,'valid grammar refusals',rejected,'\n')
}
cat('PASS: ',checks,' inspect calls, ',rejected,' expected grammar refusals\n',sep='')
