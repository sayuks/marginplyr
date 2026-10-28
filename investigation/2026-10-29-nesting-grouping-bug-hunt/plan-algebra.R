args <- commandArgs(TRUE)
pkgload::load_all(args[[1]], quiet = TRUE)
set.seed(98127)
cols <- c('a', 'b c', 'd,e', 'x=y')
d <- tibble::as_tibble(setNames(rep(list(1:2), length(cols)), cols))
unioncols <- function(x) unique(unlist(x, use.names = FALSE))
node <- function(depth = 0L) {
  kind <- if(depth > 2L) 'set' else sample(c('set','sets','rollup','cube','product'), 1)
  n <- sample(1:3, 1)
  if(kind == 'set') {
    chosen <- sample(cols, sample(0:4, 1))
    return(list(expr = rlang::call2('grouping_set', !!!lapply(chosen, rlang::sym)), sets = list(chosen)))
  }
  if(kind %in% c('rollup', 'cube')) {
    units <- replicate(n, sample(cols, sample(1:2, 1)), simplify = FALSE)
    exprs <- lapply(units, function(u) if(length(u)==1L) rlang::sym(u) else rlang::call2('grouping_set', !!!lapply(u, rlang::sym)))
    indices <- if(kind=='rollup') lapply(n:0, seq_len) else unlist(lapply(n:0, function(k) combn(seq_len(n), k, simplify=FALSE)), recursive=FALSE)
    return(list(expr = rlang::call2(kind, !!!exprs), sets = lapply(indices, function(i) if(length(i)) unioncols(units[i]) else character())))
  }
  ns <- replicate(n, node(depth+1L), simplify=FALSE)
  expr <- rlang::call2(if(kind=='sets') 'grouping_sets' else 'grouping_spec', !!!lapply(ns, `[[`, 'expr'))
  families <- lapply(ns, `[[`, 'sets')
  if(kind=='sets') sets <- unlist(families, recursive=FALSE) else {
    sets <- list(character())
    for(f in families) sets <- unlist(lapply(sets, function(x) lapply(f, function(y) unique(c(x,y)))), recursive=FALSE)
  }
  list(expr=expr, sets=sets)
}
for(i in seq_len(1200)) {
  nd <- node()
  dimensions <- unioncols(nd$sets)
  expected <- lapply(nd$sets, function(s) dimensions[dimensions %in% s])
  spec <- eval(nd$expr)
  got <- inspect_grouping(d, .grouping=spec, .duplicates='keep', .format='list')
  if(!identical(got$included, expected)) {print(nd); print(got); stop('membership mismatch ', i)}
  expected_masks <- lapply(expected, function(s) setNames(as.integer(!dimensions %in% s), dimensions))
  if(!length(dimensions)) expected_masks <- rep(list(integer()), length(expected))
  stopifnot(identical(got$grouping_bits, expected_masks), identical(got$set_id, seq_along(expected)))
  drop <- inspect_grouping(d, .grouping=spec, .duplicates='drop', .format='list')
  stopifnot(identical(drop$included, expected[!duplicated(expected)]))
}
cat('PASS: 1200 generated specification trees, keep/drop membership, plan order, bits, IDs\n')
