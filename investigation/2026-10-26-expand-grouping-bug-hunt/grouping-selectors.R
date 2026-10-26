# Investigation evidence, 2026-10-26. Run from the repository root.
pkgload::load_all('.', quiet=TRUE)
x <- tibble::tibble(a=1L,b='b',c=TRUE,d=factor('x'),e=list(1))
selected <- c('b','a')
predicate <- is.numeric
helper <- function() names(tidyselect::peek_data())[vapply(tidyselect::peek_data(),is.character,logical(1))]
selections <- alist(
  a, c(b,a), c(a,a), -a, !a, a:c, c:a, a|b, a&b,
  everything(), last_col(), last_col(1), starts_with('a'), ends_with('b'), contains('a'), matches('a|b'), num_range('x',1:2),
  all_of(c('b','a')), any_of(c('missing','b')), all_of(character()), any_of(character()),
  c(), NULL, integer(), character(), 0L, c(1L,3L), -c(1L,3L),
  selected, all_of(selected), c(selected), helper(), all_of(helper()),
  where(is.numeric), where(is.character), where(is.factor), where(is.list), where(is.atomic),
  where(function(x)length(x)>0), where(~is.numeric(.x)), predicate,
  .data$a, .data[['b']],
  all_of(names(tidyselect::peek_data())), all_of(tidyselect::peek_vars()),
  all_of(setdiff(tidyselect::peek_vars(),'e'))
)
# Keep intentionally redundant parentheses and empty arguments as syntax data.
selections <- c(selections, as.list(parse(text = c(
  '(a)', '((c(b,a)))', '(where(is.numeric))', 'c(,a)', 'c(a,)'
))))
construct <- list(grouping_set=grouping_set,grouping_sets=grouping_sets,grouping_spec=grouping_spec,rollup=rollup,cube=cube)
checked <- 0L
for(e in selections) {
 q <- rlang::new_quosure(e,globalenv())
 want <- suppressWarnings(tryCatch(names(dplyr::select(x,!!q)),error=identity))
 for(nm in names(construct)) {
  fn <- construct[[nm]]
  s <- rlang::inject(fn(!!q))
  result <- suppressWarnings(tryCatch(inspect_grouping(x,.grouping=s,.duplicates='keep',.format='list'),error=identity))
  if(inherits(want,'error')) {
    stopifnot(inherits(result,'error'), identical(class(result),class(want)))
  } else if(nm %in% c('rollup','cube')&&!length(want)) {
    stopifnot(inherits(result,'marginplyr_error'))
  } else {
    if(inherits(result,'error')){print(e);stop(conditionMessage(result))}
    stopifnot(setequal(result$included[[1]],want))
    dims <- unique(unlist(result$included,use.names=FALSE))
    stopifnot(setequal(dims,want))
    if(nm%in%c('grouping_set','grouping_sets','grouping_spec'))stopifnot(nrow(result)==1L)
    if(nm=='rollup')stopifnot(nrow(result)==length(want)+1L)
    if(nm=='cube')stopifnot(nrow(result)==2^length(want))
  }
  checked <- checked+1L
 }
}
cat('PASS: ',checked,' constructor/selector combinations against direct tidyselect selection.\n',sep='')
