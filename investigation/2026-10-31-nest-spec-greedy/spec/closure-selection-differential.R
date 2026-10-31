pkgload::load_all('.', quiet = TRUE)
options(lifecycle_verbosity = 'quiet')
set.seed(31103119)
constructors <- c('grouping_set','grouping_sets','rollup','cube','grouping_spec')
# Compare the admitted selection itself with ordinary typed tidyselect. The
# independent expansion below reads only tidyselect's column-name answer.
expand <- function(kind, cols) {
  if (kind %in% c('grouping_set','grouping_sets','grouping_spec')) return(list(cols))
  if (!length(cols)) return(NULL)
  if (kind == 'rollup') return(lapply(rev(seq.int(0L,length(cols))),function(n) cols[seq_len(n)]))
  subsets <- unlist(lapply(rev(seq.int(0L,length(cols))),function(n) combn(seq_along(cols),n,simplify=FALSE)),recursive=FALSE)
  lapply(subsets,function(i) cols[i])
}
bodies <- list(
  quote(names(tidyselect::peek_data())),
  quote(names(tidyselect::peek_data())[vapply(tidyselect::peek_data(),is.character,logical(1))]),
  quote(names(tidyselect::peek_data())[vapply(tidyselect::peek_data(),is.numeric,logical(1))]),
  quote(tidyselect::peek_vars()),
  quote(tidyselect::peek_vars()[c(3L,1L)]),
  quote(grep('a',names(tidyselect::peek_data()),value=TRUE)),
  quote(grep('a',tidyselect::peek_vars(),value=TRUE)),
  quote(names(tidyselect::peek_data())[lengths(tidyselect::peek_data())==0L]),
  quote(names(tidyselect::peek_data())[vapply(tidyselect::peek_data(),function(x) inherits(x,'factor'),logical(1))]),
  quote({x <- names(tidyselect::peek_data()); x[c(2L,1L)]}),
  quote({x <- names(tidyselect::peek_data()); x <- paste0(x,''); x}),
  quote({x <- tidyselect::peek_data(); names(x)[vapply(x,is.logical,logical(1))]}),
  quote(sort(tidyselect::peek_vars())),
  quote(unname(c(3L,1L))), quote(c('absent','a')), quote(c(renamed='a')),
  quote(character()), quote(1:2), quote(-2L), quote(c(TRUE,FALSE))
)
shapes <- list(
  function(body) rlang::new_function(pairlist(),body,.GlobalEnv),
  function(body) rlang::new_function(pairlist(x=rlang::missing_arg()),rlang::call2('identity',body),.GlobalEnv),
  function(body) rlang::new_function(as.pairlist(list(x=body)),quote(x),.GlobalEnv),
  function(body) rlang::new_function(as.pairlist(list(x=body,y=quote(x))),quote(y),.GlobalEnv)
)
counts <- c(match=0L,external=0L,rename=0L,empty=0L)
for (iteration in 1:8) {
  types <- sample(c('character','integer','logical','factor'),4L,replace=TRUE)
  d <- tibble::as_tibble(setNames(lapply(types,function(type) switch(type,character=c('x','y'),integer=1:2,logical=c(TRUE,FALSE),factor=factor(c('x','y')))),c('a','ab','c','日本語')))
  for (bi in seq_along(bodies)) for (fi in seq_along(shapes)) for (depth in 0:2) {
    fn <- shapes[[fi]](bodies[[bi]])
    env <- rlang::env(.GlobalEnv, choose=fn)
    head <- quote(choose)
    if(depth) {
      for (k in seq_len(depth)) head <- rlang::call2('(',head)
    }
    expr <- rlang::expr(tidyselect::all_of(!!rlang::call2(head)))
    q <- rlang::new_quosure(expr,env)
    direct <- tryCatch(tidyselect::eval_select(q,d),error=identity)
    for (constructor in constructors) {
      s <- eval(rlang::call2(constructor,expr),env)
      actual <- tryCatch(inspect_grouping(d,.grouping=s,.duplicates='drop',.format='list'),error=identity)
      context <- list(iteration=iteration,body=bi,shape=fi,depth=depth,kind=constructor)
      if (inherits(direct,'error')) {
        if(!inherits(actual,'error') || !identical(class(actual),class(direct))) {print(context);print(direct);print(actual);stop('external mismatch')}
        counts['external'] <- counts['external']+1L
      } else if(any(names(direct)!=names(d)[direct])) {
        if(!inherits(actual,'marginplyr_error')) {print(context);stop('missing rename refusal')}
        counts['rename'] <- counts['rename']+1L
      } else {
        expected <- expand(constructor,names(direct))
        if(is.null(expected)) {
          if(!inherits(actual,'marginplyr_error')) {print(context);stop('missing empty refusal')}
          counts['empty'] <- counts['empty']+1L
        } else {
          expected <- unique(expected)
          if(inherits(actual,'error') || !identical(actual$included,expected)) {print(context);print(expected);print(actual);stop('plan mismatch')}
          counts['match'] <- counts['match']+1L
        }
      }
    }
  }
}
print(counts)
cat('Total comparisons:',sum(counts),' seed:31103119\n')
print(sessionInfo())
