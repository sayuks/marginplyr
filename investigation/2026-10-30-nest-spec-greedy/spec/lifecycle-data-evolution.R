pkgload::load_all('.', quiet = TRUE)
set.seed(301030411)
constructors <- c('grouping_set', 'grouping_sets', 'rollup', 'cube', 'grouping_spec')
selectors <- list(
  quote(tidyselect::where(is.character)),
  quote(tidyselect::where(is.numeric)),
  quote(tidyselect::where(is.logical)),
  quote(tidyselect::all_of(chosen)),
  quote(tidyselect::any_of(c(chosen, 'absent'))),
  quote(tidyselect::all_of(pick())),
  quote(tidyselect::all_of(names(tidyselect::peek_data())[vapply(tidyselect::peek_data(), is.numeric, logical(1))])),
  quote(tidyselect::everything()),
  quote(tidyselect::last_col()),
  quote(c(1L, 3L)), quote(-2L), quote(NULL),
  quote(tidyselect::matches('^x|,$')),
  quote(tidyselect::all_of(rev(names(tidyselect::peek_data()))))
)
expand <- function(kind, cols) {
  if (kind %in% c('grouping_set', 'grouping_spec')) return(list(cols))
  if (kind == 'grouping_sets') return(list(cols, character()))
  if (!length(cols)) stop('empty units')
  if (kind == 'rollup') return(lapply(rev(seq.int(0L, length(cols))), function(n) cols[seq_len(n)]))
  ids <- unlist(lapply(rev(seq.int(0L, length(cols))), function(n) combn(seq_along(cols),n,simplify=FALSE)),recursive=FALSE)
  lapply(ids,function(i) cols[i])
}
checks <- rejected <- serialized <- 0L
for (si in seq_along(selectors)) for (kind in constructors) {
  env <- rlang::env(.GlobalEnv, chosen='x1', pick=function() character())
  env$pick <- eval(quote(function() names(tidyselect::peek_data())[vapply(tidyselect::peek_data(), is.character, logical(1))]), env)
  expr <- selectors[[si]]
  node <- rlang::call2(kind, expr)
  if (kind == 'grouping_sets') node <- rlang::call2(kind, expr, quote(grouping_set()))
  spec <- eval(node,env)
  spec_copy <- unserialize(serialize(spec,NULL))
  for (iteration in seq_len(12L)) {
    nms <- sample(c('x1','x2','a,','日本語','`odd`','v','_id'), 5L)
    types <- sample(c('character','integer','double','logical'),5L,replace=TRUE)
    vals <- lapply(types,function(type) switch(type, character=c('a','b'),integer=c(1L,2L),double=c(1.5,2.5),logical=c(TRUE,FALSE)))
    d <- tibble::as_tibble(setNames(vals,nms),.name_repair='minimal')
    env$chosen <- sample(nms, sample(0:3,1L))
    # The serialized local environment is distinct; mutate its captured values
    # independently, then ask both objects to resolve the same selection.
    copy_env <- rlang::quo_get_env(spec_copy$args[[1L]])
    if (!identical(copy_env, emptyenv())) copy_env$chosen <- env$chosen
    cols <- names(tidyselect::eval_select(rlang::new_quosure(expr,env),d))
    expected <- tryCatch(expand(kind,cols),error=identity)
    if (!inherits(expected,'error')) {
      dims <- unique(unlist(expected,use.names=FALSE))
      if(is.null(dims)) dims <- character()
      expected <- lapply(expected,function(s) dims[dims %in% s])
      expected <- unique(expected)
    }
    for (s in list(spec,spec_copy)) for (backend in c('local','dtplyr')) {
      input <- if (backend=='local') d else dtplyr::lazy_dt(d)
      actual <- tryCatch(inspect_grouping(input,.grouping=s,.duplicates='drop',.format='list'),error=identity)
      if (inherits(expected,'error')) {
        stopifnot(inherits(actual,'marginplyr_error'))
        rejected <- rejected+1L
      } else {
        if(inherits(actual,'error') || !identical(actual$included,expected)) {
          print(list(selector=expr,kind=kind,iteration=iteration,backend=backend,data=d,chosen=env$chosen,expected=expected,actual=actual))
          quit(status=1L)
        }
        stopifnot(identical(actual$set_id,seq_along(expected)))
        masks <- lapply(expected,function(s) if(length(dims)) setNames(as.integer(!dims %in% s),dims) else integer())
        stopifnot(identical(actual$grouping_bits,masks))
        checks <- checks+1L
      }
      if(identical(s,spec_copy)) serialized <- serialized+1L
    }
  }
}
cat('seed=301030411 successful plan comparisons=',checks,' expected empty-unit refusals=',rejected,' total=',checks+rejected,'\n',sep='')
cat('Reused captures and independently serialized copies both resolved fresh changed column names, column types, selection bindings, masks and occurrence IDs.\n')
print(sessionInfo())
