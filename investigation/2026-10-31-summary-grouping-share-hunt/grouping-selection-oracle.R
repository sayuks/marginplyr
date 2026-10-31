suppressPackageStartupMessages(pkgload::load_all('.', quiet = TRUE))
options(lifecycle_verbosity='quiet', width=120)
set.seed(20261031)
checks <- 0L
signals <- list()
observe <- function(x) tryCatch(force(x), error=identity)
source <- tibble::tibble(a=c('x','y'), b=c(1L,2L), c=c(1,2), d=factor(c('p','q')), e=as.Date('2026-10-31')+0:1, f=list(1,2))
layouts <- list(names(source), c('..-1','.env','a,b','a`b','na me','日本語'), c('FALSE','TRUE','NA','_','..0','.data'))
selection_exprs <- list(quote(dplyr::everything()), quote(where(is.numeric)), quote(where(is.factor)), quote(where(is.list)), quote(where(is.character)), quote(where(is.integer)), quote(where(~inherits(.x,'Date'))))
selections <- list()
for (layout in layouts) {
  data <- source; names(data) <- layout
  env <- rlang::env(globalenv(), chosen=layout[c(2L,4L)], locs=c(2L,4L), data=data)
  env$select_names <- function(x) x
  for (p in list(quote(chosen),quote(locs),quote(tidyselect::all_of(chosen)),quote(tidyselect::any_of(c(chosen,'absent'))),quote((select_names)(chosen)),quote((function(x) x)(chosen)),quote(tidyselect::all_of((function(x=chosen) x)())),quote(tidyselect::all_of(names(tidyselect::peek_data()))),quote(tidyselect::all_of(names(tidyselect::peek_data())[vapply(tidyselect::peek_data(), is.numeric, logical(1))])))) {
    selections[[length(selections)+1L]] <- list(data=data, env=env, expr=p)
  }
  for (p in selection_exprs) selections[[length(selections)+1L]] <- list(data=data,env=env,expr=p)
  for (i in seq_len(80L)) {
    leaves <- list(rlang::sym(sample(layout,1)),sample(seq_along(layout),1),rlang::call2('all_of',sample(layout,sample(0:4,1))),rlang::call2('any_of',sample(c(layout,'absent'),sample(0:4,1))))
    expr <- leaves[[sample(seq_along(leaves),1)]]
    for (j in seq_len(sample(0:3,1))) {
      op <- sample(c('c','&','|','!','(',':','/'),1)
      right <- leaves[[sample(seq_along(leaves),1)]]
      expr <- if (op %in% c('!','(')) rlang::call2(op,expr) else rlang::call2(op,expr,right)
    }
    selections[[length(selections)+1L]] <- list(data=data,env=env,expr=expr)
  }
}
for (case in selections) {
  direct <- observe(tidyselect::eval_select(rlang::new_quosure(case$expr,case$env),case$data))
  for (depth in c(0L,1L,3L)) {
    q <- rlang::new_quosure(case$expr,case$env)
    for (j in seq_len(depth)) q <- rlang::new_quosure(rlang::call2('(',q),rlang::env(globalenv(), chosen='bogus',locs=999))
    for (ctor in c('grouping_set','grouping_sets','rollup','cube','grouping_spec','by')) {
      actual <- if (ctor=='by') observe(rlang::inject(inspect_grouping(case$data,.by=!!q,.format='list'))) else {
        spec <- eval(rlang::call2(ctor,q),case$env)
        observe(inspect_grouping(case$data,.grouping=spec,.format='list'))
      }
      checks <- checks+1L
      if (inherits(direct,'error')) {
        ok <- inherits(actual,'error') && identical(class(actual),class(direct))
      } else if (ctor %in% c('rollup','cube') && length(direct)==0L) {
        ok <- inherits(actual,'marginplyr_error')
      } else if (!inherits(actual,'error')) {
        selected <- names(direct)
        expected <- if (ctor=='rollup') lapply(rev(0:length(selected)),function(n) selected[seq_len(n)]) else if (ctor=='cube') {
          unlist(lapply(rev(0:length(selected)),function(n) lapply(utils::combn(seq_along(selected),n,simplify=FALSE),function(z) selected[z])),recursive=FALSE)
        } else list(selected)
        actual_sets <- actual[[if (ctor=='by') 'fixed' else 'included']]
        ok <- identical(actual_sets,expected)
      } else ok <- FALSE
      if (!ok) signals[[length(signals)+1L]] <- list(expr=case$expr,ctor=ctor,depth=depth,direct=direct,actual=actual)
    }
  }
}
cat('SELECTION CHECKS',checks,'SIGNALS',length(signals),'\n')
if(length(signals)) { for(s in head(signals,10)) { print(s$expr);print(s$ctor);print(s$depth);print(s$direct);print(s$actual) }; quit(status=1) }
