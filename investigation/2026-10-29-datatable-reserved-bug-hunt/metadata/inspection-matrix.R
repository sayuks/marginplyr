# Dated read-only comparison of typed planning and explicitly collected inputs.
pkgload::load_all('.', quiet = TRUE)
out_dir <- 'investigation/2026-10-29-datatable-reserved-bug-hunt/metadata'
candidates <- unique(c(
  '.N', '.I', '.SD', '.GRP', '.NGRP', '.BY', '.EACHI', '.NATURAL', '.ROW',
  '.SDall', '.xSD', '.iSD', '.Last.updated', '.SDcols', '.internal.selfref',
  '.iGRP', '.xGRP', '..FUN1', '..FUN2', '..value', '..1', '...',
  'i.value', 'x.value', 'key', 'keep.rownames', 'check.names', 'stringsAsFactors',
  '.named', 'x', 'i', 'j', 'by', 'keyby', 'with', 'nomatch', 'mult', 'roll',
  'rollends', 'which', 'verbose', 'allow.cartesian', 'drop', 'on', 'env',
  'showProgress', 'T', 'F', 'get', 'mget', 'eval', 'list', '.',
  '_nqgrp_', '.ll.tech.split', '.nm.tech.split', '.ll.tech.split.names',
  '.data.table.locked', 'sorted', 'unique', 'print', '-.POSIXt', 'Cfastmean',
  'order', '..0', '..01', '..marginplyr_dtplyr_column_1'
))
attempt <- function(expr) {
  warnings <- character()
  ans <- tryCatch(withCallingHandlers(list(value = force(expr)), warning=function(cnd) {
    warnings <<- c(warnings, conditionMessage(cnd)); invokeRestart('muffleWarning')
  }), error=function(cnd) list(error=conditionMessage(cnd), class=class(cnd)[[1L]]))
  ans$warnings <- warnings
  ans
}
err <- function(ans) if(is.null(ans$error)) '' else ans$error
make_step <- function(root, derive, column) {
  sym <- rlang::sym(column)
  switch(derive,
    root = root,
    rename_other = dplyr::rename(root, amount = value),
    rename_special = dplyr::rename(root, ordinary = !!sym),
    select_special = dplyr::select(root, dplyr::all_of(c(column, 'value'))),
    select_safe = dplyr::select(root, value, flag),
    filter_true = dplyr::filter(root, TRUE),
    filter_rows = dplyr::filter(root, value > 1L),
    filter_empty = dplyr::filter(root, FALSE),
    arrange_safe = dplyr::arrange(root, value),
    arrange_special = dplyr::arrange(root, !!sym),
    group_safe = dplyr::group_by(root, value),
    group_special = dplyr::group_by(root, !!sym),
    mutate_safe = dplyr::mutate(root, doubled = value * 2L),
    mutate_copy = dplyr::mutate(root, copied = !!sym),
    mutate_special = dplyr::mutate(root, !!column := value * 2L),
    cache_rename = dplyr::rename(dplyr::compute(root), amount = value),
    cache_arrange = dplyr::arrange(dplyr::compute(root), value),
    shared_union = dplyr::union_all(dplyr::compute(root), dplyr::compute(root))
  )
}
derivations <- c('root','rename_other','rename_special','select_special','select_safe',
 'filter_true','filter_rows','filter_empty','arrange_safe','arrange_special',
 'group_safe','group_special','mutate_safe','mutate_copy','mutate_special',
 'cache_rename','cache_arrange','shared_union')
predicates <- list(character = is.character, integer = is.integer, factor = is.factor)
rows <- list()
for (column in candidates) {
 for (type in c('character','integer','factor')) {
  values <- switch(type, character=c('b','a','b'),integer=c(2L,1L,2L),
    factor=factor(c('b','a','b'), levels=c('a','b','unused')))
  for (derive in derivations) {
   source <- data.table::data.table(special=values, value=c(1L,2L,3L), flag=c(TRUE,FALSE,TRUE))
   data.table::setnames(source, 'special', column)
   before <- data.table::copy(source)
   root <- dtplyr::lazy_dt(source)
   construction <- attempt(make_step(root,derive,column))
   if (!is.null(construction$error)) {
    rows[[length(rows)+1L]] <- data.frame(column,type,derive,predicate='',role='',
     status='upstream-construction-error',error=err(construction),warning='',source_changed=!identical(source,before))
    next
   }
   step <- construction$value
   # Copy the whole graph's roots separately for the external materialization oracle.
   collected <- attempt(dplyr::collect(step))
   # Construction/execution may legally mutate caches; use a fresh graph for inspection.
   source <- data.table::copy(before)
   root <- dtplyr::lazy_dt(source)
   step <- attempt(make_step(root,derive,column))$value
   for (predicate in names(predicates)) {
    pred <- predicates[[predicate]]
    for (role in c('dimension','fixed')) {
     inspect <- function(data) {
      if(role=='dimension') inspect_grouping(data,.grouping=grouping_set(where(pred)))
      else inspect_grouping(data,.by=where(pred))
     }
     actual <- attempt(inspect(step))
     expected <- if(is.null(collected$error)) attempt(inspect(collected$value)) else collected
     status <- if(!is.null(collected$error)) 'upstream-collection-error'
      else if(!is.null(expected$error)) 'local-control-error'
      else if(!is.null(actual$error)) 'inspection-error'
      else if(!identical(actual$value,expected$value)) 'plan-mismatch'
      else if(length(actual$warnings)) 'inspection-warning' else 'pass'
     rows[[length(rows)+1L]] <- data.frame(column,type,derive,predicate,role,status,
       error=err(actual),warning=paste(actual$warnings,collapse=' | '),source_changed=!identical(source,before))
    }
   }
  }
 }
 cat('Completed', column,'\n'); flush.console()
}
results <- do.call(rbind,rows)
utils::write.csv(results,file.path(out_dir,'inspection-results.csv'),row.names=FALSE)
print(table(results$status)); print(subset(results, source_changed))
print(unique(results[results$status %in% c('inspection-error','plan-mismatch','inspection-warning'),c('column','type','derive','status','error')]))
cat('R:',R.version.string,'\ndata.table:',as.character(packageVersion('data.table')),
 '\ndtplyr:',as.character(packageVersion('dtplyr')),'\n')
