args <- commandArgs(TRUE)
root_arg <- args[startsWith(args, '--root=')]
root <- if (length(root_arg)) sub('^--root=', '', root_arg[[1L]]) else '.'
pkgload::load_all(root, quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
failures <- list()
# A generated summary column is valid ordinary dplyr / Margin output.
for (nm in c('..-1', '..-0', '..+1', '..+0', '.. 1')) {
  for (backend in c('local', 'dtplyr')) {
    data <- tibble(g = c('a', 'b'), x = c(1, 3))
    input <- if (backend == 'local') data else dtplyr::lazy_dt(data)
    materialize <- if (backend == 'local') identity else as.data.frame
    ordinary <- setNames(rlang::exprs(sum(x)), nm)
    baseline <- materialize(summarise(input, !!!ordinary))
    stopifnot(identical(baseline[[nm]], 4))
    baseline <- materialize(summarize_with_margins(input, !!!ordinary, .grouping = rollup(g)))
    stopifnot(identical(baseline[[nm]], c(1, 3, 4)))
    for (kind in c('parent', 'total')) {
      request <- rlang::call2(paste0('share_of_', kind), rlang::sym(nm))
      result <- tryCatch(materialize(summarize_with_margins(input, !!!ordinary, share = !!request, .grouping = rollup(g))), error = identity)
      id <- paste('source', encodeString(nm, quote='"'), backend, kind)
      cat(id, ':', if(inherits(result, 'error')) conditionMessage(result) else paste(result$share, collapse=','), '\n')
      failures[[id]] <- result
    }
  }
}
# The same accepted name also fails before summarization as a local fixed key.
for (nm in c('..-1', '..-0', '..+1', '..+0', '.. 1')) {
  data <- tibble(fixed = c('f', 'f'), g = c('a', 'b'), x = c(1, 3))
  names(data)[[1L]] <- nm
  baseline <- summarise(data, value = sum(x), .by = all_of(nm))
  stopifnot(identical(baseline$value, 4))
  result <- tryCatch(summarize_with_margins(data, value = sum(x), .by = all_of(nm), .grouping = rollup(g)), error=identity)
  id <- paste('fixed', encodeString(nm, quote='"'), 'local')
  cat(id, ':', if(inherits(result, 'error')) conditionMessage(result) else paste(result$value, collapse=','), '\n')
  failures[[id]] <- result
}
if ('--assert' %in% args) {
  for (id in names(failures)) {
    result <- failures[[id]]
    if (inherits(result, 'error')) stop(id, ' rejected a valid ordinary name: ', conditionMessage(result), call.=FALSE)
    if (startsWith(id, 'source')) stopifnot(identical(result$share, c(0.25, 0.75, 1))) else stopifnot(identical(result$value, c(1, 3, 4)))
  }
}
cat('Completed', length(failures), 'source/fixed probes with ordinary controls\n')
