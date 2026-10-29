# Evidence only: public API comparisons, with no package implementation changes.
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1L]] else 'investigation/2026-10-29-datatable-reserved-bug-hunt/summary'
names_to_check <- c('.N', '.SD', '.BY', '.I', '.GRP', '.NGRP', '.EACHI',
                    '.NATURAL', '.ROW', '.SDall', '.xSD', '.iSD', '.', '..x',
                    '..g', '..value', '..1', '...', 'key', 'keep.rownames',
                    'check.names', 'stringsAsFactors', 'i', 'j', 'by', 'with',
                    'on', ':=', 'CJ', 'data.table', '.SDcols', 'NROW')
canonical <- function(x) {
  x <- as.data.frame(x, optional = TRUE)
  if (nrow(x)) x <- x[do.call(order, c(unname(x), list(na.last = TRUE))), , drop = FALSE]
  rownames(x) <- NULL
  x
}
run_case <- function(name, role, share, style, backend) {
  data <- data.frame(g = c('a', 'a', 'b', 'b'), f = c('P', 'P', 'P', 'Q'),
                     x = c(2, 5, 11, 17), check.names = FALSE)
  if (role == 'measure') names(data)[3] <- name
  if (role == 'dimension') names(data)[1] <- name
  if (role == 'fixed') names(data)[2] <- name
  measure <- if (role == 'measure') name else 'x'
  dimension <- if (role == 'dimension') name else 'g'
  fixed <- if (role == 'fixed') name else 'f'
  source <- if (role == 'source_output') name else 'value'
  fraction <- if (role == 'share_output') name else 'fraction'
  dots <- if (style == 'direct') {
    rlang::set_names(list(rlang::call2('sum', rlang::sym(measure))), source)
  } else {
    list(rlang::expr(dplyr::across(dplyr::all_of(!!measure), sum, .names = !!source)))
  }
  if (share != 'none') {
    dots <- c(dots, if (style == 'direct') {
      rlang::set_names(list(rlang::call2(paste0('share_of_', share), rlang::sym(source))), fraction)
    } else {
      list(rlang::expr(dplyr::across(dplyr::all_of(!!source), !!rlang::sym(paste0('share_of_', share)), .names = !!fraction)))
    })
  }
  if (backend == 'dtplyr') data <- dtplyr::lazy_dt(data)
  result <- summarize_with_margins(data, !!!dots,
              .grouping = rollup(dplyr::all_of(dimension)),
              .by = dplyr::all_of(fixed),
              .id = if (role == 'id') name else 'set', .sort = 'last')
  if (backend == 'dtplyr') result <- collect(result)
  renamed <- c(g = dimension, f = fixed, x = measure, value = source, fraction = fraction,
               set = if (role == 'id') name else 'set')
  names(result) <- names(renamed)[match(names(result), unname(renamed))]
  canonical(result)
}
attempt <- function(...) tryCatch(list(ok = TRUE, value = run_case(...)),
                   error = function(e) list(ok = FALSE, message = conditionMessage(e), class = class(e)[1]))
rows <- list(); index <- 0L
for (name in names_to_check) for (role in c('measure','dimension','fixed','source_output','share_output','id')) {
  for (share in c('none','parent','total')) for (style in c('direct','across')) {
    if (role == 'share_output' && share == 'none') next
    # A safe-name control distinguishes literal-name constraints from this operation.
    safe <- attempt('safe_name', role, share, style, 'dtplyr')
    local <- attempt(name, role, share, style, 'local')
    actual <- attempt(name, role, share, style, 'dtplyr')
    same <- actual$ok && local$ok && isTRUE(all.equal(actual$value, local$value, check.attributes = TRUE))
    status <- if (!safe$ok) 'control_failure' else if (!local$ok) 'local_refusal' else if (!actual$ok) 'dt_error' else if (!same) 'value_mismatch' else 'pass'
    index <- index + 1L
    rows[[index]] <- data.frame(name, role, share, style, status,
      local = if (local$ok) '' else local$message,
      actual = if (actual$ok) if(same) '' else paste(capture.output(dput(actual$value)), collapse=' ') else actual$message,
      expected = if (local$ok && !same) paste(capture.output(dput(local$value)), collapse=' ') else '',
      stringsAsFactors = FALSE)
  }
  if (role == 'id') cat(name, '\n')
}
results <- do.call(rbind, rows)
write.csv(results, file.path(out, 'matrix.csv'), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(out,'session-info.txt'))
print(table(results$status))
print(results[results$status != 'pass', c('name','role','share','style','status')], row.names=FALSE)

stopifnot(nrow(results) == 1088L, sum(results$status == 'pass') == 973L, sum(results$status == 'dt_error') == 73L, sum(results$status == 'local_refusal') == 42L)
