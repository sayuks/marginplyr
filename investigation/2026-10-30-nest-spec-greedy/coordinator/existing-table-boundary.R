# Recheck a concurrently published existing Issue against the nesting seam.
pkgload::load_all('.', quiet = TRUE)
x <- tibble::tibble(g = table(c('a', 'b', 'b')), row_id = 1:2)
stopifnot(nrow(dplyr::nest_by(x, g)) == 2L)
for (verb in c('nest_with_margins', 'nest_by_with_margins')) {
  single <- get(verb)(x, .grouping = grouping_set(g), .margin_label = NULL)
  stopifnot(nrow(single) == 2L)
  failure <- tryCatch(get(verb)(x, .grouping = rollup(g), .margin_label = NULL), error = identity)
  stopifnot(inherits(failure, 'vctrs_error_ptype2'))
  cat(verb, ': included-only and upstream nesting passed; rollup reproduced existing #1071:',
      conditionMessage(failure), '\n')
}
