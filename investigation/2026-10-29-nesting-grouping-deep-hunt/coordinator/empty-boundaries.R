# Dated probe; requires the investigated checkout and its stated R dependencies.
pkgload::load_all('.', quiet = TRUE)
scalar_shapes <- list('data', c(alias = 'data'), I('data'),
                      matrix('data', 1L, 1L), array('data', c(1L, 1L, 1L)))
shape_checks <- 0L
for (size in c(0L, 2L)) for (backend in c('local', 'dtplyr')) {
  input <- data.frame(g = rep('a', size), y = seq_len(size))
  source <- if (backend == 'local') input else dtplyr::lazy_dt(input)
  for (key in scalar_shapes) for (verb in list(nest_with_margins,
                                              nest_by_with_margins)) {
    actual <- dplyr::collect(verb(source, .key = key, .id = 'set'))
    expected <- dplyr::collect(verb(source, .key = 'data', .id = 'set'))
    stopifnot(identical(actual, expected))
    shape_checks <- shape_checks + 1L
  }
}
names_to_try <- c('.data', '.env', '.rows', '.SD', '.BY', '.N', '.I', '...',
                  '..1', '-..1', '..marginplyr_nest_1', '日本語', 'line\nbreak')
source_names <- c('.SD', '.BY', '.rows', '.env', '.data', '-..1', '-..2')
empty_checks <- 0L
for (size in c(0L, 2L)) for (key in names_to_try) {
  for (id in c('set', '...', '..1', '-..1', '日本語')) {
    if (key == id || id %in% source_names) next
    input <- tibble::as_tibble(setNames(rep(list(seq_len(size)),
                                           length(source_names)), source_names))
    for (backend in c('local', 'dtplyr')) {
      source <- if (backend == 'local') input else dtplyr::lazy_dt(input)
      actual <- nest_by_with_margins(source, .key = key, .id = id)
      stopifnot(nrow(actual) == 1L,
                identical(names(actual), c(id, key)),
                identical(dplyr::group_vars(actual), id),
                identical(actual[[id]], 1L),
                identical(tibble::as_tibble(actual[[key]][[1L]]), input))
      empty_checks <- empty_checks + 1L
    }
  }
}
cat('scalar-shape comparisons:', shape_checks, '\n')
cat('empty/nonempty combined-name checks:', empty_checks, '\n')
cat('All comparisons passed.\n')
