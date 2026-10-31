# Independent reproduction of a derived SQLite Boolean-word payload failure.
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all('.', quiet = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
source <- copy_to(con, tibble(g = 'x', `TRUE` = 19L), 'independent_boolean')
for (id in list(NULL, 'sid')) {
  actual <- expand_with_margins(source, .grouping = rollup(g), .id = id)
  control <- if (is.null(id)) union_all(source, mutate(source, g = 'Total')) else
    union_all(mutate(source, sid = 1L), mutate(source, g = 'Total', sid = 2L))
  direct <- collect(actual)
  stopifnot(nrow(direct) == 2L, identical(direct[['TRUE']], c(19L, 19L)))
  cat('\nIdentifier:', if (is.null(id)) 'absent' else id, '\nDirect result:\n')
  print(direct)
  for (operation in c('head', 'filter')) {
    transform <- if (operation == 'head') function(x) head(x, 1L) else function(x) filter(x, TRUE)
    expected <- collect(transform(control))
    observed <- tryCatch(collect(transform(actual)), error = identity)
    stopifnot(!inherits(expected, 'error'), identical(expected[[2L]],
      rep(19L, if (operation == 'head') 1L else 2L)))
    cat('Ordinary union columns:', paste(names(expected), collapse = ', '), '\n')
    cat(operation, 'ordinary control rows:', nrow(expected), '\n')
    if (inherits(observed, 'error')) cat('Package-derived error:', conditionMessage(observed), '\n') else print(observed)
    if ('--assert' %in% commandArgs(trailingOnly = TRUE)) {
      stopifnot(!inherits(observed, 'error'), identical(observed[['TRUE']], expected[[2L]]),
        identical(names(observed), names(direct)))
    }
  }
}
DBI::dbDisconnect(con)
