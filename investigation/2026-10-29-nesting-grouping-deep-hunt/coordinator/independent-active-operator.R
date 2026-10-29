# Independent public-call witness for the active arithmetic operator finding.
pkgload::load_all('.', quiet = TRUE)
fixture <- function() {
  env <- new.env(parent = globalenv())
  env$reads <- 0L
  env$proxy_rows <- integer()
  makeActiveBinding('+', function() {
    env$reads <- base::`+`(env$reads, 1L)
    if (env$reads == 3L) {
      function(e1, e2) {
        env$proxy_rows <- c(env$proxy_rows, length(e1))
        if (!length(e1)) character() else base::`+`(e1, e2)
      }
    } else base::`+`
  }, env)
  env$query <- evalq(dplyr::mutate(
    dtplyr::lazy_dt(data.frame(v = 1:2)), z = v + 1L
  ), env)
  env
}
# Every populated evaluation agrees, including the third operator read.
control <- fixture()
for (i in 1:5) {
  collected <- dplyr::collect(control$query)
  stopifnot(identical(collected$z, 2:3))
}
cat('Five ordinary collections returned integer z = 2:3.\n')
for (name in c('inspect_grouping', 'nest_with_margins', 'nest_by_with_margins')) {
  env <- fixture()
  observed <- switch(name,
    inspect_grouping = inspect_grouping(env$query, .by = where(is.character), .format = 'list'),
    nest_with_margins = dplyr::collect(nest_with_margins(env$query, .by = where(is.character))),
    nest_by_with_margins = nest_by_with_margins(env$query, .by = where(is.character))
  )
  expected <- switch(name,
    inspect_grouping = inspect_grouping(collected, .by = where(is.character), .format = 'list'),
    nest_with_margins = nest_with_margins(collected, .by = where(is.character)),
    nest_by_with_margins = nest_by_with_margins(collected, .by = where(is.character))
  )
  if (name == 'inspect_grouping') {
    stopifnot(identical(observed$fixed, list('z')), identical(expected$fixed, list(character())))
  } else {
    stopifnot(nrow(observed) == 2L, nrow(expected) == 1L,
              identical(names(observed), c('z', 'data')),
              identical(names(expected), 'data'))
  }
  stopifnot(identical(env$proxy_rows, 0L))
  cat(name, ': wrong typed fixed keys reproduced; synthetic callback rows = 0\n')
}
