# Observational reproduction; --assert requires faithful metadata or safe refusal.
Sys.setenv(TESTTHAT_PARALLEL = 'false')
suppressPackageStartupMessages(library(dplyr))
pkgload::load_all(Sys.getenv('MARGINPLYR_HUNT_SOURCE', unset = getwd()), quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
e <- new.env(parent = globalenv())
e$events <- integer()
e$merge <- function(x, y, ...) {
  events <<- c(events, nrow(x), nrow(y))
  result <- get('merge.data.table', envir = asNamespace('data.table'))(x, y, ...)
  if (nrow(result) == 0L) result[, g := integer()]
  result
}
environment(e$merge) <- e
e$source <- data.table::data.table(g = c('a', 'b'), v = 1:2)
snapshot <- serialize(e$source, NULL)
e$query <- evalq({
  x <- dtplyr::lazy_dt(source)
  full_join(x, x, by = 'g')
}, e)
cat('query\n'); print(show_query(e$query))
native <- collect(e$query)
stopifnot(identical(native$g, c('a', 'b')), identical(e$events, c(2L, 2L)))
cat('native control\n'); print(native)
e$events <- integer()
plain <- inspect_grouping(e$query, .grouping = grouping_set(g), .format = 'list')
stopifnot(identical(plain$included, list('g')), length(e$events) == 0L)
cat('name-only control: included g; merge calls 0\n')
result <- list()
for (verb in c('inspect_grouping', 'nest_with_margins', 'nest_by_with_margins')) {
  e$events <- integer()
  f <- get(verb)
  ans <- tryCatch(f(e$query, .grouping = grouping_set(where(is.character))), error = identity)
  events <- e$events
  if (inherits(ans, 'error')) {
    faithful <- inherits(ans, 'marginplyr_error') && grepl("Can't safely determine column metadata", conditionMessage(ans), fixed = TRUE) && length(events) == 0L
    cat(verb, 'error:', conditionMessage(ans), '\n')
  } else if (verb == 'inspect_grouping') {
    faithful <- identical(ans$included, '(g)')
    print(ans)
  } else {
    if (inherits(ans, 'dtplyr_step')) ans <- collect(ans)
    faithful <- 'g' %in% names(ans) && nrow(ans) == 2L
    print(ans)
  }
  cat(verb, 'faithful_or_safe_refusal:', faithful, '; merge row counts:', events, '\n')
  result[[verb]] <- faithful
}
stopifnot(identical(snapshot, serialize(e$source, NULL)))
cat('source unchanged: TRUE\n')
rm('merge', envir = e)
for (verb in names(result)) {
  ans <- get(verb)(e$query, .grouping = grouping_set(where(is.character)))
  if (verb == 'inspect_grouping') stopifnot(identical(ans$included, '(g)')) else {
    if (inherits(ans, 'dtplyr_step')) ans <- collect(ans)
    stopifnot('g' %in% names(ans), nrow(ans) == 2L)
  }
}
cat('unmasked controls: all 3 passed\n')
print(sessionInfo())
if ('--assert' %in% args) stopifnot(all(unlist(result)))
