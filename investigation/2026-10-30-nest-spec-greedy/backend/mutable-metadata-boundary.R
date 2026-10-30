# Dated inspection isolation and Mutable-step admission probes.
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
observe <- function(expr) tryCatch(force(expr), error = identity)
counts <- c(graphs = 0L, inspected = 0L, inspection_matches = 0L,
            inspection_refusals = 0L, nest_refusals = 0L,
            row_predicate_calls = 0L, unsafe_calls = 0L, source_mutations = 0L)
row_events <- integer()
row_predicate <- function(x) {row_events <<- c(row_events, length(x)); x > 0L}
unsafe_events <- integer()
unsafe <- function(x) {unsafe_events <<- c(unsafe_events, length(x)); x}
for (size in c(0L, 5L)) for (side in c('left', 'right', 'both')) {
  left <- data.table::data.table(g = rep(c('a', 'b', 'a', NA_character_, 'b'), length.out = size), v = seq_len(size))
  right <- data.table::copy(left)
  snapshots <- lapply(list(left, right), serialize, connection = NULL)
  lroot <- dtplyr::lazy_dt(left, immutable = !(side %in% c('left', 'both')))
  rroot <- dtplyr::lazy_dt(right, immutable = !(side %in% c('right', 'both')))
  bases <- list(
    inner = inner_join(lroot, rroot, by = c('g', 'v')),
    semi = semi_join(lroot, rroot, by = c('g', 'v')),
    union = union_all(lroot, rroot),
    difference = setdiff(lroot, rroot),
    filtered_union = union_all(filter(lroot, row_predicate(v)), filter(rroot, row_predicate(v))),
    cached_join = inner_join(compute(lroot), compute(rroot), by = c('g', 'v')),
    cache_chain = arrange(compute(rename(compute(inner_join(lroot, rroot, by = c('g', 'v'))), amount = v)), amount),
    unsafe = union_all(mutate(lroot, z = unsafe(v)), mutate(rroot, z = unsafe(v)))
  )
  for (name in names(bases)) for (cache in c(FALSE, TRUE)) {
    q <- bases[[name]]
    if (cache) q <- compute(q)
    counts[['graphs']] <- counts[['graphs']] + 1L
    row_events <- integer()
    unsafe_events <- integer()
    for (kind in c('names', 'typed')) {
      actual <- observe(if (kind == 'names') inspect_grouping(q, .grouping = rollup(g), .format = 'list') else inspect_grouping(q, .by = where(is.character), .format = 'list'))
      counts[['inspected']] <- counts[['inspected']] + 1L
      if (inherits(actual, 'marginplyr_error')) {
        counts[['inspection_refusals']] <- counts[['inspection_refusals']] + 1L
      } else {
        stopifnot(!inherits(actual, 'error'))
        expected <- if (kind == 'names') list(c('g'), character()) else list(character())
        stopifnot(identical(actual$included, expected))
        if (kind == 'typed') stopifnot(identical(actual$fixed, list('g')))
        counts[['inspection_matches']] <- counts[['inspection_matches']] + 1L
      }
    }
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      error <- observe(verb(q, .grouping = rollup(g)))
      stopifnot(inherits(error, 'marginplyr_error'), grepl('immutable = FALSE', conditionMessage(error), fixed = TRUE))
      counts[['nest_refusals']] <- counts[['nest_refusals']] + 1L
    }
    counts[['row_predicate_calls']] <- counts[['row_predicate_calls']] + length(row_events)
    counts[['unsafe_calls']] <- counts[['unsafe_calls']] + length(unsafe_events)
    if (!identical(snapshots, lapply(list(left, right), serialize, connection = NULL))) counts[['source_mutations']] <- counts[['source_mutations']] + 1L
  }
}
print(counts)
stopifnot(counts[['row_predicate_calls']] == 0L, counts[['unsafe_calls']] == 0L, counts[['source_mutations']] == 0L)
