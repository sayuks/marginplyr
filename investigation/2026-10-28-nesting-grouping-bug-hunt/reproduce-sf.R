# Run from the repository root; exploratory evidence for 2026-10-28.
# Reproduction only; no package implementation is changed.
wt <- '.'
pkgload::load_all(wt, quiet = TRUE)
assert <- '--assert' %in% commandArgs(trailingOnly = TRUE)
input <- sf::st_as_sf(data.frame(g = c('a', 'b'), x = 1:2, y = 3:4),
                      coords = c('x', 'y'))
cat('sf', as.character(packageVersion('sf')), '\n')
failures <- list()
for (verb_name in c('nest_with_margins', 'nest_by_with_margins')) {
  for (plan in c('none', 'fixed', 'rollup')) for (keep in c(FALSE, TRUE)) {
    for (size in c(0L, 2L)) for (sort in c('none', 'last', 'first')) {
      source <- input[seq_len(size), ]
      verb <- get(verb_name)
      result <- tryCatch(rlang::inject(verb(
        source, .by = !!(if (plan == 'fixed') rlang::sym('g') else NULL),
        .grouping = !!(if (plan == 'rollup') rollup(g) else NULL),
        .keep = keep, .sort = sort
      )), error = identity)
      name <- paste(verb_name, plan, keep, size, sort)
      if (!inherits(result, 'error')) {
        expected <- if (size == 0L) {
          if (plan == 'none' && verb_name == 'nest_by_with_margins') 0L else integer()
        } else if (plan == 'none') 2L else if (plan == 'fixed') c(1L, 1L) else c(1L, 1L, 2L)
        cells <- result[['data']]
        stopifnot(identical(sort(vapply(cells, nrow, integer(1))), expected))
        for (cell in cells) stopifnot('geometry' %in% names(cell))
      }
      if (inherits(result, 'error')) {
        failures[[name]] <- list(class = class(result), message = conditionMessage(result),
                                 call = conditionCall(result))
      }
    }
  }
}
cat('ERROR:', length(failures), 'TOTAL: 72\n')
print(table(vapply(failures, function(z) z$message, character(1))))
# Positive controls: supported upstream nesting of the very same sf input.
upstream <- tidyr::nest(input, .by = g)
upstream_by <- dplyr::nest_by(input, g)
stopifnot(identical(names(upstream), c('g', 'data')),
          identical(vapply(upstream$data, nrow, integer(1)), c(1L, 1L)),
          identical(names(upstream_by), c('g', 'data')),
          identical(vapply(upstream_by$data, nrow, integer(1)), c(1L, 1L)))
cat('tidyr::nest and dplyr::nest_by positive controls passed.\n')
# This source-level observation locates a cause, not a public attribute promise:
# select.sf updates its active geometry name when numeric aliases are applied,
# and rlang::set_names restores columns without updating that dependent name.
aliases <- stats::setNames(names(input), seq_along(input))
reordered <- dplyr::select(input, dplyr::all_of(aliases))
reordered <- rlang::set_names(reordered, names(input))
cat('reorder_margin_columns output names:', paste(names(reordered), collapse = ','), '\n')
cat('active geometry attribute:', attr(reordered, 'sf_column'), '\n')
geometry <- tryCatch(sf::st_geometry(reordered), error = identity)
if (inherits(geometry, 'error')) {
  cat('st_geometry error:', conditionMessage(geometry), '\n')
} else {
  cat('geometry metadata valid after reordering.\n')
}
saveRDS(failures, '/tmp/marginplyr-4306-local-sf-nesting-failures.rds')
if (assert && length(failures) > 0L) {
  stop('BUG REPRODUCED: admitted local sf nesting escapes with an internal invariant error.')
}
