# Run from the repository root; exploratory evidence for 2026-10-28.
# Reproduction only. This file does not modify marginplyr.
# Default mode records evidence and exits successfully. --assert makes the
# public .key name-invariance assertion fail against the investigated commit.
wt <- '.'
pkgload::load_all(wt, quiet = TRUE)
assert <- '--assert' %in% commandArgs(trailingOnly = TRUE)
input <- tibble::tibble(g = c('a', 'b'), value = 1:2)
invoke <- function(verb, input, key, plan, id, keep, sort) {
  rlang::inject(verb(
    input,
    .by = !!(if (plan == 'fixed') rlang::sym('g') else NULL),
    .grouping = !!(if (plan == 'rollup') rollup(g) else NULL),
    .key = key, .id = id, .keep = keep, .sort = sort
  ))
}
normalize <- function(result, key) {
  result <- dplyr::ungroup(result)
  names(result)[names(result) == key] <- 'nested_cell'
  list(names = names(result), values = as.list(result),
       cell_rows = vapply(result$nested_cell, nrow, integer(1)))
}
failures <- list()
passes <- 0L
for (verb_name in c('nest_with_margins', 'nest_by_with_margins')) {
  verb <- get(verb_name)
  for (key in c('...', '..1')) for (plan in c('none', 'fixed', 'rollup')) {
    for (size in c(0L, 2L)) for (id in list(NULL, 'set')) {
      for (keep in c(FALSE, TRUE)) for (sort in c('none', 'last', 'first')) {
        source <- input[seq_len(size), ]
        reference <- invoke(verb, source, 'data', plan, id, keep, sort)
        actual <- tryCatch(invoke(verb, source, key, plan, id, keep, sort), error = identity)
        label <- paste(verb_name, key, plan, size, if (is.null(id)) 'no_id' else id, keep, sort)
        if (inherits(actual, 'error')) {
          failures[[label]] <- list(class = class(actual), message = conditionMessage(actual),
                                    call = conditionCall(actual))
        } else if (!identical(normalize(actual, key), normalize(reference, 'data'))) {
          failures[[label]] <- list(actual = normalize(actual, key),
                                    expected = normalize(reference, 'data'))
        } else {
          passes <- passes + 1L
        }
      }
    }
  }
}
cat('PASS:', passes, 'FAIL:', length(failures), 'TOTAL:', passes + length(failures), '\n')
cat('Failures by verb, plan, and condition class:\n')
print(table(vapply(names(failures), function(name) {
  parts <- strsplit(name, ' ', fixed = TRUE)[[1L]]
  paste(parts[[1L]], parts[[3L]], failures[[name]]$class[[1L]])
}, character(1))))
saveRDS(failures, '/tmp/marginplyr-4306-local-dot-dot-key-failures.rds')
cat('\nMinimal demonstrations:\n')
expressions <- list(
  quote(nest_with_margins(input, .by = g, .key = '..1')),
  quote(nest_with_margins(input, .by = g, .key = '..1', .id = 'set')),
  quote(nest_with_margins(input, .key = '..1', .sort = 'last')),
  quote(nest_with_margins(input, .grouping = rollup(g), .key = '..1', .id = 'set')),
  quote(nest_by_with_margins(input, .by = g, .key = '..1', .id = 'set'))
)
for (expression in expressions) {
  cat(paste(deparse(expression), collapse = '\n'), '\n')
  result <- tryCatch(eval(expression), error = identity)
  if (inherits(result, 'error')) {
    cat(class(result)[[1L]], conditionMessage(result), '\n')
    cat('conditionCall:', paste(deparse(conditionCall(result)), collapse = ' '), '\n')
  } else {
    cat('names:', paste(names(result), collapse = ','), '\n')
    cat('cell rows:', paste(vapply(result[['..1']], nrow, integer(1)), collapse = ','), '\n')
  }
}
if (assert && length(failures) > 0L) {
  stop('BUG REPRODUCED: valid dot-dot .key names do not preserve nesting behavior.')
}
