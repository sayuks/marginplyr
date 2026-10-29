# Public dependency-guard reproductions, investigated 2026-10-29.
# Report mode prints the defects; --assert requires the documented refusals.
args <- commandArgs(TRUE)
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
d <- tibble(g = c('a', 'b'), x = c(1, 3), prior = c(100, 300))
removals <- list(
  identity = quote(identity(rm(prior))),
  conditional_true = rlang::call2('if', TRUE, quote(rm(prior))),
  conditional_value = quote(if (sum(x) > 0) remove(prior)),
  eval_quote = quote(eval(quote(rm(prior)))),
  evalq = quote(evalq(remove(prior))),
  brace_argument = quote(identity({ rm(prior); NULL })),
  invisible = quote(invisible(base::rm(list = 'prior'))),
  loop_body = quote(for (i in 1) rm(prior))
)
failures <- list()
checks <- 0L
for (kind in c('parent', 'total')) {
  for (name in names(removals)) {
    expression <- rlang::expr({ prior <- 0; !!removals[[name]]; sum(prior) })
    for (direction in c('source', 'derived')) {
      helper <- rlang::call2(paste0('share_of_', kind), quote(value))
      actual <- tryCatch(if (direction == 'source') {
        summarize_with_margins(d, prior = sum(x), value = !!expression,
          fraction = !!helper, .grouping = rollup(g))
      } else {
        summarize_with_margins(d, value = sum(x), prior = !!helper,
          derived = !!expression, .grouping = rollup(g))
      }, error = identity)
      correct <- inherits(actual, 'marginplyr_error')
      checks <- checks + 1L
      cat(kind, name, direction, if (correct) 'REFUSED' else 'BYPASS', '\n')
      if (!correct) {
        failures[[paste(kind, name, direction)]] <- actual
        print(actual)
      }
    }
  }
}
# Removing a for-index itself also invalidates the scope after that loop.
for (kind in c('parent', 'total')) {
  helper <- rlang::call2(paste0('share_of_', kind), quote(value))
  actual <- tryCatch(summarize_with_margins(d, prior = sum(x),
    value = { for (prior in 1) rm(prior); sum(prior) },
    fraction = !!helper, .grouping = rollup(g)), error = identity)
  checks <- checks + 1L
  cat(kind, 'removed_loop_index', if (inherits(actual, 'marginplyr_error')) 'REFUSED' else 'BYPASS', '\n')
  if (!inherits(actual, 'marginplyr_error')) failures[[paste(kind, 'removed_loop_index')]] <- actual
}
# Removing a formal also reveals the enclosing data mask.
for (kind in c('parent', 'total')) {
  helper <- rlang::call2(paste0('share_of_', kind), quote(value))
  for (direction in c('source', 'derived')) {
    expression <- quote((function(prior = 0) {
      identity(rm(prior)); sum(prior)
    })())
    actual <- tryCatch(if (direction == 'source') {
      summarize_with_margins(d, prior = sum(x), value = !!expression,
        fraction = !!helper, .grouping = rollup(g))
    } else {
      summarize_with_margins(d, value = sum(x), prior = !!helper,
        derived = !!expression, .grouping = rollup(g))
    }, error = identity)
    checks <- checks + 1L
    cat(kind, 'removed_formal', direction,
        if (inherits(actual, 'marginplyr_error')) 'REFUSED' else 'BYPASS', '\n')
    if (!inherits(actual, 'marginplyr_error')) {
      failures[[paste(kind, 'removed_formal', direction)]] <- actual
    }
  }
}
controls <- 0L
for (kind in c('parent', 'total')) {
  helper <- rlang::call2(paste0('share_of_', kind), quote(value))
  # Straight-line removal is already recognized, in both dependency directions.
  for (direction in c('source', 'derived')) {
    answer <- tryCatch(if (direction == 'source') {
      summarize_with_margins(d, prior = sum(x),
        value = { prior <- 0; rm(prior); sum(prior) }, fraction = !!helper,
        .grouping = rollup(g))
    } else {
      summarize_with_margins(d, value = sum(x), prior = !!helper,
        derived = { prior <- 0; rm(prior); sum(prior) }, .grouping = rollup(g))
    }, error = identity)
    stopifnot(inherits(answer, 'marginplyr_error'))
    controls <- controls + 1L
  }
  # A retained lexical binding never reads the earlier alias.
  for (expression in list(quote({ prior <- 2; identity(NULL); sum(prior) }),
                          quote({ prior <- 2; if (FALSE) rm(prior); sum(prior) }))) {
    answer <- summarize_with_margins(d, prior = sum(x), value = !!expression,
      fraction = !!helper, .grouping = rollup(g))
    stopifnot(identical(answer$value, c(2, 2, 2)), identical(answer$fraction, c(1, 1, 1)))
    controls <- controls + 1L
  }
}
# Without a contextual share ordinary sequential-alias evaluation is allowed.
answer <- summarize_with_margins(d, prior = sum(x),
  value = { prior <- 0; identity(rm(prior)); sum(prior) }, .grouping = rollup(g))
stopifnot(identical(answer$value, c(1, 3, 4)))
controls <- controls + 1L
cat('Dependency checks:', checks, 'documented refusals missing:', length(failures),
    'controls passed:', controls, '\n')
if ('--assert' %in% args && length(failures)) {
  stop(length(failures), ' contextual-share dependency refusals were bypassed', call. = FALSE)
}
