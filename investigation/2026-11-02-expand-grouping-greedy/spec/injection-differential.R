# Dated public-capture differential; run from the repository root.
Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all(quiet = TRUE)
options(lifecycle_verbosity = 'quiet')
set.seed(2026110201)
frames <- list(
  tibble::tibble(a = c(1L, 2L), b = c('x', 'y'), c = c(TRUE, FALSE), d = factor(c('m', 'n'))),
  tibble::tibble(a = integer(), b = character(), c = logical(), d = factor())
)
selection_oracle <- function(.data, selection) {
  tidyselect::eval_select(rlang::enquo(selection), .data, allow_rename = FALSE)
}
capture <- function(expr) suppressWarnings(tryCatch(expr, error = identity))
env <- rlang::env(globalenv(), external = c('b', 'd'))
inner <- rlang::env(globalenv(), external = c('a', 'c'))
q_a <- rlang::new_quosure(quote(a), inner)
q_external <- rlang::new_quosure(quote(all_of(external)), inner)
q_where <- rlang::new_quosure(quote(where(is.numeric)), inner)
q_missing <- rlang::new_quosure(quote(all_of('absent')), inner)
q_empty <- rlang::new_quosure(quote(NULL), inner)
env$q_a <- q_a
env$q_external <- q_external
env$q_where <- q_where
env$q_missing <- q_missing
env$q_empty <- q_empty
env$symbol_a <- rlang::sym('a')
env$literal <- c('b', 'a')
env$spliced <- list(q_a, q_external)
leaves <- list(
  quote(!!q_a), quote(!!q_external), quote(!!q_where), quote(!!q_missing),
  quote(!!q_empty), quote(!!symbol_a), quote(!!literal),
  quote(c(!!!spliced)), quote(c(!!q_a, !!q_external)),
  quote(c(!!q_external, everything())), quote(all_of(!!literal)),
  quote(c(!!q_where, d)), quote((!!q_a)), rlang::call2("(", rlang::call2("(", quote(!!q_external)))
)
expressions <- leaves
for (i in seq_len(360L)) {
  op <- sample(c('c', '|', '&', '/', ':', '-', '!'), 1L)
  left <- leaves[[sample.int(length(leaves), 1L)]]
  right <- list(quote(a), quote(c), quote(where(is.character)), quote(everything()), quote(NULL))[[sample.int(5L, 1L)]]
  expressions[[length(expressions) + 1L]] <- if (op %in% c('-', '!')) rlang::call2(op, rlang::call2('(', left)) else rlang::call2(op, left, right)
}
rows <- list()
for (f in seq_along(frames)) {
  env$data <- frames[[f]]
  for (i in seq_along(expressions)) {
    expression <- expressions[[i]]
    oracle <- capture(eval(rlang::call2('selection_oracle', quote(data), expression), env))
    for (parent in c('.by', 'grouping_set', 'grouping_sets', 'rollup', 'cube', 'grouping_spec')) {
      call <- if (parent == '.by') {
        rlang::call2('inspect_grouping', quote(data), .by = expression, .format = 'list')
      } else {
        rlang::call2('inspect_grouping', quote(data), .grouping = rlang::call2(parent, expression), .format = 'list', .duplicates = 'keep')
      }
      actual <- capture(eval(call, env))
      status <- 'match'
      if (inherits(oracle, 'error')) {
        if (!inherits(actual, 'error') || !identical(class(oracle)[[1L]], class(actual)[[1L]])) status <- 'candidate-error'
      } else if (!length(oracle) && parent %in% c('rollup', 'cube')) {
        status <- if (inherits(actual, 'marginplyr_error') && grepl('at least one dimension', conditionMessage(actual))) 'expected-empty-refusal' else 'candidate-empty'
      } else if (inherits(actual, 'error')) status <- 'candidate-refusal' else {
        cols <- if (parent == '.by') actual$fixed[[1L]] else actual$included[[1L]]
        if (!identical(cols, names(oracle))) status <- 'candidate-values'
      }
      rows[[length(rows) + 1L]] <- data.frame(frame = f, expression = i, parent = parent, status = status,
        source = paste(rlang::expr_deparse(expression), collapse = '\\n'),
        oracle = if (inherits(oracle, 'error')) conditionMessage(oracle) else paste(names(oracle), collapse = ','),
        actual = if (inherits(actual, 'error')) conditionMessage(actual) else paste(if (parent == '.by') actual$fixed[[1L]] else actual$included[[1L]], collapse = ','))
    }
  }
}
results <- do.call(rbind, rows)
write.csv(results, 'investigation/2026-11-02-expand-grouping-greedy/spec/injection-results.csv', row.names = FALSE)
print(table(results$status))
candidates <- results[grepl('candidate', results$status), ]
print(utils::head(candidates, 30L), row.names = FALSE)
cat('Executed comparisons:', nrow(results), '\n')
stopifnot(nrow(candidates) == 0L)
