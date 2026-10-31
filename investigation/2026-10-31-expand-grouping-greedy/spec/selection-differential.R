# Dated investigation harness; run from the repository root.
pkgload::load_all(quiet = TRUE)
options(lifecycle_verbosity = 'quiet')
normalize_observations <- function(results) {
  keys <- interaction(results$frame, results$expression, results$parent, drop = TRUE)
  groups <- split(results, keys)
  normalized <- do.call(rbind, lapply(groups, function(group) {
    other <- group[setdiff(names(group), 'source')]
    stopifnot(nrow(unique(other)) == 1L)
    group$source <- paste(group$source, collapse = '\n')
    group[1L, ]
  }))
  # Public capture injects these ASTs; direct eval_select does not. Neither
  # answer is an oracle for the other's different evaluation phase.
  injection <- grepl('!!', normalized$source, fixed = TRUE)
  normalized$status[injection] <- 'excluded-injection-phase'
  rownames(normalized) <- NULL
  normalized
}
report_observations <- function(results) {
  print(table(results$status))
  cat('Executed distinct frame/expression/position comparisons:', nrow(results), '\n')
  candidates <- results[grepl('candidate', results$status), ]
  print(utils::head(candidates[c('source', 'parent', 'status', 'oracle', 'actual')], 25L))
  stopifnot(nrow(candidates) == 0L)
}
result_path <- 'investigation/2026-10-31-expand-grouping-greedy/spec/selection-results.csv'
if ('--classify-existing' %in% commandArgs(trailingOnly = TRUE)) {
  results <- normalize_observations(read.csv(result_path))
  write.csv(results, result_path, row.names = FALSE)
  report_observations(results)
  quit(status = 0L)
}
set.seed(2026103101)
frames <- list(
  tibble::tibble(a = c('x', 'y'), b = c(1L, 2L), c = c(TRUE, FALSE), d = as.Date(c('2020-01-01', '2020-01-02'))),
  tibble::tibble(a = integer(), b = character(), c = factor(), d = list())
)
selectors <- list(
  quote(a), quote(b), quote(c), quote(d), quote(everything()),
  quote(where(is.numeric)), quote(where(is.character)), quote(where(is.factor)),
  quote(where(function(x) length(x) == 0L)),
  quote(all_of(names(tidyselect::peek_data())[vapply(tidyselect::peek_data(), is.numeric, logical(1))])),
  quote(all_of(names(tidyselect::peek_data()))),
  quote(any_of(c('a', 'missing'))), quote(starts_with('a')),
  quote(c(1L, 3L)), quote(integer()), quote(NULL),
  quote(base::c('b', 'a')), quote((base::c)('b', 'a')),
  quote(.data[['b']]), quote((.data)[['b']]),
  quote((function(x) names(x)[vapply(x, is.numeric, logical(1))])(tidyselect::peek_data())),
  quote(all_of((function(x) x)(c('d', 'a')))), quote(base::get('external')),
  quote((function(...) c(...))('a', 'b')),
  quote(marginplyr::grouping_set(a)),
  quote((function() marginplyr::grouping_set(a))()),
  quote(~a), quote(!!rlang::quo(a)), quote(a && b), quote(a ^ b)
)
external <- c('b', 'c')
env <- environment()
ops <- c('c', '|', '&', '/', ':', '!', '-')
expressions <- selectors
for (i in seq_len(1500L)) {
  op <- sample(ops, 1L)
  left <- sample(expressions[seq_len(min(length(expressions), 150L))], 1L)[[1L]]
  if (op %in% c('!', '-')) {
    value <- rlang::call2(op, left)
  } else {
    right <- sample(selectors, 1L)[[1L]]
    value <- rlang::call2(op, left, right)
  }
  expressions[[length(expressions) + 1L]] <- value
}
capture <- function(expr) tryCatch(expr, error = identity)
condition_kind <- function(x) class(x)[[1L]]
constructors <- c('grouping_set', 'grouping_sets', 'rollup', 'cube', 'grouping_spec')
recognized_spelling <- utils::getFromNamespace('static_spelling_name', 'marginplyr')
rows <- list()
for (f in seq_along(frames)) {
  data <- frames[[f]]
  for (i in seq_along(expressions)) {
    expression <- expressions[[i]]
    oracle <- suppressWarnings(capture(tidyselect::eval_select(expression, data, env, allow_rename = FALSE)))
    # A written constructor call denotes a specification rather than selection.
    if (!is.null(recognized_spelling(expression, 'grouping_constructor'))) next
    for (parent in c('.by', constructors)) {
      call <- if (parent == '.by') {
        rlang::expr(inspect_grouping(data, .by = !!expression, .format = 'list'))
      } else {
        spec_call <- rlang::call2(parent, expression)
        rlang::expr(inspect_grouping(data, .grouping = !!spec_call, .format = 'list', .duplicates = 'keep'))
      }
      actual <- suppressWarnings(capture(eval(call, env)))
      status <- 'match'
      if (inherits(oracle, 'error')) {
        if (!inherits(actual, 'error') || !identical(condition_kind(oracle), condition_kind(actual))) {
          # These Package conditions deliberately contextualize an unsupported nested specification.
          if (inherits(actual, 'marginplyr_error') && grepl('nested position|composite dimension', conditionMessage(actual))) {
            status <- 'expected-specification-refusal'
          } else status <- 'candidate-error'
        }
      } else if (length(oracle) == 0L && parent %in% c('rollup', 'cube')) {
        status <- if (inherits(actual, 'marginplyr_error') && grepl('at least one dimension', conditionMessage(actual))) 'expected-empty-refusal' else 'candidate-empty'
      } else if (inherits(actual, 'error')) {
        status <- 'candidate-refusal'
      } else {
        selected <- if (parent == '.by') actual$fixed[[1L]] else actual$included[[1L]]
        if (!identical(selected, names(oracle))) status <- 'candidate-values'
      }
      rows[[length(rows) + 1L]] <- data.frame(frame = f, expression = i, parent = parent, status = status,
        source = paste(rlang::expr_deparse(expression), collapse = '\n'),
        oracle = if (inherits(oracle, 'error')) conditionMessage(oracle) else paste(names(oracle), collapse = ','),
        actual = if (inherits(actual, 'error')) conditionMessage(actual) else paste(if (parent == '.by') actual$fixed[[1L]] else actual$included[[1L]], collapse = ','))
    }
  }
}
results <- normalize_observations(do.call(rbind, rows))
write.csv(results, result_path, row.names = FALSE)
report_observations(results)
