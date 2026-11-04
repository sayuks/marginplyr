Sys.setenv(TESTTHAT_PARALLEL = 'false')
pkgload::load_all(Sys.getenv('MARGINPLYR_HUNT_SOURCE', unset = '.'), quiet = TRUE)
options(lifecycle_verbosity = 'quiet')
out_dir <- 'investigation/2026-11-04-summary-grouping-share-bug-hunt/grouping'
inputs <- list(
  populated = tibble::tibble(a = c('x', NA), b = 1:2, c = c(TRUE, FALSE)),
  zero_rows = tibble::tibble(a = character(), b = integer(), c = logical()),
  reordered = tibble::tibble(c = 1:2, b = c('x', 'y'), a = c(TRUE, FALSE))
)
forms <- c("all_of(..01)", "all_of(`..+1`)", "all_of(`.. 1`)", "all_of(`..-1`)",
"all_of(`..1x`)", "all_of(c(..01, ..02))", "all_of(base::c(`..+1`))"
)
arguments <- list(
  empty = list(), literal = list('a'),
  typed = list(quote(pick())),
  two_typed = list(quote(pick()), quote(pick())),
  three = list(quote(pick()), 'b', 'c'),
  missing = list(rlang::missing_arg()),
  missing_first = list(rlang::missing_arg(), 'b'),
  nested_empty = list(quote(character()))
)
run <- function(input, expr, args, position) {
  e <- rlang::env(globalenv(), input = input, forces = 0L)
  e$pick <- eval(quote(function() {
    forces <<- forces + 1L
    names(Filter(is.character, tidyselect::peek_data()))
  }), e)
  # Capture the selection inside the actual dots frame in both paths.
  body <- if (position == 'native') {
    e$eval_select <- tidyselect::eval_select
    e$new_quosure <- rlang::new_quosure
    rlang::call2('eval_select',
      rlang::call2('new_quosure', rlang::call2('quote', expr),
        rlang::call2('environment')), quote(input))
  } else if (position == '.by') {
    rlang::call2('inspect_grouping', quote(input), .by = expr, .format = 'list')
  } else {
    rlang::call2('inspect_grouping', quote(input),
      .grouping = rlang::call2(position, expr), .format = 'list')
  }
  e$f <- eval(rlang::new_function(pairlist(... = rlang::missing_arg()), body, env = e), e)
  result <- tryCatch(eval(rlang::call2('f', !!!args), e), error = identity)
  list(result = result, forces = e$forces)
}
rows <- list()
for (input_name in names(inputs)) {
  input <- inputs[[input_name]]
  for (form in forms) for (argument_name in names(arguments)) {
    expr <- parse(text = form)[[1L]]
    native <- run(input, expr, arguments[[argument_name]], 'native')
    for (position in c('.by', 'grouping_set', 'grouping_sets', 'rollup', 'cube', 'grouping_spec')) {
      got <- run(input, expr, arguments[[argument_name]], position)
      expected <- native$result
      actual <- got$result
      status <- if (inherits(expected, 'error')) {
        if (inherits(actual, 'error') && identical(class(expected)[[1L]], class(actual)[[1L]]) && identical(native$forces, got$forces)) 'matching_external_error' else 'mismatch'
      } else if (!length(expected) && position %in% c('rollup', 'cube')) {
        if (inherits(actual, 'marginplyr_error') && grepl('requires at least one dimension', conditionMessage(actual)) && identical(native$forces, got$forces)) 'required_empty_refusal' else 'mismatch'
      } else if (inherits(actual, 'error')) 'mismatch' else {
        selected <- names(input)[unname(expected)]
        observed <- if (position == '.by') actual$fixed[[1L]] else unique(unlist(actual$included, use.names = FALSE))
        if (identical(selected, observed) && identical(native$forces, got$forces)) 'match' else 'mismatch'
      }
      rows[[length(rows) + 1L]] <- data.frame(input = input_name, expression = form,
        arguments = argument_name, position = position, status = status,
        native_forces = native$forces, package_forces = got$forces,
        expected = if (inherits(expected, 'error')) conditionMessage(expected) else paste(names(input)[unname(expected)], collapse = '|'),
        observed = if (inherits(actual, 'error')) conditionMessage(actual) else paste(if (position == '.by') actual$fixed[[1L]] else unique(unlist(actual$included, use.names = FALSE)), collapse = '|'))
    }
  }
}
results <- do.call(rbind, rows)
write.csv(results, file.path(out_dir, 'dots-selection-results.csv'), row.names = FALSE)
print(table(results$status))
print(results[results$status == 'mismatch', ], row.names = FALSE)
cat('Comparisons:', nrow(results), '\n')
stopifnot(!any(results$status == 'mismatch'))
