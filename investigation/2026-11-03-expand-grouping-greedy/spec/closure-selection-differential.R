Sys.setenv(TESTTHAT_PARALLEL = "false")
source_path <- Sys.getenv("MARGINPLYR_HUNT_SOURCE", unset = ".")
pkgload::load_all(source_path, quiet = TRUE)
options(lifecycle_verbosity = "quiet")
out_dir <- "investigation/2026-11-03-expand-grouping-greedy/spec"

# Every expression below is evaluated in the same lexical environment by a
# direct typed tidyselect oracle and by each public inspection position.
base_env <- rlang::env(globalenv(),
  external = c("a", "b"), other = c("b", "c"),
  scalar = "a", empty = character(), offset = 0L)
base_env$vars <- function() tidyselect::peek_vars()
base_env$types <- function() names(Filter(is.numeric, tidyselect::peek_data()))
base_env$numeric <- function(x = tidyselect::peek_data()) names(Filter(is.numeric, x))
base_env$default_chain <- function(x = y, y = z, z = names(tidyselect::peek_data())) x
base_env$forward <- function(x = character(), ...) c(x, ...)
base_env$nested <- function(x = external) { y <- x; unname(y) }
base_env$lexical <- local({ x <- c("a", "c"); function(y = x) y })
base_env$helper <- function(x = tidyselect::peek_vars()) x
base_env$identity <- base::identity
base_env$outer <- function(x = numeric()) x
base_env$bound <- function(names = base::names, x = tidyselect::peek_data()) names(x)
base_env$recurse <- function(x = 0L) if (x) recurse(x - 1L) else tidyselect::peek_vars()
base_env$eager <- function(x = c("a", "b"), y = x) y
base_env$pick <- function(x = scalar) x
base_env$arg_order <- function(x = y, y = scalar, ...) c(x, ...)
base_env$paren <- function(x = (((external)))) (((x)))
base_env$kw <- function(alpha = external, beta = empty) c(alpha, beta)
base_env$names_alias <- base::names
base_env$peek_alias <- tidyselect::peek_data
for (nm in ls(base_env)) {
  value <- get(nm, base_env)
  if (is.function(value) && identical(environment(value), globalenv()) && !is.primitive(value)) {
    environment(value) <- base_env
    assign(nm, value, base_env)
  }
}

leaves <- c(
  'external', 'scalar', 'empty', 'vars()', 'types()', 'numeric()',
  'numeric(tidyselect::peek_data())', 'default_chain()',
  'forward()', 'forward(external)', 'forward(external, "c")',
  'nested()', 'nested(other)', 'lexical()', 'lexical(external)',
  'helper()', 'helper(names(tidyselect::peek_data()))',
  'identity(types())', 'outer()', 'bound()', 'recurse()', 'eager()',
  'pick()', 'arg_order()', 'arg_order(y = other)',
  'paren()', 'kw(al = scalar)', 'kw(beta = other)',
  'names_alias(tidyselect::peek_data())', 'names(peek_alias())',
  '{ x <- external; x }', '{ x <- types(); x }',
  '(function(x = external) x)()',
  '(function(x = names(tidyselect::peek_data())) x)()',
  '(function(...) c(...))()', '(function(...) c(...))(external)',
  '{ local_vector <- external; local_vector[seq_along(local_vector)] }',
  'default_chain(y = other)', 'eager(y = other)', 'kw(beta = empty, alpha = scalar)'
)
helper_exprs <- unlist(lapply(leaves, function(x) c(
  paste0('all_of(', x, ')'), paste0('any_of(', x, ')'),
  paste0('dplyr::all_of(', x, ')'),
  paste0('(tidyselect::all_of)(', x, ')'),
  paste0('all_of(unname(', x, '))'),
  paste0('all_of(c(', x, ', character()))')
)), use.names = FALSE)
extra <- c('where(is.numeric)', 'where(is.character)', 'where(is.logical)',
           'where(~length(.x) == 0L)', 'where(~anyNA(.x))',
           'c(a, b)', 'a:c', 'c(a, b)/b', '!c(c)',
           'all_of(names(tidyselect::peek_data()))',
           'all_of(names(tidyselect::peek_data())[1])')
exprs <- unique(c(helper_exprs, extra))
inputs <- list(
  int_chr_logical = tibble::tibble(a = 1:3, b = c("x", NA, "z"), c = c(TRUE, FALSE, TRUE)),
  chr_double_integer = tibble::tibble(a = letters[1:3], b = c(1, 2, NA), c = 3:1),
  zero_rows = tibble::tibble(a = integer(), b = character(), c = logical()),
  numeric_all = tibble::tibble(a = 1:2, b = c(1.1, 2.2), c = 2:3),
  named_other_order = tibble::tibble(c = TRUE, a = "x", b = 1L)
)

capture <- function(expr, env, input) {
  q <- rlang::new_quosure(expr, env)
  tryCatch(tidyselect::eval_select(q, input, allow_rename = TRUE), error = identity)
}
construct <- function(position, expr, env, input) {
  e <- rlang::env(env, input = input)
  call <- if (position == '.by') {
    rlang::call2('inspect_grouping', quote(input), .by = expr, .format = 'list')
  } else {
    rlang::call2('inspect_grouping', quote(input),
      .grouping = rlang::call2(position, expr), .format = 'list')
  }
  tryCatch(eval(call, e), error = identity)
}
rows <- list()
for (input_name in names(inputs)) {
  input <- inputs[[input_name]]
  for (expr_text in exprs) {
    expr <- parse(text = expr_text)[[1L]]
    expected <- capture(expr, base_env, input)
    for (position in c('.by', 'grouping_set', 'grouping_sets', 'rollup', 'cube', 'grouping_spec')) {
      got <- construct(position, expr, base_env, input)
      status <- if (inherits(expected, 'error')) {
        if (inherits(got, 'error') && identical(class(expected)[[1L]], class(got)[[1L]])) 'matching_external_error' else 'mismatch'
      } else if (!length(expected) && position %in% c('rollup', 'cube')) {
        if (inherits(got, 'marginplyr_error') && grepl('requires at least one dimension', conditionMessage(got))) 'required_empty_refusal' else 'mismatch'
      } else if (inherits(got, 'error')) {
        'mismatch'
      } else {
        selected <- names(input)[unname(expected)]
        actual <- if (position == '.by') got$fixed[[1L]] else unique(unlist(got$included, use.names = FALSE))
        if (identical(selected, actual)) 'match' else 'mismatch'
      }
      rows[[length(rows) + 1L]] <- data.frame(
        input = input_name, expression = expr_text, position = position,
        status = status,
        expected = if (inherits(expected, 'error')) conditionMessage(expected) else paste(names(input)[unname(expected)], collapse = '|'),
        observed = if (inherits(got, 'error')) conditionMessage(got) else paste(if (position == '.by') got$fixed[[1L]] else unique(unlist(got$included, use.names = FALSE)), collapse = '|')
      )
    }
  }
}
results <- do.call(rbind, rows)
write.csv(results, file.path(out_dir, 'closure-selection-results.csv'), row.names = FALSE)
print(table(results$status))
print(results[results$status == 'mismatch', ], row.names = FALSE)
cat('Expressions:', length(exprs), 'comparisons:', nrow(results), '\n')
print(sessionInfo())
stopifnot(!any(results$status == 'mismatch'))
