# A public deterministic metadata-admission reproduction. No implementation fix.
args <- commandArgs(trailingOnly = TRUE)
assert <- '--assert' %in% args
roots <- args[!startsWith(args, '--')]
if (length(roots)) setwd(roots[[1L]])
pkgload::load_all('.', quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
fixture <- function(binding = 'changing', size = 2L, grouped = FALSE) {
  state <- new.env(parent = emptyenv())
  state$lookups <- 0L
  state$argument_lengths <- integer()
  operator_env <- new.env(parent = environment())
  custom <- function(e1, e2) {
    state$argument_lengths <- c(state$argument_lengths, length(e1))
    if (!length(e1)) character() else base::`+`(e1, e2)
  }
  if (binding == 'changing') {
    makeActiveBinding('+', function() {
      state$lookups <- base::`+`(state$lookups, 1L)
      if (state$lookups == 3L) custom else base::`+`
    }, operator_env)
  } else if (binding == 'base') {
    assign('+', base::`+`, envir = operator_env)
  } else if (binding == 'custom') {
    assign('+', custom, envir = operator_env)
  }
  root_input <- tibble(value = seq_len(size))
  if (grouped) root_input$g <- rep('a', size)
  source <- eval(quote(mutate(
    dtplyr::lazy_dt(root_input),
    derived = value + 1L
  )), envir = operator_env)
  list(source = source, state = state)
}
# Every operator returned by the active binding agrees on populated input.
# Five sequential ordinary collections include the special third lookup.
control <- fixture()
cat('Lookups after ordinary dtplyr construction:', control$state$lookups, '\n')
for (i in seq_len(5L)) {
  materialized <- collect(control$source)
  stopifnot(identical(materialized$derived, 2:3))
}
cat('Five sequential ordinary collections:', typeof(materialized$derived), '2, 3\n')
cat('Changing-operator argument lengths in ordinary collection:',
    paste(control$state$argument_lengths, collapse = ', '), '\n')
expected <- inspect_grouping(materialized, .by = where(is.character), .format = 'list')
probe <- fixture()
actual <- tryCatch(
  inspect_grouping(probe$source, .by = where(is.character), .format = 'list'),
  error = identity
)
cat('Expected fixed keys:'); print(expected$fixed)
if (inherits(actual, 'error')) {
  cat('Actual inspection condition:', conditionMessage(actual), '\n')
} else {
  cat('Actual fixed keys:'); print(actual$fixed)
}
cat('Lookup count during inspection:', probe$state$lookups, '\n')
cat('Changing-operator argument lengths during inspection:',
    paste(probe$state$argument_lengths, collapse = ', '), '\n')
safely_refused <- function(value, state) {
  inherits(value, 'marginplyr_error') && !length(state$argument_lengths)
}
failures <- !identical(actual, expected) && !safely_refused(actual, probe$state)
for (verb in list(nest_with_margins, nest_by_with_margins)) {
  name <- if (identical(verb, nest_with_margins)) 'nest_with_margins' else 'nest_by_with_margins'
  expected_nest <- verb(materialized, .by = where(is.character))
  stopifnot(nrow(expected_nest) == 1L, nrow(expected_nest$data[[1L]]) == 2L)
  probe <- fixture()
  actual_nest <- tryCatch(
    verb(probe$source, .by = where(is.character)), error = identity
  )
  before_collect <- probe$state$argument_lengths
  if (inherits(actual_nest, 'dtplyr_step')) actual_nest <- collect(actual_nest)
  if (inherits(actual_nest, 'error')) {
    cat(name, 'actual condition:', conditionMessage(actual_nest), '\n')
    failures <- c(failures, !safely_refused(actual_nest, probe$state))
    next
  }
  cat(name, 'expected outer rows:', nrow(expected_nest), '\n')
  cat(name, 'actual outer rows:', nrow(actual_nest), '\n')
  cat(name, 'actual names:', paste(names(actual_nest), collapse = ', '), '\n')
  cat(name, 'actual cell row counts:',
      paste(vapply(actual_nest$data, nrow, integer(1)), collapse = ', '), '\n')
  cat(name, 'custom-operator argument lengths before own collection:',
      paste(before_collect, collapse = ', '), '\n')
  failures <- c(failures, nrow(actual_nest) != nrow(expected_nest))
}
# Stable standard arithmetic is accepted; a plainly nonstandard binding is
# already refused before its row-dependent implementation can run on a proxy.
base_control <- fixture('base')
stopifnot(identical(
  inspect_grouping(base_control$source, .by = where(is.character), .format = 'list'),
  expected
))
for (verb in list(nest_with_margins, nest_by_with_margins)) {
  base_control <- fixture('base')
  materialized_base <- collect(base_control$source)
  expected_base <- verb(materialized_base, .by = where(is.character))
  actual_base <- verb(base_control$source, .by = where(is.character))
  if (inherits(actual_base, 'dtplyr_step')) actual_base <- collect(actual_base)
  normalized <- function(value) {
    result <- as.list(ungroup(value))
    result$data <- lapply(result$data, as.list)
    result
  }
  stopifnot(identical(normalized(actual_base), normalized(expected_base)))
}
cat('Fixed base nesting controls: both matched cells and memberships\n')
custom_control <- fixture('custom')
refusal <- tryCatch(
  inspect_grouping(custom_control$source, .by = where(is.character)),
  error = identity
)
stopifnot(inherits(refusal, 'marginplyr_error'))
stopifnot(identical(custom_control$state$argument_lengths, integer()))
cat('Fixed base control: matched\nFixed custom control:', conditionMessage(refusal), '\n')
name_control <- fixture()
stopifnot(identical(
  inspect_grouping(name_control$source, .by = everything(), .format = 'list')$fixed[[1L]],
  c('value', 'derived')
))
stopifnot(identical(name_control$state$lookups, 0L))
cat('Names-only inspection operator lookups:', name_control$state$lookups, '\n')
# The same false character type also reaches omitted-dimension labelling.
# A separate populated character key makes this rollup valid in the oracle.
for (verb in list(nest_with_margins, nest_by_with_margins)) {
  probe <- fixture(grouped = TRUE)
  materialized <- collect(probe$source)
  expected_rollup <- verb(
    materialized, .grouping = rollup(where(is.character)), .id = 'set'
  )
  stopifnot(nrow(expected_rollup) == 2L)
  probe <- fixture(grouped = TRUE)
  actual_rollup <- tryCatch({
    result <- verb(
      probe$source, .grouping = rollup(where(is.character)), .id = 'set'
    )
    if (inherits(result, 'dtplyr_step')) collect(result) else result
  }, error = identity)
  if (inherits(actual_rollup, 'error')) {
    cat('Rollup actual condition classes:',
        paste(class(actual_rollup), collapse = ', '), '\n')
    cat(conditionMessage(actual_rollup), '\n')
    failures <- c(failures, !safely_refused(actual_rollup, probe$state))
  } else {
    failures <- c(failures, nrow(actual_rollup) != nrow(expected_rollup))
  }
}
if (assert && any(failures)) stop(
  'Active arithmetic metadata disagreed with the populated-source control.',
  call. = FALSE
)
