test_that("Grouping plans retain selections from parenthesized callees", {
  input <- tibble::tibble(a = c("x", "y"), b = c(1L, 1L))
  inspect <- function(spec) {
    inspect_grouping(input, .grouping = spec, .format = "list")$included
  }
  expand <- function(spec) {
    expand_with_margins(input, .grouping = spec)
  }
  nest_cells <- function(verb) {
    function(spec) {
      result <- verb(input, .grouping = spec)
      list(keys = result$a, cells = result$data)
    }
  }
  consumers <- list(
    inspect = inspect,
    expand = expand,
    nest = nest_cells(nest_with_margins),
    nest_by = nest_cells(nest_by_with_margins)
  )
  expected_nest <- list(
    keys = c("x", "y"),
    cells = list(input[1L, "b"], input[2L, "b"])
  )
  expected <- list(
    inspect = list("a"),
    expand = input,
    nest = expected_nest,
    nest_by = expected_nest
  )

  run <- function(consume, argument, pairs, second_condition) {
    observed <- rlang::env(
      head_reads = 0L, selector_reads = 0L, warnings = list()
    )
    first_warning <- simpleWarning("first selector evaluation")
    caller <- rlang::env()
    caller[["("]] <- function(x) {
      force(x)
      observed$head_reads <- observed$head_reads + 1L
      if (observed$selector_reads > 0L) {
        if (second_condition == "error") {
          stop("second callee evaluation")
        }
        warning("second callee evaluation")
        return(function(...) "b")
      }
      function(...) {
        observed$selector_reads <- observed$selector_reads + 1L
        warning(first_warning)
        "a"
      }
    }

    spec <- eval(rlang::call2("grouping_sets", argument), caller)
    result <- tryCatch(
      withCallingHandlers(consume(spec), warning = function(cnd) {
        observed$warnings <- c(observed$warnings, list(cnd))
        invokeRestart("muffleWarning")
      }),
      error = identity
    )
    expect_identical(observed$head_reads, pairs)
    expect_identical(observed$selector_reads, 1L)
    expect_identical(observed$warnings, list(first_warning))
    result
  }

  for (name in names(consumers)) {
    consume <- consumers[[name]]
    for (second_condition in c("warning", "error")) {
      expect_identical(
        run(consume, quote((rollup)()), 1L, second_condition),
        expected[[name]]
      )
      expect_identical(
        run(consume, quote(((marginplyr::rollup))()), 2L, second_condition),
        expected[[name]]
      )
    }
  }
})

test_that("direct selections and parenthesized nested calls keep their paths", {
  input <- tibble::tibble(a = c("x", "y"), b = c(1L, 1L))
  observed <- rlang::env(reads = 0L)
  select_a <- function() {
    observed$reads <- observed$reads + 1L
    "a"
  }
  direct <- inspect_grouping(
    input, .grouping = grouping_sets(select_a()), .format = "list"
  )
  nested <- inspect_grouping(
    input, .grouping = grouping_sets((marginplyr::rollup)(a)), .format = "list"
  )

  expect_identical(observed$reads, 1L)
  expect_identical(direct$included, list("a"))
  expect_identical(nested$included, list("a", character()))
})

test_that("explicit qualified parentheses retain the caller's selection", {
  input <- data.frame(a = c("x", "y"), b = c(1L, 1L))
  no_reads <- c(outer = 0L, inner = 0L, selector = 0L)
  observed <- rlang::env(reads = no_reads)
  outer_limit <- 1L
  first_selection <- function() {
    if (observed$reads[["selector"]] %% 2L == 1L) "a" else "b"
  }
  choose <- first_selection
  selector <- function() {
    observed$reads[["selector"]] <- observed$reads[["selector"]] + 1L
    choose()
  }
  lookup <- function(package, name) {
    package <- as.character(substitute(package))
    name <- as.character(substitute(name))
    if (package == "base" && name == "(") {
      observed$reads[["outer"]] <- observed$reads[["outer"]] + 1L
      if (observed$reads[["outer"]] > outer_limit) stop("second outer lookup")
      return(get("(", baseenv()))
    }
    stopifnot(package == "marginplyr", name == "rollup")
    observed$reads[["inner"]] <- observed$reads[["inner"]] + 1L
    selector
  }
  caller <- rlang::env(`::` = lookup, `:::` = lookup, rollup = selector)
  inspect <- function(spec) {
    inspect_grouping(input, .grouping = spec, .format = "list")$included
  }
  expand <- function(spec) expand_with_margins(input, .grouping = spec)
  qualified <- quote(base::`(`(marginplyr::rollup)())
  spellings <- list(
    qualified = qualified,
    quoted_package = quote("base"::`(`(marginplyr::rollup)()),
    internal = quote(base::`(`(marginplyr:::rollup)()),
    bare = quote(base::`(`(rollup)()),
    wrapped = quote((base::`(`(marginplyr::rollup))()),
    whole = quote(base::`(`(base::`(`(marginplyr::rollup)())),
    whole_wrapped = quote((base::`(`(base::`(`(marginplyr::rollup)())))
  )
  one_read <- c(outer = 1L, inner = 1L, selector = 1L)
  for (spelling in names(spellings)) {
    expr <- spellings[[spelling]]
    spec <- eval(rlang::expr(grouping_sets(!!expr)), caller)
    original <- rlang::duplicate(spec)
    expected_reads <- one_read
    expected_reads[["inner"]] <- as.integer(spelling != "bare")
    for (consume in list(inspect, expand)) {
      observed$reads <- no_reads
      expect_identical(consume(spec), consume(grouping_sets(a)))
      expect_identical(observed$reads, expected_reads)
      expect_identical(spec, original)
    }
  }
  expect_identical(caller[["::"]], lookup)
  expect_identical(caller[[":::"]], lookup)
  expect_identical(caller$rollup, selector)

  selection <- rlang::new_quosure(qualified, caller)
  spec <- grouping_sets(!!selection)
  observed$reads <- no_reads
  outer_limit <- 2L
  expect_identical(inspect(spec), list("a"))
  expect_identical(inspect(spec), list("b"))
  expect_identical(observed$reads, 2L * one_read)

  observed$reads <- no_reads
  expect_identical(inspect(grouping_sets(!!selection, !!selection)),
                   list("a", "b"))
  expect_identical(observed$reads, 2L * one_read)

  outer_limit <- 1L
  observed$reads <- no_reads
  first_warning <- simpleWarning("first selector evaluation")
  choose <- function() {
    warning(first_warning)
    "a"
  }
  observed$warnings <- list()
  result <- withCallingHandlers(inspect(spec), warning = function(cnd) {
    observed$warnings <- c(observed$warnings, list(cnd))
    invokeRestart("muffleWarning")
  })
  expect_identical(result, list("a"))
  expect_identical(observed$warnings, list(first_warning))
  expect_identical(observed$reads, one_read)

  observed$reads <- no_reads
  failure <- rlang::error_cnd("selection_error", message = "first failure")
  choose <- function() stop(failure)
  expect_identical(tryCatch(inspect(spec), error = identity), failure)
  expect_identical(observed$reads, one_read)

  choose <- function() list("a")
  for (spelling in c("qualified", "whole")) {
    selection <- rlang::new_quosure(spellings[[spelling]], caller)
    observed$reads <- no_reads
    outer_limit <- 1L
    actual <- expect_error(inspect(grouping_sets(!!selection)),
                           class = "vctrs_error_subscript_type")
    expect_identical(observed$reads, one_read)

    observed$reads <- no_reads
    outer_limit <- 2L
    direct <- expect_error(tidyselect::eval_select(selection, input),
                           class = "vctrs_error_subscript_type")
    expect_identical(actual$subscript_arg, direct$subscript_arg)
    expect_identical(conditionMessage(actual), conditionMessage(direct))
  }
})
