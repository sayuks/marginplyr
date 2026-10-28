# These heads evaluate their operands lexically in tidyselect.
ordinary_subscript_heads <- function(operator) {
  qualified <- rlang::call2("::", quote(base), rlang::sym(operator))
  list(qualified, rlang::call2("(", rlang::sym(operator)),
       rlang::call2("(", qualified))
}

# Compare fixed-key inspection and expansion with a direct typed selection.
expect_typed_subscript_keys <- function(as_source) {
  data <- tibble::tibble(
    region = c("east", "west"), amount = c(1, 1), segment = c("x", "y")
  )
  source <- as_source(data)
  .data <- plain_map <- names(data)
  calls <- 0L
  first_numeric <- function() {
    calls <<- calls + 1L
    which(vapply(tidyselect::peek_data(), is.numeric, logical(1)))[[1L]]
  }
  for (head in ordinary_subscript_heads("[[")) {
    for (operand in c(".data", "plain_map")) {
      selection <- rlang::call2(head, rlang::sym(operand),
                                quote(first_numeric()))
      for (expr in list(selection, rlang::call2("(", selection))) {
        expected <- names(tidyselect::eval_select(expr, data))
        expect_identical(expected, "amount")
        calls <- 0L
        expect_identical(
          inspect_grouping(source, .by = !!expr, .format = "list")$fixed,
          list(expected)
        )
        expect_identical(calls, 1L)
        control <- expand_with_margins(
          source, .by = tidyselect::all_of(expected),
          .grouping = rollup(segment), .margin_label = NULL, .id = "sid"
        )
        calls <- 0L
        result <- expand_with_margins(
          source, .by = !!expr, .grouping = rollup(segment),
          .margin_label = NULL, .id = "sid"
        )
        expect_identical(calls, 1L)
        expect_identical(dplyr::collect(result), dplyr::collect(control))
      }
    }
  }
}

test_that("ordinary .data subscripts select typed fixed keys locally", {
  expect_typed_subscript_keys(identity)
})

test_that("ordinary .data subscripts select typed fixed keys on dtplyr", {
  skip_if_suggest_absent("dtplyr")
  expect_typed_subscript_keys(dtplyr::lazy_dt)
})

# A promise must first be forced in the typed selection context. In particular,
# the integer names proxy has no character column for the second predicate.
expect_typed_subscript_dimensions <- function(as_source) {
  data <- tibble::tibble(
    region = c("east", "west"), amount = c(1, 1), segment = c("x", "y")
  )
  source <- as_source(data)
  .data <- plain_map <- names(data)
  calls <- 0L
  first_matching <- function() {
    calls <<- calls + 1L
    which(vapply(tidyselect::peek_data(), predicate, logical(1)))[[1L]]
  }
  direct <- function(selection, chosen = first_matching()) {
    names(tidyselect::eval_select(selection, data, env = environment()))
  }
  inspect <- function(selection, constructor, chosen = first_matching()) {
    spec <- eval(rlang::call2(constructor, selection))
    inspect_grouping(source, .grouping = spec, .format = "list")
  }
  for (predicate in list(is.numeric, is.character)) {
    for (head in ordinary_subscript_heads("[[")) {
      for (operand in c(".data", "plain_map")) {
        selection <- rlang::call2(head, rlang::sym(operand), quote(chosen))
        for (expr in list(selection, rlang::call2("(", selection))) {
          columns <- direct(expr)
          expect_identical(columns, if (identical(predicate, is.numeric)) {
            "amount"
          } else {
            "region"
          })
          control <- rlang::expr(tidyselect::all_of(!!columns))
          for (constructor in c(
            "grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec"
          )) {
            calls <- 0L
            expect_identical(inspect(expr, constructor),
                             inspect(control, constructor))
            expect_identical(calls, 1L)
          }
        }
      }
    }
  }
}

test_that("ordinary subscripts retain typed wrapper dimensions locally", {
  expect_typed_subscript_dimensions(identity)
})

test_that("ordinary subscripts retain typed wrapper dimensions on dtplyr", {
  skip_if_suggest_absent("dtplyr")
  expect_typed_subscript_dimensions(dtplyr::lazy_dt)
})

test_that("ordinary dollar heads force lexical .data against typed data", {
  data <- tibble::tibble(region = c("east", "west"), amount = c(1, 1))
  inspect <- function(selection, .data = list(key = names(data)[vapply(
    tidyselect::peek_data(), is.numeric, logical(1)
  )])) {
    inspect_grouping(data, .by = !!selection, .format = "list")
  }
  for (head in ordinary_subscript_heads("$")) {
    selection <- rlang::call2(head, quote(.data), quote(key))
    expect_identical(inspect(selection)$fixed, list("amount"))
  }
})

test_that("bare dynamic .data subscripts keep tidyselect's early refusal", {
  data <- tibble::tibble(region = "east", amount = 1)
  calls <- 0L
  first_numeric <- function() {
    calls <<- calls + 1L
    which(vapply(tidyselect::peek_data(), is.numeric, logical(1)))[[1L]]
  }
  # A forwarded quosure retains the written subscript through tidy capture.
  selection <- rlang::new_quosure(quote(.data[[first_numeric()]]), environment())
  direct <- expect_error(tidyselect::eval_select(selection, data),
                         "constant", class = "rlang_error")
  local_mocked_bindings(grouping_selection_proxy = function(...) {
    stop("unexpected metadata read")
  })
  fixed_call <- function() inspect_grouping(data, .by = !!selection)
  dimension_call <- function() {
    spec <- rollup(!!selection)
    inspect_grouping(data, .grouping = spec)
  }
  fixed <- expect_error(fixed_call(), class = "rlang_error")
  dimension <- expect_error(dimension_call(), class = "rlang_error")
  for (error in list(fixed, dimension)) {
    expect_identical(class(error), class(direct))
    expect_identical(conditionMessage(error), conditionMessage(direct))
    expect_identical(error$parent, direct$parent)
  }
  expect_identical(calls, 0L)
})
