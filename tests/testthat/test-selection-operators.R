test_that("ordinary c calls force wrapper promises against typed data", {
  data <- tibble::tibble(cols = c("x", "y"), key = c(1, 1))
  numeric_columns <- function() {
    data <- tidyselect::peek_data()
    names(data)[vapply(data, is.numeric, logical(1))]
  }
  direct <- function(cols = numeric_columns()) {
    names(dplyr::select(data, base::c(cols)))
  }
  fixed <- function(cols = numeric_columns()) {
    inspect_grouping(data, .by = base::c(cols), .format = "list")
  }
  dimension <- function(cols = numeric_columns()) {
    inspect_grouping(
      data, .grouping = grouping_set(base::c(cols)), .format = "list"
    )
  }
  expect_identical(direct(), "key")
  expect_identical(fixed()$fixed, list("key"))
  expect_identical(dimension()$included, list("key"))
  expect_identical(
    fixed(
      if (is.numeric(tidyselect::peek_data()$cols)) "cols" else "key"
    )$fixed,
    list("key")
  )
  expect_identical(
    dimension(
      if (is.numeric(tidyselect::peek_data()$cols)) "cols" else "key"
    )$included,
    list("key")
  )
})

# Each ordinary head evaluates cols lexically; the bare forms use selection
# syntax. Include both parentheses around a head and around its whole call.
ordinary_operator_selections <- function() {
  operands <- list(
    c = list(quote(cols)),
    `:` = list(quote(cols), 2L),
    `-` = list(quote(cols), 0L),
    unary_minus = list(quote(cols)),
    `/` = list(quote(cols), 1L),
    `!` = list(quote(cols)),
    `&` = list(quote(cols), 1L),
    `|` = list(quote(cols), 1L)
  )
  selections <- list()
  for (operator in names(operands)) {
    name <- if (operator == "unary_minus") "-" else operator
    qualified <- rlang::call2("::", quote(base), rlang::sym(name))
    heads <- list(
      qualified,
      rlang::call2("(", rlang::sym(name)),
      rlang::call2("(", qualified)
    )
    for (head in heads) {
      call <- rlang::call2(head, !!!operands[[operator]])
      selections <- c(selections, list(call, rlang::call2("(", call)))
    }
  }
  selections
}

# Compare every constructor's public inspection with a direct typed selection.
expect_typed_operator_plans <- function(as_source) {
  data <- tibble::tibble(cols = c("x", "y"), key = c(1, 1), other = "z")
  source <- as_source(data)
  direct <- function(selection, cols = if (
    is.numeric(tidyselect::peek_data()$cols)
  ) 1L else 2L) {
    tidyselect::eval_select(selection, data, env = environment())
  }
  inspect <- function(selection, constructor = NULL, cols = if (
    is.numeric(tidyselect::peek_data()$cols)
  ) 1L else 2L) {
    if (is.null(constructor)) {
      return(inspect_grouping(source, .by = !!selection, .format = "list"))
    }
    spec <- eval(rlang::call2(constructor, selection))
    inspect_grouping(source, .grouping = spec, .format = "list")
  }
  constructors <- c(
    "grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec"
  )
  for (selection in ordinary_operator_selections()) {
    expected <- tryCatch(direct(selection), error = identity)
    if (inherits(expected, "error")) {
      fixed <- expect_error(inspect(selection), class = class(expected)[[1L]])
      expect_identical(conditionMessage(fixed), conditionMessage(expected))
      for (constructor in constructors) {
        dimension <- expect_error(
          inspect(selection, constructor), class = class(expected)[[1L]]
        )
        expect_identical(
          conditionMessage(dimension), conditionMessage(expected)
        )
      }
    } else {
      columns <- names(expected)
      expect_identical(inspect(selection)$fixed, list(columns))
      control <- rlang::expr(tidyselect::all_of(!!columns))
      for (constructor in constructors) {
        expect_identical(
          inspect(selection, constructor), inspect(control, constructor),
          info = rlang::expr_deparse(selection)
        )
      }
    }
  }
}

test_that("ordinary operators inspect typed keys and every kind locally", {
  expect_typed_operator_plans(identity)
})

test_that("ordinary operators inspect typed keys and every kind on dtplyr", {
  skip_if_suggest_absent("dtplyr")
  expect_typed_operator_plans(dtplyr::lazy_dt)
})

# Cell contents expose the source-row split that an incorrect key causes.
expect_typed_operator_nesting <- function(as_source) {
  data <- tibble::tibble(cols = c("x", "y"), key = c(1, 1))
  source <- as_source(data)
  numeric_columns <- function() {
    data <- tidyselect::peek_data()
    names(data)[vapply(data, is.numeric, logical(1))]
  }
  run <- function(verb, selection, position, cols = numeric_columns()) {
    call <- rlang::call2(verb, source, .margin_label = NULL)
    if (position == "fixed") {
      call$.by <- selection
    } else {
      call$.grouping <- rlang::call2("grouping_set", selection)
    }
    dplyr::collect(eval(call))
  }
  selections <- list(
    quote(base::c(cols)), quote((c)(cols)),
    quote((base::c)(cols)), quote((base::c(cols)))
  )
  for (selection in selections) {
    for (verb in c("nest_with_margins", "nest_by_with_margins")) {
      for (position in c("fixed", "dimension")) {
        result <- run(verb, selection, position)
        expect_identical(names(result), c("key", "data"))
        expect_identical(result$key, 1)
        expect_length(result$data, 1L)
        expect_identical(nrow(result$data[[1L]]), 2L)
        expect_identical(as.list(result$data[[1L]]), list(cols = c("x", "y")))
      }
    }
  }
}

test_that("typed wrapper selections preserve both nesting verbs' local cells", {
  expect_typed_operator_nesting(identity)
})

test_that("typed wrapper selections preserve both verbs' dtplyr cells", {
  skip_if_suggest_absent("dtplyr")
  expect_typed_operator_nesting(dtplyr::lazy_dt)
})

test_that("ordinary operator bindings do not cause a premature duplicate set", {
  data <- tibble::tibble(cols = c("x", "y"), key = c(1, 1))
  env <- rlang::env(rlang::current_env())
  reads <- 0L
  makeActiveBinding("cols", function() {
    reads <<- reads + 1L
    if (is.numeric(tidyselect::peek_data()$cols)) "cols" else "key"
  }, env)
  for (selection in rlang::exprs(
    base::c(cols), (c)(cols), (base::c)(cols), tidyselect::all_of(cols)
  )) {
    reads <- 0L
    result <- eval(rlang::expr(inspect_grouping(
      data, .grouping = grouping_sets(!!selection, cols), .format = "list"
    )), env)
    expect_identical(result$included, list("key", "cols"))
    expect_identical(reads, 2L)
  }
})
