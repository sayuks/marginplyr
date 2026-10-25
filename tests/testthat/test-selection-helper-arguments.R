test_that("helper arguments select fixed keys from typed metadata", {
  data <- tibble::tibble(a = c("x", "y"), b = c(1, 1))
  selection <- rlang::expr(tidyselect::all_of(
    names(tidyselect::peek_data())[
      vapply(tidyselect::peek_data(), is.numeric, logical(1))
    ]
  ))
  identity_selection <- function(x) x

  spellings <- list(selection, rlang::expr(identity_selection(!!selection)))
  for (spelling in spellings) {
    inspection <- inspect_grouping(data, .by = !!spelling)
    expect_identical(inspection$fixed, "(b)")
    result <- summarize_with_margins(data, n = dplyr::n(), .by = !!spelling)
    expect_identical(result, tibble::tibble(b = 1, n = 2L))
    expect_identical(
      result,
      dplyr::summarize(data, n = dplyr::n(), .by = !!spelling)
    )
  }
})

test_that("helper arguments select typed Grouping dimensions without overlap", {
  data <- tibble::tibble(a = c("x", "y"), b = c(1, 1))
  numeric_names <- rlang::expr(names(tidyselect::peek_data())[
    vapply(tidyselect::peek_data(), is.numeric, logical(1))
  ])
  identity_selection <- function(x) x

  for (helper in c("all_of", "any_of", "starts_with", "ends_with", "contains",
                   "matches", "one_of")) {
    selection <- rlang::call2(helper, numeric_names, .ns = "tidyselect")
    spellings <- list(selection, rlang::expr(identity_selection(!!selection)))
    for (spelling in spellings) {
      inspection <- inspect_grouping(
        data,
        .by = a,
        .grouping = grouping_set(!!spelling)
      )
      expect_identical(inspection$fixed, "(a)")
      expect_identical(inspection$included, "(b)")
      result <- summarize_with_margins(
        data,
        n = dplyr::n(),
        .by = a,
        .grouping = grouping_set(!!spelling),
        .margin_label = NULL
      )
      expect_identical(
        result,
        tibble::tibble(a = c("x", "y"), b = c(1, 1), n = c(1L, 1L))
      )
    }
  }
})

test_that("nesting fixes keys selected by typed helper arguments", {
  data <- tibble::tibble(a = c("x", "y"), b = c(1, 1))
  selection <- rlang::expr(tidyselect::all_of(
    names(tidyselect::peek_data())[
      vapply(tidyselect::peek_data(), is.numeric, logical(1))
    ]
  ))
  identity_selection <- function(x) x

  spellings <- list(selection, rlang::expr(identity_selection(!!selection)))
  for (spelling in spellings) {
    for (verb in c("nest_with_margins", "nest_by_with_margins")) {
      result <- rlang::eval_tidy(rlang::call2(verb, data, .by = spelling))
      expect_identical(names(result), c("b", "data"))
      expect_identical(result$b, 1)
      expect_identical(result$data, list(tibble::tibble(a = c("x", "y"))))
    }
  }
})

test_that("typed helper arguments are evaluated once per selection", {
  data <- tibble::tibble(a = c("x", "y"), b = c(1, 1))
  calls <- 0L
  selection <- rlang::expr(tidyselect::all_of({
    calls <<- calls + 1L
    names(tidyselect::peek_data())[
      vapply(tidyselect::peek_data(), is.numeric, logical(1))
    ]
  }))

  inspection <- inspect_grouping(data, .by = !!selection)
  expect_identical(inspection$fixed, "(b)")
  expect_identical(calls, 1L)

  calls <- 0L
  inspection <- inspect_grouping(
    data,
    .by = a,
    .grouping = grouping_set(!!selection)
  )
  expect_identical(inspection$included, "(b)")
  expect_identical(calls, 1L)

  calls <- 0L
  result <- summarize_with_margins(data, n = dplyr::n(), .by = !!selection)
  expect_identical(result, tibble::tibble(b = 1, n = 2L))
  expect_identical(calls, 1L)

  calls <- 0L
  result <- summarize_with_margins(
    data,
    n = dplyr::n(),
    .grouping = grouping_set(!!selection),
    .margin_label = NULL
  )
  expect_identical(result, tibble::tibble(b = 1, n = 2L))
  expect_identical(calls, 1L)
})

test_that("helper arguments see types through caller functions and aliases", {
  data <- tibble::tibble(a = c("x", "y"), b = c(1, 1))
  numeric_names <- function(...) {
    names(tidyselect::peek_data())[
      vapply(tidyselect::peek_data(), is.numeric, logical(1))
    ]
  }
  data_alias <- tidyselect::peek_data
  invoke <- function(f) f()
  default_names <- function(columns = numeric_names()) columns
  recursive_names <- function(again = TRUE) {
    if (again) recursive_names(FALSE) else numeric_names()
  }
  identity_selection <- function(x) x
  selections <- list(
    rlang::expr(tidyselect::all_of(numeric_names())),
    rlang::expr(tidyselect::all_of(names(data_alias())[
      vapply(data_alias(), is.numeric, logical(1))
    ])),
    rlang::expr(tidyselect::all_of(unlist(lapply(1, numeric_names)))),
    rlang::expr(tidyselect::all_of(invoke(numeric_names))),
    rlang::expr(tidyselect::all_of(default_names())),
    rlang::expr(tidyselect::all_of(recursive_names())),
    rlang::expr(tidyselect::all_of((function() numeric_names())())),
    rlang::expr(tidyselect::all_of(identity_selection(numeric_names())))
  )
  for (selection in selections) {
    expect_identical(inspect_grouping(data, .by = !!selection)$fixed, "(b)")
    expect_identical(
      summarize_with_margins(data, n = dplyr::n(), .by = !!selection),
      tibble::tibble(b = 1, n = 2L)
    )
    expect_identical(
      inspect_grouping(
        data, .by = a, .grouping = grouping_set(!!selection)
      )$included,
      "(b)"
    )
  }
})

test_that("helper argument classification does not force caller bindings", {
  data <- tibble::tibble(a = c("x", "y"), b = c(1, 1))
  calls <- 0L
  select_numeric <- function(...) {
    calls <<- calls + 1L
    names(tidyselect::peek_data())[
      vapply(tidyselect::peek_data(), is.numeric, logical(1))
    ]
  }
  forced <- 0L
  select_delayed <- function(helper = {
    forced <<- forced + 1L
    select_numeric
  }) {
    inspect_grouping(data, .by = tidyselect::all_of(helper()))
  }
  expect_identical(select_delayed()$fixed, "(b)")
  expect_identical(calls, 1L)
  expect_identical(forced, 1L)

  calls <- 0L
  forced <- 0L
  env <- rlang::env(data = data)
  makeActiveBinding("helper", function() {
    forced <<- forced + 1L
    select_numeric
  }, env)
  result <- eval(quote(inspect_grouping(
    data, .by = tidyselect::all_of(helper())
  )), env)
  expect_identical(result$fixed, "(b)")
  expect_identical(calls, 1L)
  expect_identical(forced, 1L)

  calls <- 0L
  select_delayed_names <- function(columns = select_numeric()) {
    inspect_grouping(data, .by = tidyselect::all_of(columns))
  }
  expect_identical(select_delayed_names()$fixed, "(b)")
  expect_identical(calls, 1L)
})

test_that("unknown helper callees retain their own selection errors", {
  data <- tibble::tibble(a = c("x", "y"), b = c(1, 1))
  selections <- list(
    rlang::expr(tidyselect::all_of(absent_helper())),
    rlang::expr(tidyselect::all_of(!!rlang::call2(
      "select", .ns = "marginplyrMissing825"
    )))
  )
  for (selection in selections) {
    expect_error(
      inspect_grouping(data, .by = !!selection),
      class = "rlang_error"
    )
  }
})

test_that("a caller's names function cannot hide type-dependent selection", {
  data <- tibble::tibble(a = c("x", "y"), b = c(1, 1))
  names <- function(x) base::names(x)[vapply(x, is.numeric, logical(1))]
  selection <- rlang::expr(tidyselect::all_of(names(tidyselect::peek_data())))

  expect_identical(inspect_grouping(data, .by = !!selection)$fixed, "(b)")
  expect_identical(
    summarize_with_margins(data, n = dplyr::n(), .by = !!selection),
    tibble::tibble(b = 1, n = 2L)
  )
})
