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
