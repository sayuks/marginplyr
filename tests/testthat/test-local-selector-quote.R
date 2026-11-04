test_that("local summary selectors ignore a caller's quote binding", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = 10:12)
  calls <- 0L
  quote <- function(...) {
    calls <<- calls + 1L
    base::quote(y)
  }

  expected <- dplyr::summarise(
    data,
    dplyr::across(x, sum, .names = "total_{.col}"),
    picked = sum(dplyr::pick(x)),
    any_small = any(dplyr::if_any(x, ~ .x < 5L)),
    all_small = all(dplyr::if_all(x, ~ .x < 5L)),
    .by = g
  )
  expect_identical(calls, 0L)
  actual <- summarize_with_margins(
    data,
    dplyr::across(x, sum, .names = "total_{.col}"),
    picked = sum(dplyr::pick(x)),
    any_small = any(dplyr::if_any(x, ~ .x < 5L)),
    all_small = all(dplyr::if_all(x, ~ .x < 5L)),
    .grouping = grouping_set(g)
  )
  expect_equal(actual, expected)
  expect_identical(calls, 0L)
})

test_that("local selectors keep their captured scope and evaluation order", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = 10:12)
  events <- character()
  select_columns <- function() {
    events <<- c(events, "caller selector")
    "y"
  }
  selection <- local({
    select_columns <- function() {
      events <<- c(events, "captured selector")
      "x"
    }
    rlang::quo(dplyr::all_of(select_columns()))
  })
  quote <- function(...) {
    events <<- c(events, "caller quote")
    "y"
  }

  expected <- dplyr::summarise(
    data, before = quote(ignored), picked = sum(dplyr::pick(!!selection)),
    .by = g
  )
  expected_events <- events
  expect_identical(expected_events, c(
    "caller quote", "caller quote", "captured selector"
  ))

  events <- character()
  actual <- summarize_with_margins(
    data, before = quote(ignored), picked = sum(dplyr::pick(!!selection)),
    .grouping = grouping_set(g)
  )
  expect_equal(actual, expected)
  expect_identical(events, expected_events)
})
