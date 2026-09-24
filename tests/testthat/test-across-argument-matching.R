test_that("partial across selection and function arguments keep their values", {
  data <- tibble::tibble(g = c("a", "b"), x = 1:2, y = 3:4)

  actual <- summarize_with_margins(
    data,
    dplyr::across(.c = x, .f = sum),
    .grouping = grouping_set(g)
  )
  expected <- dplyr::summarise(
    data,
    dplyr::across(.c = x, .f = sum),
    .by = g
  )

  expect_identical(actual, expected)
  expect_identical(names(actual), c("g", "x"))
  expect_identical(actual$x, 1:2)
})

test_that("partial predicate helper arguments resolve their selection", {
  data <- tibble::tibble(g = c("a", "b"), x = 1:2, y = 3:4)

  actual <- summarize_with_margins(
    data,
    any_x = dplyr::if_any(.c = x, .f = ~ .x > 1),
    all_x = dplyr::if_all(.c = x, .f = ~ .x > 1),
    any_positional = dplyr::if_any(.f = ~ .x > 1, x),
    all_positional = dplyr::if_all(x, .f = ~ .x > 1),
    .grouping = grouping_set(g)
  )
  expected <- dplyr::summarise(
    data,
    any_x = dplyr::if_any(.c = x, .f = ~ .x > 1),
    all_x = dplyr::if_all(.c = x, .f = ~ .x > 1),
    any_positional = dplyr::if_any(.f = ~ .x > 1, x),
    all_positional = dplyr::if_all(x, .f = ~ .x > 1),
    .by = g
  )

  expect_identical(actual, expected)
  expect_identical(actual$any_x, c(FALSE, TRUE))
  expect_identical(actual$all_x, c(FALSE, TRUE))
})

test_that("across matches positional arguments around partial names", {
  data <- tibble::tibble(g = c("a", "b"), x = 1:2, y = 3:4)

  actual <- summarize_with_margins(
    data,
    dplyr::across(x, .f = sum, .names = "{.col}_first"),
    dplyr::across(.f = sum, x, .names = "{.col}_second"),
    dplyr::across(.c = x, sum, .names = "{.col}_third"),
    .grouping = grouping_set(g)
  )

  expect_identical(names(actual), c("g", "x_first", "x_second", "x_third"))
  expect_identical(actual$x_first, 1:2)
  expect_identical(actual$x_second, 1:2)
  expect_identical(actual$x_third, 1:2)
})

test_that("genuine dots reach the function and later formals need exact names", {
  data <- tibble::tibble(g = c("a", "b"), x = 1:2)

  actual <- suppressWarnings(summarize_with_margins(
    data,
    dplyr::across(.cols = x, .fns = sum, .c = 10, .names = "{.col}_plus"),
    dplyr::across(
      .c = x,
      .f = function(v, ...) v,
      .n = "renamed",
      .un = TRUE
    ),
    .grouping = grouping_set(g)
  ))
  expected <- suppressWarnings(dplyr::summarise(
    data,
    dplyr::across(.cols = x, .fns = sum, .c = 10, .names = "{.col}_plus"),
    dplyr::across(
      .c = x,
      .f = function(v, ...) v,
      .n = "renamed",
      .un = TRUE
    ),
    .by = g
  ))

  expect_identical(actual, expected)
  expect_identical(names(actual), c("g", "x_plus", "x"))
  expect_identical(actual$x_plus, c(11, 12))
  expect_identical(actual$x, 1:2)
})

test_that("a partial unpack name stays in the dots", {
  data <- tibble::tibble(g = c("a", "b"), x = 1:2)

  actual <- suppressWarnings(summarize_with_margins(
    data,
    dplyr::across(
      .c = x,
      .f = function(v, ...) tibble::tibble(double = v * 2),
      .un = TRUE
    ),
    .grouping = grouping_set(g)
  ))

  expect_identical(names(actual), c("g", "x"))
  expect_s3_class(actual$x, "data.frame")
  expect_identical(actual$x$double, c(2, 4))
})

test_that("multiply matched partial arguments remain actionable errors", {
  data <- tibble::tibble(g = "a", x = 1, y = 2)

  expect_error(
    summarize_with_margins(
      data,
      dplyr::across(.c = x, .co = y, .f = sum),
      .grouping = grouping_set(g)
    ),
    'formal argument "\\.cols" matched by multiple actual arguments'
  )
  expect_error(
    summarize_with_margins(
      data,
      any_x = dplyr::if_any(.c = x, .f = is.numeric, .fn = is.numeric),
      .grouping = grouping_set(g)
    ),
    'formal argument "\\.fns" matched by multiple actual arguments'
  )
})
