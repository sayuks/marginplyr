test_that("nest_with_margins() works correctly with local data", {
  data <- get_data_dummy()

  actual <- nest_with_margins(
    .data = data,
    .margins = c(g2, g3),
    .without_all = year,
    .with_all = h1,
    .key = "nested"
  )

  expected <- union_all_with_margins(
    .data = data,
    .margins = c(g2, g3),
    .without_all = year,
    .with_all = h1
  )

  expected <- tidyr::nest(
    .data = expected,
    .by = c(year, g2, g3, h1),
    .key = "nested"
  )

  expected <- dplyr::arrange(.data = expected, year, g2, g3, h1)

  expect_identical(actual, expected)
})
