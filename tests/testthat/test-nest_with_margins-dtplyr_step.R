test_that("nest_with_margins() works correctly with dtplyr_step", {
  data <- get_data_dummy()

  data_dt <- dtplyr::lazy_dt(data)

  actual <- nest_with_margins(
    .data = data_dt,
    .margins = c(g2, g3),
    .without_all = year,
    .with_all = h1,
    .key = "nested"
  )

  actual <- dplyr::collect(actual)

  # compare with data.frame input
  expected <- nest_with_margins(
    .data = data,
    .margins = c(g2, g3),
    .without_all = year,
    .with_all = h1,
    .key = "nested"
  )

  expected$nested <- lapply(
    expected$nested,
    data.table::as.data.table
  )

  expect_identical(actual, expected)
})

test_that(".keep works", {
  data <- get_data_dummy()

  data_dt <- dtplyr::lazy_dt(data)

  actual <- nest_with_margins(
    .data = data_dt,
    .margins = c(g2, g3),
    .without_all = year,
    .with_all = h1,
    .key = "nested",
    .keep = TRUE
  )

  actual <- dplyr::collect(actual)

  # compare with data.frame input
  expected <- nest_with_margins(
    .data = data,
    .margins = c(g2, g3),
    .without_all = year,
    .with_all = h1,
    .key = "nested",
    .keep = TRUE
  )

  expected$nested <- lapply(
    expected$nested,
    data.table::as.data.table
  )

  expect_identical(actual, expected)
})
