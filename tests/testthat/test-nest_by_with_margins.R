test_that("nest_by_with_margins() works correctly with local data", {
  data <- get_data_dummy()

  actual <- nest_by_with_margins(
    .data = data,
    .rollup = c(g2, g3),
    .by = year,
    .cube = h1,
    .key = "nested"
  )

  expected <- union_all_with_margins(
    .data = data,
    .rollup = c(g2, g3),
    .by = year,
    .cube = h1
  )

  expected <- dplyr::nest_by(
    .data = expected,
    year, g2, g3, h1,
    .key = "nested",
    .keep = FALSE
  )

  # remove "S3 object of class <vctrs_list_of/vctrs_vctr/list>"
  class(expected$nested) <- "list"
  # remove attribute
  # original attr(expected$nested, 'ptype')` is
  # an S3 object of class <tbl_df/tbl/data.frame>
  attr(expected$nested, "ptype") <- NULL

  expected <- dplyr::arrange(.data = expected, year, g2, g3, h1)

  expect_identical(actual, expected)
})

test_that(".keep works", {
  data <- get_data_dummy()

  actual <- nest_by_with_margins(
    .data = data,
    .rollup = c(g2, g3),
    .by = year,
    .cube = h1,
    .key = "nested",
    .keep = TRUE
  )

  expected <- union_all_with_margins(
    .data = data,
    .rollup = c(g2, g3),
    .by = year,
    .cube = h1
  )

  expected <- dplyr::nest_by(
    .data = expected,
    year, g2, g3, h1,
    .key = "nested",
    .keep = TRUE
  )

  class(expected$nested) <- "list"
  attr(expected$nested, "ptype") <- NULL

  expect_identical(actual, expected)
})
