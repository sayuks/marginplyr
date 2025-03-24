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

test_that(".keep works", {
  data <- get_data_dummy()

  actual <- nest_with_margins(
    .data = data,
    .margins = c(g2, g3),
    .without_all = year,
    .with_all = h1,
    .key = "nested",
    .keep = TRUE
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

  expected <- dplyr::mutate(
    .data = dplyr::rowwise(expected),
    nested = list({
      res <- dplyr::mutate(
        .data = nested,
        dplyr::pick(!nested)
      )
      dplyr::relocate(
        .data = res,
        year, g2, g3, h1
      )
    })
  )

  expected <- dplyr::ungroup(expected)

  expect_identical(actual, expected)
})
