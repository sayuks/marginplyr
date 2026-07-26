test_that("get_col_names preserves tidyselect names and order", {
  data <- data.frame(
    first = 1:2,
    second = 3:4,
    third = 5:6
  )

  expect_identical(
    get_col_names(data, third, renamed = first),
    c("third", "renamed")
  )
  expect_identical(
    get_col_names(data, dplyr::everything()),
    names(data)
  )
  expect_identical(
    get_col_names(
      dplyr::group_by(data, first),
      dplyr::everything()
    ),
    names(data)
  )
})

test_that("get_col_names reads dbplyr query metadata", {
  data <- data.frame(
    first = 1:2,
    second = 3:4,
    third = 5:6
  )
  remote <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_postgres())

  expect_identical(
    get_col_names(remote, third, renamed = first),
    c("third", "renamed")
  )
})

test_that("get_col_names reads dtplyr step metadata", {
  skip_if_not_installed("dtplyr")

  data <- data.frame(
    first = 1:2,
    second = 3:4,
    third = 5:6
  )
  lazy <- dtplyr::lazy_dt(data)

  expect_identical(
    get_col_names(lazy, third, renamed = first),
    c("third", "renamed")
  )
})

test_that("get_col_names reads Arrow metadata", {
  skip_if_not_installed("arrow")

  data <- data.frame(
    first = 1:2,
    second = 3:4,
    third = 5:6
  )
  table <- arrow::Table$create(data)
  dataset <- arrow::InMemoryDataset$create(table)
  query <- dplyr::select(table, first, second, third)

  for (lazy in list(table, dataset, query)) {
    expect_identical(
      get_col_names(lazy, third, renamed = first),
      c("third", "renamed")
    )
  }
})
