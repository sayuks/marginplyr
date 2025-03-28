# nolint start: line_length_linter
# We can use dbplyr::memdb_frame(data) to create an in memory lazy table using
# sqlite driver, but we use duckdb.
# Three main reasons to use duckdb instead of sqlite:
# * duckdb also supports in memory database.
# * R4DS (2e) also uses duckdb in Databases section.
#   https://r4ds.hadley.nz/databases#in-this-book
# * We use like `paste(x, collapse = "/")` in testthat,
#   but need to replace it with `stringr::str_flatten(x, collapse = "/")`
#   in the context of dbplyr in lazy table.
#   str_flatten() is not supported in sqlite, but it is supported in duckdb.
# The disadvantage of using duckdb is that the order of the rows in the result
# of aggregate functions is different each time.
# https://duckdb.org/docs/sql/aggregates.html#order-by-clause-in-aggregate-functions
# Therefore, when testing a lazy table, check whether the data are equal,
# ignoring the order of the rows.
# nolint end
test_that(".margins works", {
  data <- get_data_dummy()

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "data", data)

  data <- dplyr::tbl(con, "data")

  actual <- summarize_with_margins(
    .data = data,
    n = dplyr::n(),
    mean = mean(value, na.rm = TRUE),
    .margins = c(g1, g2, g3),
    .check_margin_name = TRUE
  )

  expected <- list(
    dplyr::summarize(
      .data = data,
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g1 = "(all)",
      g2 = "(all)",
      g3 = "(all)"
    ),
    dplyr::summarize(
      .data = data,
      .by = g1,
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g2 = "(all)",
      g3 = "(all)",
    ),
    dplyr::summarize(
      .data = data,
      .by = c(g1, g2),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g3 = "(all)",
    ),
    dplyr::summarize(
      .data = data,
      .by = c(g1, g2, g3),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
    )
  )
  expected <- Reduce(dplyr::union_all, expected)
  expected <- dplyr::relocate(.data = expected, g1, g2, g3)
  expected <- dplyr::arrange(.data = expected, g1, g2, g3)

  actual <- arrange_then_collect(actual)
  expected <- arrange_then_collect(expected)

  expect_identical(actual, expected)
})

test_that(".without_all works", {
  data <- get_data_dummy()
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "data", data)

  data <- dplyr::tbl(con, "data")

  actual <- summarize_with_margins(
    .data = data,
    n = dplyr::n(),
    mean = mean(value, na.rm = TRUE),
    .margins = c(g1, g2, g3),
    .without_all = year,
    .check_margin_name = TRUE
  )

  expected <- list(
    dplyr::summarize(
      .data = data,
      .by = year,
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g1 = "(all)",
      g2 = "(all)",
      g3 = "(all)",
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, g1),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g2 = "(all)",
      g3 = "(all)",
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, g1, g2),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g3 = "(all)",
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, g1, g2, g3),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE)
    )
  )
  expected <- Reduce(dplyr::union_all, expected)
  expected <- dplyr::relocate(.data = expected, year, g1, g2, g3)
  expected <- dplyr::arrange(.data = expected, year, g1, g2, g3)

  actual <- arrange_then_collect(actual)
  expected <- arrange_then_collect(expected)

  expect_identical(actual, expected)
})

test_that(".with_all works", {
  data <- get_data_dummy()

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "data", data)

  data <- dplyr::tbl(con, "data")

  actual <- summarize_with_margins(
    .data = data,
    n = dplyr::n(),
    mean = mean(value, na.rm = TRUE),
    .margins = c(g1, g2, g3),
    .without_all = year,
    .with_all = c(h1, k1),
    .check_margin_name = TRUE
  )

  expected <- list(
    # all g1, g2, g3
    dplyr::summarize(
      .data = data,
      .by = year,
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g1 = "(all)",
      g2 = "(all)",
      g3 = "(all)",
      h1 = "(all)",
      k1 = "(all)"
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, k1),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g1 = "(all)",
      g2 = "(all)",
      g3 = "(all)",
      h1 = "(all)"
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, h1),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g1 = "(all)",
      g2 = "(all)",
      g3 = "(all)",
      k1 = "(all)"
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, h1, k1),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g1 = "(all)",
      g2 = "(all)",
      g3 = "(all)"
    ),
    # by g1, all g2, g3
    dplyr::summarize(
      .data = data,
      .by = c(year, g1),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g2 = "(all)",
      g3 = "(all)",
      h1 = "(all)",
      k1 = "(all)"
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, g1, k1),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g2 = "(all)",
      g3 = "(all)",
      h1 = "(all)"
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, g1, h1),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g2 = "(all)",
      g3 = "(all)",
      k1 = "(all)"
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, g1, h1, k1),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g2 = "(all)",
      g3 = "(all)"
    ),
    # by g1, g2, all g3
    dplyr::summarize(
      .data = data,
      .by = c(year, g1, g2),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g3 = "(all)",
      h1 = "(all)",
      k1 = "(all)"
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, g1, g2, k1),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g3 = "(all)",
      h1 = "(all)"
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, g1, g2, h1),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g3 = "(all)",
      k1 = "(all)"
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, g1, g2, h1, k1),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g3 = "(all)"
    ),
    # by g1, g2, g3
    dplyr::summarize(
      .data = data,
      .by = c(year, g1, g2, g3),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      h1 = "(all)",
      k1 = "(all)"
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, g1, g2, g3, k1),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      h1 = "(all)"
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, g1, g2, g3, h1),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      k1 = "(all)"
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, g1, g2, g3, h1, k1),
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE)
    )
  )
  expected <- Reduce(dplyr::union_all, expected)
  expected <- dplyr::relocate(.data = expected, year, g1, g2, g3, h1, k1)
  expected <- dplyr::arrange(.data = expected, year, g1, g2, g3, h1, k1)

  actual <- arrange_then_collect(actual)
  expected <- arrange_then_collect(expected)

  expect_identical(actual, expected)
})

test_that(".margin_name basic (non-NA)", {
  data <- get_data_dummy()
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "data", data)

  data <- dplyr::tbl(con, "data")

  actual <- summarize_with_margins(
    .data = data,
    n = dplyr::n(),
    mean = mean(value, na.rm = TRUE),
    .margins = g3,
    .margin_name = "total",
    .check_margin_name = TRUE
  )

  expected <- list(
    dplyr::summarize(
      .data = data,
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g3 = "total"
    ),
    dplyr::summarize(
      .data = data,
      .by = g3,
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE)
    )
  )
  expected <- Reduce(dplyr::union_all, expected)
  expected <- dplyr::relocate(.data = expected, g3)
  expected <- dplyr::arrange(.data = expected, g3)

  actual <- arrange_then_collect(actual)
  expected <- arrange_then_collect(expected)

  expect_identical(actual, expected)
})

test_that(".margin_name basic (NA)", {
  data <- get_data_dummy()
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "data", data)

  data <- dplyr::tbl(con, "data")

  actual <- summarize_with_margins(
    .data = data,
    n = dplyr::n(),
    mean = mean(value, na.rm = TRUE),
    .margins = g2,
    .margin_name = NA_character_,
    .check_margin_name = TRUE
  )

  expected <- list(
    dplyr::summarize(
      .data = data,
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      g2 = NA_character_
    ),
    dplyr::summarize(
      .data = data,
      .by = g2,
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE)
    )
  )
  expected <- Reduce(dplyr::union_all, expected)
  expected <- dplyr::relocate(.data = expected, g2)
  expected <- dplyr::arrange(.data = expected, g2)

  actual <- arrange_then_collect(actual)
  expected <- arrange_then_collect(expected)

  expect_identical(actual, expected)
})


test_that(".margin_name conflict error", {
  data <- get_data_dummy()
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "data", data)

  data <- dplyr::tbl(con, "data")

  expect_error(
    summarize_with_margins(
      .data = data,
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      .margins = g3,
      .margin_name = "RD360",
      .check_margin_name = TRUE
    ),
    "not allowed as a margin name"
  )

  expect_error(
    summarize_with_margins(
      .data = data,
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      .margins = g3,
      .margin_name = NA_character_,
      .check_margin_name = TRUE
    ),
    "not allowed as a margin name"
  )
})

test_that("Can use the margin variable before calculating", {
  # for easier debugging, extract the sample that has more than one row
  data <- dplyr::filter(get_data_dummy(), id == "108")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "data", data)

  data <- dplyr::tbl(con, "data")

  actual <- summarize_with_margins(
    .data = data,
    n = dplyr::n(),
    g3s = stringr::str_flatten(g3, collapse = "/"),
    .margins = c(g2, g3),
    .without_all = year,
    .with_all = h1,
    .check_margin_name = TRUE
  )

  expected <- list(
    # all g2, g3
    dplyr::summarize(
      .data = data,
      .by = year,
      n = dplyr::n(),
      g3s = stringr::str_flatten(g3, collapse = "/"),
      g2 = "(all)",
      g3 = "(all)",
      h1 = "(all)"
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, h1),
      n = dplyr::n(),
      g3s = stringr::str_flatten(g3, collapse = "/"),
      g2 = "(all)",
      g3 = "(all)"
    ),
    # all g3
    dplyr::summarize(
      .data = data,
      .by = c(year, g2),
      n = dplyr::n(),
      g3s = stringr::str_flatten(g3, collapse = "/"),
      g3 = "(all)",
      h1 = "(all)"
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, g2, h1),
      n = dplyr::n(),
      g3s = stringr::str_flatten(g3, collapse = "/"),
      g3 = "(all)"
    ),
    # by g2, g3
    dplyr::summarize(
      .data = data,
      .by = c(year, g2, g3),
      n = dplyr::n(),
      g3s = stringr::str_flatten(g3, collapse = "/"),
      h1 = "(all)"
    ),
    dplyr::summarize(
      .data = data,
      .by = c(year, g2, g3, h1),
      n = dplyr::n(),
      g3s = stringr::str_flatten(g3, collapse = "/"),
    )
  )
  expected <- Reduce(dplyr::union_all, expected)
  expected <- dplyr::relocate(.data = expected, year, g2, g3, h1)
  expected <- dplyr::arrange(.data = expected, year, g2, g3, h1)

  actual <- arrange_then_collect(actual)
  expected <- arrange_then_collect(expected)

  expect_identical(actual, expected)
})
