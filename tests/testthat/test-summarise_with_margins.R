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
test_that(".margin works correctly with local data frame and lazy table", {
  run_test <- function(lazy) {
    data <- get_data_dummy()

    if (lazy) {
      con <- DBI::dbConnect(duckdb::duckdb())
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

      DBI::dbWriteTable(con, "data", data)

      data <- dplyr::tbl(con, "data")
    }

    actual <- summarise_with_margins(
      data,
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      .margins = c(g1, g2, g3)
    )

    expected <- list(
      data %>%
        dplyr::summarise(
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g1 = "(all)",
          g2 = "(all)",
          g3 = "(all)"
        ),
      data %>%
        dplyr::summarise(
          .by = g1,
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g2 = "(all)",
          g3 = "(all)",
        ),
      data %>%
        dplyr::summarise(
          .by = c(g1, g2),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g3 = "(all)",
        ),
      data %>%
        dplyr::summarise(
          .by = c(g1, g2, g3),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
        )
    ) %>%
      Reduce(dplyr::union_all, .) %>%
      dplyr::relocate(g1, g2, g3) %>%
      dplyr::arrange(g1, g2, g3)

    if (lazy) {
      # If lazy = TRUE, test the original actual and expected are equal ignoring row order
      actual <- arrange_then_collect(actual)
      expected <- arrange_then_collect(expected)
    } else {

    }

    expect_identical(actual, expected)
  }

  purrr::walk(
    c(FALSE, TRUE),
    run_test
  )
})

test_that(".without_all works correctly with local data frame and lazy table", {
  run_test <- function(lazy) {
    data <- get_data_dummy()

    if (lazy) {
      con <- DBI::dbConnect(duckdb::duckdb())
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

      DBI::dbWriteTable(con, "data", data)

      data <- dplyr::tbl(con, "data")
    }

    actual <- summarise_with_margins(
      data,
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      .margins = c(g1, g2, g3),
      .without_all = year
    )

    expected <- list(
      data %>%
        dplyr::summarise(
          .by = year,
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g1 = "(all)",
          g2 = "(all)",
          g3 = "(all)",
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, g1),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g2 = "(all)",
          g3 = "(all)",
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, g1, g2),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g3 = "(all)",
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, g1, g2, g3),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE)
        )
    ) %>%
      Reduce(dplyr::union_all, .) %>%
      dplyr::relocate(year, g1, g2, g3) %>%
      dplyr::arrange(year, g1, g2, g3)

    if (!lazy) {
      expected <- dplyr::arrange(expected, year)
    }

    if (lazy) {
      actual <- arrange_then_collect(actual)
      expected <- arrange_then_collect(expected)
    }

    expect_identical(actual, expected)
  }

  purrr::walk(
    c(FALSE, TRUE),
    run_test
  )
})

test_that(".with_all works correctly with local data frame and lazy table", {
  run_test <- function(lazy) {
    data <- get_data_dummy()

    if (lazy) {
      con <- DBI::dbConnect(duckdb::duckdb())
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

      DBI::dbWriteTable(con, "data", data)

      data <- dplyr::tbl(con, "data")
    }

    actual <- summarise_with_margins(
      data,
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      .margins = c(g1, g2, g3),
      .without_all = year,
      .with_all = c(h1, k1)
    )

    expected <- list(
      # all g1, g2, g3
      data %>%
        dplyr::summarise(
          .by = year,
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g1 = "(all)",
          g2 = "(all)",
          g3 = "(all)",
          h1 = "(all)",
          k1 = "(all)"
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, k1),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g1 = "(all)",
          g2 = "(all)",
          g3 = "(all)",
          h1 = "(all)"
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, h1),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g1 = "(all)",
          g2 = "(all)",
          g3 = "(all)",
          k1 = "(all)"
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, h1, k1),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g1 = "(all)",
          g2 = "(all)",
          g3 = "(all)"
        ),
      # by g1, all g2, g3
      data %>%
        dplyr::summarise(
          .by = c(year, g1),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g2 = "(all)",
          g3 = "(all)",
          h1 = "(all)",
          k1 = "(all)"
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, g1, k1),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g2 = "(all)",
          g3 = "(all)",
          h1 = "(all)"
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, g1, h1),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g2 = "(all)",
          g3 = "(all)",
          k1 = "(all)"
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, g1, h1, k1),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g2 = "(all)",
          g3 = "(all)"
        ),
      # by g1, g2, all g3
      data %>%
        dplyr::summarise(
          .by = c(year, g1, g2),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g3 = "(all)",
          h1 = "(all)",
          k1 = "(all)"
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, g1, g2, k1),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g3 = "(all)",
          h1 = "(all)"
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, g1, g2, h1),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g3 = "(all)",
          k1 = "(all)"
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, g1, g2, h1, k1),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          g3 = "(all)"
        ),
      # by g1, g2, g3
      data %>%
        dplyr::summarise(
          .by = c(year, g1, g2, g3),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          h1 = "(all)",
          k1 = "(all)"
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, g1, g2, g3, k1),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          h1 = "(all)"
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, g1, g2, g3, h1),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          k1 = "(all)"
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, g1, g2, g3, h1, k1),
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE)
        )
    ) %>%
      Reduce(dplyr::union_all, .) %>%
      dplyr::relocate(year, g1, g2, g3, h1, k1) %>%
      dplyr::arrange(year, g1, g2, g3, h1, k1)

    if (!lazy) {
      expected <- dplyr::arrange(expected, year)
    }

    if (lazy) {
      actual <- arrange_then_collect(actual)
      expected <- arrange_then_collect(expected)
    }

    expect_identical(actual, expected)
  }

  purrr::walk(
    c(FALSE, TRUE),
    run_test
  )
})

test_that(".margin_name works correctly with local data frame and lazy table", {
  run_test <- function(lazy, margin_var, margine_name) {
    data <- get_data_dummy()

    if (lazy) {
      con <- DBI::dbConnect(duckdb::duckdb())
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

      DBI::dbWriteTable(con, "data", data)

      data <- dplyr::tbl(con, "data")
    }

    actual <- summarise_with_margins(
      data,
      n = dplyr::n(),
      mean = mean(value, na.rm = TRUE),
      .margins = {{ margin_var }},
      .margin_name = margine_name
    )

    expected <- list(
      data %>%
        dplyr::summarise(
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE),
          "{{ margin_var }}" := margine_name
        ),
      data %>%
        dplyr::summarise(
          .by = {{ margin_var }},
          n = dplyr::n(),
          mean = mean(value, na.rm = TRUE)
        )
    ) %>%
      Reduce(dplyr::union_all, .) %>%
      dplyr::relocate({{ margin_var }}) %>%
      dplyr::arrange(dplyr::pick({{ margin_var }}))

    if (lazy) {
      actual <- arrange_then_collect(actual)
      expected <- arrange_then_collect(expected)
    }

    expect_identical(actual, expected)
  }

  # run tests for lazy and not lazy data at once
  run_test_both <- function(margin_var, margine_name) {
    purrr::walk(
      c(FALSE, TRUE),
      function(lazy) run_test(lazy, {{ margin_var }}, margine_name)
    )
  }

  # basic case
  run_test_both(g3, "total")

  # since "RD360" is used in g3, it can not be used as margin name
  expect_error(
    run_test_both(g3, "RD360"),
    "not allowed as a margin name"
  )

  # since NA_character_ is used in g3, it can not be used as margin name
  expect_error(
    run_test_both(g3, NA_character_),
    "not allowed as a margin name"
  )

  # g2 has no NA_character_, so no error
  run_test_both(g2, NA_character_)
})

test_that("Can calculate something using the margin variable before calculating the margin with local data frame and lazy table", {
  run_test <- function(lazy, ...) {
    data <- get_data_dummy() %>%
      # for easier debugging, extract the sample that has more than one row
      dplyr::filter(id == "108")

    if (lazy) {
      con <- DBI::dbConnect(duckdb::duckdb())
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

      DBI::dbWriteTable(con, "data", data)

      data <- dplyr::tbl(con, "data")
    }

    actual <- summarise_with_margins(
      data,
      ...,
      .margins = c(g2, g3),
      .without_all = year,
      .with_all = h1
    )

    expected <- list(
      # all g2, g3
      data %>%
        dplyr::summarise(
          .by = year,
          ...,
          g2 = "(all)",
          g3 = "(all)",
          h1 = "(all)"
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, h1),
          ...,
          g2 = "(all)",
          g3 = "(all)"
        ),
      # all g3
      data %>%
        dplyr::summarise(
          .by = c(year, g2),
          ...,
          g3 = "(all)",
          h1 = "(all)"
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, g2, h1),
          ...,
          g3 = "(all)"
        ),
      # by g2, g3
      data %>%
        dplyr::summarise(
          .by = c(year, g2, g3),
          ...,
          h1 = "(all)"
        ),
      data %>%
        dplyr::summarise(
          .by = c(year, g2, g3, h1),
          ...
        )
    ) %>%
      Reduce(dplyr::union_all, .) %>%
      dplyr::relocate(year, g2, g3, h1) %>%
      dplyr::arrange(year, g2, g3, h1)

    if (!lazy) {
      expected <- dplyr::arrange(expected, year)
    }

    if (lazy) {
      actual <- arrange_then_collect(actual)
      expected <- arrange_then_collect(expected)
    }

    expect_identical(actual, expected)
  }

  purrr::walk(
    c(FALSE, TRUE),
    function(lazy) run_test(lazy, n = dplyr::n(), g3s = stringr::str_flatten(g3, collapse = "/"))
  )
})
