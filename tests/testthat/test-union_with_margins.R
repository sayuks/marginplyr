test_that("union_all_with_margins() works with local data and lazy tables", {
  run_test <- function(lazy) {
    data <- get_data_dummy()

    if (lazy) {
      con <- DBI::dbConnect(duckdb::duckdb())
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

      DBI::dbWriteTable(con, "data", data)

      data <- dplyr::tbl(con, "data")
    }

    actual <- union_all_with_margins(
      data,
      .margins = c(g2, g3),
      .without_all = year,
      .with_all = h1
    )

    expected <- list(
      # all g2, g3
        dplyr::mutate(
          .data = data,
          g2 = "(all)",
          g3 = "(all)",
          h1 = "(all)"
        ),
        dplyr::mutate(
          .data = data,
          g2 = "(all)",
          g3 = "(all)"
        ),
      # all g3
        dplyr::mutate(
          .data = data,
          g3 = "(all)",
          h1 = "(all)"
        ),
        dplyr::mutate(
          .data = data,
          g3 = "(all)"
        ),
      # by g2, g3
        dplyr::mutate(
          .data = data,
          h1 = "(all)"
        ),
      data
    )
    expected <- Reduce(dplyr::union_all, expected)
    expected <- dplyr::relocate(.data = expected, year, g2, g3, h1)

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
