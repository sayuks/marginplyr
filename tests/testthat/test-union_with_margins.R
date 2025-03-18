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
      data %>%
        dplyr::mutate(
          g2 = "(all)",
          g3 = "(all)",
          h1 = "(all)"
        ),
      data %>%
        dplyr::mutate(
          g2 = "(all)",
          g3 = "(all)"
        ),
      # all g3
      data %>%
        dplyr::mutate(
          g3 = "(all)",
          h1 = "(all)"
        ),
      data %>%
        dplyr::mutate(
          g3 = "(all)"
        ),
      # by g2, g3
      data %>%
        dplyr::mutate(
          h1 = "(all)"
        ),
      data
    ) %>%
      Reduce(dplyr::union_all, .) %>%
      dplyr::relocate(year, g2, g3, h1)


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
