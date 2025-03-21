test_that("get_all_subsets() works correctly", {
  x <- c("A", "B", "C")

  expect_identical(
    get_all_subsets(x),
    list(
      character(0),
      "A",
      "B",
      "C",
      c("A", "B"),
      c("A", "C"),
      c("B", "C"),
      c("A", "B", "C")
    )
  )

  expect_identical(
    get_all_subsets(x, rev = TRUE),
    list(
      character(0),
      "C",
      "B",
      "A",
      c("B", "C"),
      c("A", "C"),
      c("A", "B"),
      c("A", "B", "C")
    )
  )
})

test_that("get_hierarchy() works correctly", {
  x <- c("A", "B", "C")

  expect_identical(
    get_hierarchy(x),
    list(
      character(0),
      "A",
      c("A", "B"),
      c("A", "B", "C")
    )
  )
})

test_that("assert_column_intersect() works correctly", {
  l <- list(
    g1 = "A",
    g2 = c("A", "B"),
    g3 = c("A", "B", "D")
  )

  expect_error(
    assert_column_intersect(l),
    "common columns"
  )
})

test_that("assert_margin_name() works correctly with local and lazy data", {
  run_test <- function(lazy) {
    d <- data.frame(
      x = c(NA_character_, "a"),
      y = c("b", NA_character_),
      z = c("a", "b")
    )

    if (lazy) {
      con <- DBI::dbConnect(duckdb::duckdb())
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

      DBI::dbWriteTable(con, "d", d)

      d <- dplyr::tbl(con, "d")
    }

    # basic success
    expect_no_error(
      assert_margin_name(d, "all")
    )

    # basic error
    expect_error(
      assert_margin_name(d, "a"),
      "not allowed as a margin name"
    )

    # NA_character_ can be used as margin_name
    expect_no_error(
      assert_margin_name(dplyr::select(d, z), NA_character_)
    )

    # NA_character_ is also error-checked
    expect_error(
      assert_margin_name(d, NA_character_),
      "not allowed as a margin name"
    )
  }

  purrr::walk(
    c(FALSE, TRUE),
    run_test
  )
})

test_that("with_margins() can also not create margins", {
  run_test <- function(lazy) {
    data <- get_data_dummy()

    if (lazy) {
      con <- DBI::dbConnect(duckdb::duckdb())
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

      DBI::dbWriteTable(con, "data", data)

      data <- dplyr::tbl(con, "data")
    }

    # return as is
    .f <- function(.data, ..., .margin_pairs, .by) {
      .data
    }

    actual <- with_margins(
      .data = data,
      .margins = c(g1, g2, g3),
      .f = .f,
      .check_margin_name = TRUE,
      .sort = FALSE
    )

    expected <- list(
      data,
      data,
      data,
      data
    )
    expected <- Reduce(dplyr::union_all, expected)
    expected <- dplyr::relocate(.data = expected, g1, g2, g3)

    if (lazy) {
      actual <- dplyr::collect(actual)
      expected <-  dplyr::collect(expected)
    }

    expect_equal(actual, expected)
  }

  purrr::walk(
    c(FALSE, TRUE),
    run_test
  )
})

test_that("with_margins() can reconstruct factors as expexted in local", {
  data <- get_data_dummy(factor = TRUE)

  # just make margins
  .f <- function(.data, ..., .margin_pairs, .by) {
    dplyr::mutate(.data = .data, !!!.margin_pairs)
  }

  # Case 1: .margin_name is not a NA_character_ ----
  res1 <- with_margins(
    .data = data,
    .margins = c(g1, g2, g3),
    .without_all = year,
    .with_all = c(h1, k1),
    .f = .f,
    .margin_name = "(all)",
    .check_margin_name = TRUE,
    .sort = TRUE
  )

  # factor levels as expected (including NA in levels)
  expect_identical(
    lapply(
      dplyr::select(.data = res1, tidyselect::where(is.factor)),
      levels
    ),
    list(
      # .margin_name "(all)" comes at the beginning of level.
      # If .margin_name is not NA, it causes no error
      # even if the levels contain NA.
      g1 = c("(all)", "A", "B", "APL", "SSD", NA),
      g2 = c("(all)", "Q", "E", "C", "D", "S", "APL", "SSD"),
      # originally h1 contained level "(all)" at the end,
      # but now comes at the beginning.
      h1 = c("(all)", "JBB", "SIO", "KLS", "YZU", "YAL", "CKE"),
      k1 = c("(all)", "WQ1", "WQ2", "WQ3", "WQ4", "WQ5", "WQ6",
             "HU1", "HU2", "HU3", "CK1", "CK2", "CK3", NA)
    )
  )

  # ordered or not as expected (remains unchanged)
  expect_identical(
    lapply(
      dplyr::select(.data = res1, tidyselect::where(is.factor)),
      is.ordered
    ),
    list(
      g1 = FALSE,
      g2 = TRUE,
      h1 = FALSE,
      k1 = TRUE
    )
  )

  # Case 2: .margin_name is a NA_character_ ----
  # g1 has <NA> level but does not have values with level <NA>.
  # school_name has <NA> level and has values with level <NA>.
  # Both are detected in error.
  expect_error(
    with_margins(
      .data = data,
      .margins = g1,
      .with_all = k1,
      .f = .f,
      .margin_name = NA_character_,
      .check_margin_name = TRUE,
      .sort = TRUE
    ),
    paste(
      "If `\\.margin_name` is a `NA_character_`, the following",
      "factor columns specified in `\\.margins` or `\\.with_all`",
      "must not contain <NA> in the level: `g1`, `k1`"
    )
  )

  res2 <- with_margins(
    # Since NA_character_ is used for .margin_name, the rows containing NA
    # must be deleted.
    .data = tidyr::drop_na(data = data, g2, g3, h1),
    .margins = c(g2, g3),
    .without_all = year,
    .with_all = h1,
    .f = .f,
    .margin_name = NA_character_,
    .check_margin_name = TRUE,
    .sort = TRUE
  )

  # factor levels as expected
  expect_identical(
    lapply(
      dplyr::select(.data = res2, tidyselect::where(is.factor)),
      levels
    ),
    list(
      # originally, g2 and h1 did not include NA in the level.
      # If .margin_name is a NA_character_, level does not include NA.
      # This is consistent with the default base::factor().
      g2 = c("Q", "E", "C", "D", "S", "APL", "SSD"),
      h1 = c("JBB", "SIO", "KLS", "YZU", "YAL", "CKE", "(all)"),
      # g1, k1 was not used with_margins().
      # These factors remain as they are.
      g1 = c("A", "B", "APL", "SSD", NA),
      k1 = c(
        "WQ1", "WQ2", "WQ3", "WQ4", "WQ5", "WQ6", "HU1", "HU2", "HU3", "CK1",
        "CK2", "CK3", NA
      )
    )
  )

  # ordered or not as expected (remains unchanged)
  expect_identical(
    lapply(
      dplyr::select(.data = res2, tidyselect::where(is.factor)),
      is.ordered
    ),
    list(
      g2 = TRUE,
      h1 = FALSE,
      g1 = FALSE,
      k1 = TRUE
    )
  )
})


test_that("row order is as expected when factor is specified", {
  # row order is as expected when factor is specified in`.with_all` and
  # `.margins` in local data frame
  x <- c(2, 10, 1, NA)

  data <- data.frame(
    x = factor(x, levels = as.character(sort(x)))
  )

  # just make margins
  f <- function(.data, ..., .margin_pairs, .by) {
    dplyr::mutate(.data = .data, !!!.margin_pairs)
  }

  # test .sort = TRUE (default)
  # sorted by x using factor levels
  actual <- with_margins(
    .data = data,
    .with_all = x,
    .f = f,
    .check_margin_name = TRUE,
    .sort = TRUE
  )

  expected <- data.frame(
    x = factor(
      c("(all)", "(all)", "(all)", "(all)", "1", "2", "10", NA),
      levels = c("(all)", "1", "2", "10")
    )
  )

  expect_identical(actual, expected)

  # If only one is specified in `.margins`, it is the same as the result of
  # `.with_all`.
  actual <- with_margins(
    .data = data,
    .margins = x,
    .f = f,
    .check_margin_name = TRUE,
    .sort = TRUE
  )

  expect_identical(actual, expected)

  # test .sort = FALSE
  # With the exception of the leading (all),
  # the row order of the inputs remains the same.
  actual <- with_margins(
    .data = data,
    .with_all = x,
    .f = f,
    .check_margin_name = TRUE,
    .sort = FALSE
  )

  expected <- data.frame(
    x = factor(
      c("(all)", "(all)", "(all)", "(all)", "2", "10", "1", NA),
      levels = c("(all)", "1", "2", "10")
    )
  )

  expect_identical(actual, expected)
})
