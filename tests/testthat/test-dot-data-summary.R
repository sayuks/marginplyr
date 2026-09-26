test_that("dynamic .data summary names survive portable evaluation", {
  data <- tibble::tibble(g = "a", v = 1L)
  name <- ".data"
  expected <- tibble::tibble(g = c("a", "Total"), .data = c(1L, 1L))
  ordinary <- dplyr::summarize(data, "{name}" := dplyr::n(), .by = g)
  expect_identical(ordinary[[".data"]], 1L)

  dynamic <- summarize_with_margins(
    data, "{name}" := dplyr::n(), .grouping = rollup(g)
  )
  expect_identical(dynamic, expected)

  dots <- rlang::quos(dplyr::n())
  names(dots) <- name
  spliced <- summarize_with_margins(
    data, !!!dots, .grouping = rollup(g)
  )
  expect_identical(spliced, expected)
})

test_that(".data summary names survive lazy portable evaluation", {
  data <- tibble::tibble(g = "a", v = 1L)
  expected <- data.frame(g = c("a", "Total"), .data = c(1L, 1L),
                         check.names = FALSE)
  name <- ".data"

  if (suggest_available("dtplyr")) {
    source <- dtplyr::lazy_dt(data)
    expect_identical(
      dplyr::collect(dplyr::summarize(source, "{name}" := dplyr::n(),
                                      .by = g))[[".data"]],
      1L
    )
    query <- summarize_with_margins(
      source, "{name}" := dplyr::n(), .grouping = rollup(g)
    )
    expect_s3_class(query, "dtplyr_step")
    expect_identical(as.data.frame(dplyr::collect(query)), expected)

    dots <- rlang::quos(dplyr::n())
    names(dots) <- name
    spliced <- summarize_with_margins(
      source, !!!dots, .grouping = rollup(g)
    )
    expect_identical(as.data.frame(dplyr::collect(spliced)), expected)
  }

  if (suggest_available("RSQLite") && suggest_available("DBI")) {
    sqlite_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(sqlite_con), add = TRUE)
    source <- dplyr::copy_to(sqlite_con, data, temporary = TRUE)
    query <- summarize_with_margins(
      source, "{name}" := dplyr::n(), .grouping = rollup(g)
    )
    expect_s3_class(query, "tbl_lazy")
    expect_identical(as.data.frame(dplyr::collect(query)), expected)
  }

  if (suggest_available("duckdb") && suggest_available("DBI")) {
    duck_con <- duckdb_test_connection()
    on.exit(DBI::dbDisconnect(duck_con, shutdown = TRUE), add = TRUE)
    source <- dplyr::copy_to(duck_con, data, temporary = TRUE)
    query <- summarize_with_margins(
      source, "{name}" := dplyr::n(), .grouping = rollup(g),
      .duplicates = "keep", .id = "set"
    )
    expect_s3_class(query, "tbl_lazy")
    result <- as.data.frame(dplyr::collect(query))
    expect_identical(names(result), c("g", "set", ".data"))
    expect_equal(result[[".data"]], c(1, 1))
  }
})

test_that(".data remains a valid contextual share output", {
  data <- tibble::tibble(g = "a", v = 1L)
  name <- ".data"
  result <- summarize_with_margins(
    data, total = sum(v), "{name}" := share_of_total(total),
    .grouping = rollup(g)
  )
  expect_identical(names(result), c("g", "total", ".data"))
  expect_equal(result[[".data"]], c(1, 1))

  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(con, data, temporary = TRUE)
  query <- summarize_with_margins(
    source, total = sum(v), "{name}" := share_of_total(total),
    .grouping = rollup(g)
  )
  expect_s3_class(query, "tbl_lazy")
  collected <- dplyr::collect(query)
  expect_identical(names(collected), c("g", "total", ".data"))
  expect_equal(collected[[".data"]], c(1, 1))
})
