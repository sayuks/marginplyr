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

test_that(".data summary names survive dtplyr portable evaluation", {
  skip_if_suggest_absent("dtplyr")
  data <- tibble::tibble(g = "a", v = 1L)
  expected <- data.frame(g = c("a", "Total"), .data = c(1L, 1L),
                         check.names = FALSE)
  name <- ".data"
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
})

test_that(".data summary names survive SQLite portable evaluation", {
  skip_if_suggest_absent("RSQLite", "DBI")
  data <- tibble::tibble(g = "a", v = 1L)
  expected <- data.frame(g = c("a", "Total"), .data = c(1L, 1L),
                         check.names = FALSE)
  name <- ".data"
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(con, data, temporary = TRUE)
  query <- summarize_with_margins(
    source, "{name}" := dplyr::n(), .grouping = rollup(g)
  )
  expect_s3_class(query, "tbl_lazy")
  expect_identical(as.data.frame(dplyr::collect(query)), expected)
})

test_that(".data summary names survive DuckDB native and portable paths", {
  skip_if_suggest_absent("duckdb", "DBI")
  data <- tibble::tibble(g = "a", v = 1L)
  name <- ".data"
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(con, data, temporary = TRUE)
  ordinary <- dplyr::collect(dplyr::summarize(
    source, "{name}" := dplyr::n(), .by = g
  ))
  expect_identical(names(ordinary), c("g", ".data"))
  expect_equal(ordinary[[".data"]], 1)

  native <- summarize_with_margins(
    source, "{name}" := dplyr::n(), .grouping = rollup(g)
  )
  expect_s3_class(native, "tbl_lazy")
  expect_match(dbplyr::sql_render(native), "GROUPING SETS")
  native_result <- as.data.frame(dplyr::collect(native))
  expect_identical(names(native_result), c("g", ".data"))
  expect_equal(native_result[[".data"]], c(1, 1))

  query <- summarize_with_margins(
    source, "{name}" := dplyr::n(), .grouping = rollup(g),
    .duplicates = "keep", .id = "set"
  )
  expect_s3_class(query, "tbl_lazy")
  result <- as.data.frame(dplyr::collect(query))
  expect_identical(names(result), c("g", "set", ".data"))
  expect_equal(result[[".data"]], c(1, 1))
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
