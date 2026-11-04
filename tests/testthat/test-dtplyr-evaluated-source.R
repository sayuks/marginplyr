test_that("dtplyr expansion copies one evaluated source per collection", {
  skip_if_suggest_absent("dtplyr")

  calls <- 0L
  keep_first <- function(row_id) {
    calls <<- calls + 1L
    row_id == 19L & calls == 1L
  }
  # dtplyr imports data.table, so the backend guard also covers this input.
  input <- data.table::data.table(g = "a", row_id = 19L)
  before <- data.table::copy(input)
  source <- dtplyr::lazy_dt(input) |>
    dplyr::filter(keep_first(row_id))

  query <- expand_with_margins(
    source, .grouping = rollup(g), .margin_label = NULL, .id = "sid"
  )
  expect_identical(calls, 0L)

  first <- dplyr::arrange(dplyr::collect(query), sid)
  expect_identical(calls, 1L)
  expect_identical(first$sid, 1:2)
  expect_identical(first$g, c("a", NA_character_))
  expect_identical(first$row_id, c(19L, 19L))

  second <- dplyr::collect(query)
  expect_identical(calls, 2L)
  expect_identical(nrow(second), 0L)
  expect_identical(input, before)
})

test_that("dtplyr expansion copies cached input across duplicate sets", {
  skip_if_suggest_absent("dtplyr")

  calls <- 0L
  pick_row <- function(row_id) {
    calls <<- calls + 1L
    row_id == calls
  }
  # dtplyr imports data.table, so the backend guard also covers this input.
  input <- data.table::data.table(g = c("a", "a"), row_id = 1:2)
  before <- data.table::copy(input)
  source <- dtplyr::lazy_dt(input) |>
    dplyr::filter(row_id > 0L) |>
    dplyr::compute(name = "cached_rows") |>
    dplyr::filter(pick_row(row_id))

  query <- expand_with_margins(
    source,
    .grouping = grouping_sets(rollup(g), grouping_set(g), grouping_set()),
    .duplicates = "keep", .margin_label = NULL, .id = "sid"
  )
  expect_identical(calls, 0L)
  result <- dplyr::arrange(dplyr::collect(query), sid)
  expect_identical(calls, 1L)
  expect_identical(result$sid, 1:4)
  expect_identical(result$g, c("a", NA_character_, "a", NA_character_))
  expect_identical(result$row_id, rep(1L, 4L))
  expect_identical(input, before)
})

test_that("dtplyr deferred Nest shares eligible rows across cells", {
  skip_if_suggest_absent("dtplyr")

  calls <- 0L
  pick_row <- function(row_id) {
    calls <<- calls + 1L
    row_id == calls
  }
  source <- dtplyr::lazy_dt(data.frame(g = c("a", "a"), row_id = 1:2)) |>
    dplyr::filter(pick_row(row_id))

  query <- nest_with_margins(source, .grouping = rollup(g), .id = "sid")
  expect_identical(calls, 0L)

  first <- dplyr::arrange(dplyr::collect(query), sid)
  expect_identical(calls, 1L)
  expect_identical(first$sid, 1:2)
  expect_identical(lapply(first$data, function(cell) cell$row_id), list(1L, 1L))

  second <- dplyr::arrange(dplyr::collect(query), sid)
  expect_identical(calls, 2L)
  expect_identical(second$sid, 1:2)
  expect_identical(lapply(second$data, function(cell) cell$row_id), list(2L, 2L))
})

test_that("dtplyr row-wise Nest shares eligible rows across cells", {
  skip_if_suggest_absent("dtplyr")

  calls <- 0L
  pick_row <- function(row_id) {
    calls <<- calls + 1L
    row_id == calls
  }
  source <- dtplyr::lazy_dt(data.frame(g = c("a", "a"), row_id = 1:2)) |>
    dplyr::filter(pick_row(row_id))

  result <- nest_by_with_margins(source, .grouping = rollup(g), .id = "sid")
  expect_s3_class(result, "rowwise_df")
  expect_identical(calls, 1L)
  expect_identical(sort(result$sid), 1:2)
  expect_identical(lapply(result$data, function(cell) cell$row_id), list(1L, 1L))
})
