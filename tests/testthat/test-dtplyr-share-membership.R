test_that("dtplyr own-denominator shares ignore caller membership bindings", {
  skip_if_suggest_absent("dtplyr")
  # data.table is imported by dtplyr; only one optional backend is required.
  source <- data.table::data.table(g = c("b", "a", NA_character_), x = 1:3)
  before <- serialize(source, NULL)
  for (timing in c("preexisting", "late")) {
    for (raises in c(FALSE, TRUE)) {
      root <- new.env(parent = globalenv())
      root$source <- source
      calls <- 0L
      callback <- function(x, table) {
        calls <<- calls + 1L
        if (raises) stop("caller membership helper ran")
        !base::`%in%`(x, table)
      }
      if (timing == "preexisting") root$`%in%` <- callback
      input <- evalq(dtplyr::lazy_dt(source), root)
      query <- summarize_with_margins(
        input, total = sum(x), parent = share_of_parent(total),
        whole = share_of_total(total), .grouping = rollup(g), .sort = "last"
      )
      expect_s3_class(query, "dtplyr_step")
      expect_identical(calls, 0L)
      if (timing == "late") root$`%in%` <- callback
      actual <- dplyr::collect(query)
      expect_identical(names(actual), c("g", "total", "parent", "whole"))
      expect_identical(actual$g, c("a", "b", NA_character_, "Total"))
      expect_identical(actual$total, c(2L, 1L, 3L, 6L))
      expect_identical(actual$parent, c(2 / 6, 1 / 6, 3 / 6, 1))
      expect_identical(actual$whole, c(2 / 6, 1 / 6, 3 / 6, 1))
      expect_identical(calls, 0L)
      expect_identical(dplyr::collect(input), tibble::as_tibble(source))
      root$`%in%` <- base::`%in%`
      expect_identical(dplyr::collect(query), actual)
      expect_identical(serialize(source, NULL), before)
    }
  }
})

test_that("dtplyr Parent denominator selection ignores caller membership", {
  skip_if_suggest_absent("dtplyr")
  source <- tibble::tibble(
    fixed = c(rep("x", 3L), rep(NA_character_, 3L)),
    g = c("a", "a", "b", "a", NA_character_, NA_character_),
    h = c("u", "v", NA_character_, "u", "u", "v"),
    x = c(2L, 4L, 6L, 1L, 3L, 0L)
  )
  run <- function(data) {
    summarize_with_margins(
      data, total = sum(x), parent = share_of_parent(total),
      whole = share_of_total(total), .by = fixed, .grouping = rollup(g, h),
      .sort = "last", .id = "set"
    )
  }
  expected <- run(source)
  expect_equal(expected$parent, c(
    2 / 6, 4 / 6, 6 / 12, 1, 6 / 12, 1,
    1, 1 / 4, 1, 0, 3 / 4, 1
  ))
  for (timing in c("preexisting", "late")) {
    for (raises in c(FALSE, TRUE)) {
      root <- new.env(parent = globalenv())
      root$source <- source
      calls <- 0L
      callback <- function(x, table) {
        calls <<- calls + 1L
        if (raises) stop("caller membership helper ran")
        !base::`%in%`(x, table)
      }
      if (timing == "preexisting") root$`%in%` <- callback
      input <- evalq(dtplyr::lazy_dt(source), root)
      query <- run(input)
      expect_identical(calls, 0L)
      if (timing == "late") root$`%in%` <- callback
      expect_identical(dplyr::collect(query), expected)
      expect_identical(calls, 0L)
    }
  }
})

test_that("dtplyr shares preserve caller membership lookup and lazy execution", {
  skip_if_suggest_absent("dtplyr")
  root <- new.env(parent = globalenv())
  root$source <- tibble::tibble(g = c("b", "a", NA_character_), x = 1:3)
  visits <- calls <- 0L
  root$observe <- function(x) {
    visits <<- visits + length(x)
    rep(TRUE, length(x))
  }
  root$`%in%` <- function(x, table) {
    calls <<- calls + 1L
    !base::`%in%`(x, table)
  }
  root$input <- evalq(
    dplyr::filter(dtplyr::lazy_dt(source), observe(x)), root
  )
  query <- evalq(summarize_with_margins(
    input, total = sum(x %in% 1L), parent = share_of_parent(total),
    whole = share_of_total(total), .grouping = rollup(g), .sort = "last"
  ), root)
  expect_s3_class(query, "dtplyr_step")
  expect_identical(visits, 0L)
  expect_identical(calls, 0L)
  actual <- dplyr::collect(query)
  expect_gt(visits, 0L)
  expect_identical(calls, 4L)
  expect_identical(actual$total, c(1L, 0L, 1L, 2L))
  expect_identical(actual$parent, c(0.5, 0, 0.5, 1))
  expect_identical(actual$whole, c(0.5, 0, 0.5, 1))

  root$`%in%` <- function(...) stop("caller membership helper ran")
  expect_error(dplyr::collect(query), "caller membership helper ran")
  own <- summarize_with_margins(
    root$input, total = sum(x), whole = share_of_total(total)
  )
  own <- dplyr::collect(own)
  expect_identical(own$total, 6L)
  expect_identical(own$whole, 1)
})

test_that("share membership retains native and portable SQL translation", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- tibble::tibble(g = c("a", "a", "b"), h = c("u", "v", "u"),
                           x = c(2L, 4L, 6L))
  input <- dplyr::copy_to(con, source, "share_membership")
  run <- function(data, portable) {
    summarize_with_margins(
      data, total = sum(x), parent = share_of_parent(total),
      whole = share_of_total(total), .grouping = rollup(g, h),
      .sort = "last", .duplicates = if (portable) "keep" else "error"
    )
  }
  expected <- run(source, FALSE)
  for (portable in c(FALSE, TRUE)) {
    query <- run(input, portable)
    expect_identical(
      grepl("GROUPING SETS", dbplyr::sql_render(query), fixed = TRUE), !portable
    )
    actual <- dplyr::collect(query)
    expect_identical(names(actual), names(expected))
    expect_equal(actual, expected)
  }
})
