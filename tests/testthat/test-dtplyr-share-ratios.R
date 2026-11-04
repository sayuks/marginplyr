test_that("dtplyr share ratios ignore late caller missingness bindings", {
  skip_if_suggest_absent("dtplyr")
  root <- new.env(parent = globalenv())
  root$source <- tibble::tibble(g = c("a", "b"), x = c(1, 3))
  input <- evalq(dtplyr::lazy_dt(source), root)
  query <- summarize_with_margins(
    input, value = sum(x), parent = share_of_parent(value),
    whole = share_of_total(value), .grouping = rollup(g), .sort = "last"
  )
  root$is.na <- function(x) rep(TRUE, length(x))
  actual <- dplyr::collect(query)
  expect_identical(actual$value, c(1, 3, 4))
  expect_identical(actual$parent, c(0.25, 0.75, 1))
  expect_identical(actual$whole, c(0.25, 0.75, 1))
})

test_that("dtplyr generated ratios ignore caller helpers across partitions", {
  skip_if_suggest_absent("dtplyr")
  # data.table is imported by dtplyr; only one optional backend is required.
  source <- data.table::data.table(
    fixed = c(rep("x", 3L), rep(NA_character_, 3L)),
    g = c("a", "a", "b", "a", NA_character_, NA_character_),
    h = c("u", "v", NA_character_, "u", "u", "v"),
    x = c(2L, 4L, 6L, 1L, 3L, 0L)
  )
  before <- serialize(source, NULL)
  run <- function(data, fixed, across) {
    ordinary <- if (across) {
      list(rlang::expr(dplyr::across(x, sum, .names = "value")))
    } else {
      list(value = rlang::expr(sum(x)))
    }
    shares <- if (across) {
      list(
        rlang::expr(dplyr::across(value, share_of_parent, .names = "parent")),
        rlang::expr(dplyr::across(value, share_of_total, .names = "whole"))
      )
    } else {
      list(parent = rlang::expr(share_of_parent(value)),
           whole = rlang::expr(share_of_total(value)))
    }
    summarize_with_margins(
      data, !!!ordinary, !!!shares, .by = !!if (fixed) rlang::sym("fixed"),
      .grouping = rollup(g, h), .sort = "last"
    )
  }
  corruptions <- list(
    is.na = function(x) rep(TRUE, length(x)),
    as.double = function(x) base::as.double(x) + 1,
    `*` = function(x, y) x * y * 10,
    `/` = function(x, y) rep(17, length(x)),
    `==` = function(x, y) rep(TRUE, max(length(x), length(y))),
    `|` = function(x, y) rep(TRUE, max(length(x), length(y))),
    `%in%` = function(x, table) rep(FALSE, length(x))
  )
  for (fixed in c(FALSE, TRUE)) {
    expected <- run(tibble::as_tibble(source), fixed, FALSE)
    for (across in c(FALSE, TRUE)) {
      for (helper in names(corruptions)) {
        for (timing in c("preexisting", "late")) {
          for (mode in c("corrupt", "raise", "count")) {
            root <- new.env(parent = globalenv())
            root$source <- source
            calls <- 0L
            callback <- function(...) {
              calls <<- calls + 1L
              if (mode == "raise") stop("caller ratio helper ran")
              fn <- if (mode == "corrupt") {
                corruptions[[helper]]
              } else {
                get(helper, baseenv())
              }
              fn(...)
            }
            if (timing == "preexisting") assign(helper, callback, root)
            input <- evalq(dtplyr::lazy_dt(source, immutable = TRUE), root)
            query <- run(input, fixed, across)
            expect_s3_class(query, "dtplyr_step")
            expect_identical(calls, 0L)
            if (timing == "late") assign(helper, callback, root)
            expect_identical(dplyr::collect(query), expected,
                             info = paste(fixed, across, helper, timing, mode))
            expect_identical(calls, 0L)
            expect_identical(dplyr::collect(input), tibble::as_tibble(source))
            expect_identical(serialize(source, NULL), before)
          }
        }
      }
    }
  }
})

test_that("dtplyr ratio isolation preserves lexical summaries and laziness", {
  skip_if_suggest_absent("dtplyr")
  root <- new.env(parent = globalenv())
  root$source <- tibble::tibble(g = c("a", "b"), x = c(1, 3))
  visits <- calls <- 0L
  root$observe <- function(x) {
    visits <<- visits + length(x)
    rep(TRUE, length(x))
  }
  root$input <- evalq(
    dplyr::filter(dtplyr::lazy_dt(source), observe(x)), root
  )
  root$is.na <- function(x) {
    calls <<- calls + 1L
    rep(TRUE, length(x))
  }
  query <- evalq(summarize_with_margins(
    input, value = sum(is.na(x)), parent = share_of_parent(value),
    whole = share_of_total(value), .grouping = rollup(g), .sort = "last"
  ), root)
  expect_identical(visits, 0L)
  expect_identical(calls, 0L)
  actual <- dplyr::collect(query)
  expect_gt(visits, 0L)
  expect_gt(calls, 0L)
  expect_identical(actual$value, c(1L, 1L, 2L))
  expect_identical(actual$parent, c(0.5, 0.5, 1))
  expect_identical(actual$whole, c(0.5, 0.5, 1))
  root$is.na <- function(...) stop("caller summary helper ran")
  expect_error(dplyr::collect(query), "caller summary helper ran")
})

test_that("isolated dtplyr ratios retain missing, zero and own denominators", {
  skip_if_suggest_absent("dtplyr")
  root <- new.env(parent = globalenv())
  root$source <- tibble::tibble(
    fixed = c("missing", "missing", NA_character_, NA_character_),
    g = c("a", "b", "a", "b"), x = c(NA_real_, 1, -1, 1)
  )
  for (helper in c("is.na", "as.double", "*", "/", "==", "|", "%in%")) {
    assign(helper, function(...) stop("caller ratio helper ran"), root)
  }
  input <- evalq(dtplyr::lazy_dt(source), root)
  query <- summarize_with_margins(
    input, value = sum(x), parent = share_of_parent(value),
    whole = share_of_total(value), .by = fixed,
    .grouping = rollup(g), .sort = "last"
  )
  actual <- dplyr::collect(query)
  expected <- c(NA_real_, NA_real_, 1, NA_real_, NA_real_, 1)
  expect_identical(actual$parent, expected)
  expect_identical(actual$whole, expected)
  own <- summarize_with_margins(
    input, value = sum(x), whole = share_of_total(value), .by = fixed
  )
  expect_identical(dplyr::collect(own)$whole, c(1, 1))
})
