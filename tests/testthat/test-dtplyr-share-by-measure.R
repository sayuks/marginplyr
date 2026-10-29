test_that("dtplyr shares preserve aggregates over the .BY input measure", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; this test requires only the dtplyr backend.
  input <- data.table::data.table(g = c("a", "b"), .BY = c(1, 3))
  before <- data.table::copy(input)
  source <- dtplyr::lazy_dt(input)
  ordinary <- dplyr::summarise(source, value = sum(.BY), .by = g)
  expect_equal(dplyr::collect(ordinary)$value, c(1, 3))
  plain <- summarize_with_margins(
    source, value = sum(.BY), .grouping = rollup(g)
  )
  expect_equal(dplyr::collect(plain)$value, c(1, 3, 4))
  for (helper in c("share_of_parent", "share_of_total")) {
    for (style in c("direct", "across")) {
      summaries <- if (style == "direct") {
        rlang::exprs(value = sum(.BY))
      } else {
        rlang::exprs(dplyr::across(dplyr::all_of(".BY"), sum, .names = "value"))
      }
      query <- summarize_with_margins(
        source, !!!summaries,
        fraction = !!rlang::call2(helper, quote(value)),
        .grouping = rollup(g)
      )
      expect_s3_class(query, "dtplyr_step")
      expect_identical(input, before)
      actual <- dplyr::collect(query)
      expect_identical(names(actual), c("g", "value", "fraction"))
      expect_equal(actual$value, c(1, 3, 4))
      expect_identical(actual$fraction, c(0.25, 0.75, 1))
      expect_identical(input, before)
    }
  }
})

test_that("standard .BY aggregates retain source and share values", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), .BY = c(1, 3))
  expected <- list(
    sum = list(value = c(1, 3, 4), share = c(0.25, 0.75, 1)),
    mean = list(value = c(1, 3, 2), share = c(0.5, 1.5, 1)),
    min = list(value = c(1, 3, 1), share = c(1, 3, 1)),
    max = list(value = c(1, 3, 3), share = c(1 / 3, 1, 1)),
    median = list(value = c(1, 3, 2), share = c(0.5, 1.5, 1)),
    sd = list(value = c(NA_real_, NA_real_, sqrt(2)),
              share = c(NA_real_, NA_real_, 1)),
    var = list(value = c(NA_real_, NA_real_, 2),
               share = c(NA_real_, NA_real_, 1))
  )
  for (fn in names(expected)) {
    for (helper in c("share_of_parent", "share_of_total")) {
      for (style in c("direct", "across")) {
        summarize <- function(data, column) {
          summaries <- if (style == "direct") {
            rlang::exprs(value = !!rlang::call2(fn, rlang::sym(column)))
          } else {
            rlang::exprs(dplyr::across(
              dplyr::all_of(column), !!rlang::sym(fn), .names = "value"
            ))
          }
          summarize_with_margins(
            dtplyr::lazy_dt(data), !!!summaries,
            fraction = !!rlang::call2(helper, quote(value)),
            .grouping = rollup(g)
          )
        }
        actual <- dplyr::collect(summarize(input, ".BY"))
        expect_equal(actual$value, expected[[fn]]$value)
        expect_identical(actual$fraction, expected[[fn]]$share)
        renamed <- dplyr::rename(input, measure = dplyr::all_of(".BY"))
        expect_equal(actual, dplyr::collect(summarize(renamed, "measure")))
      }
    }
  }
})

test_that(".BY source validation stays lazy and preserves caller tables", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; this test requires only the dtplyr backend.
  input <- data.table::data.table(g = c("a", "b"), .BY = c(1, 3))
  before <- data.table::copy(input)
  observed <- new.env(parent = emptyenv())
  observed$rows <- 0L
  observed$summaries <- 0L
  predicate <- function(x) {
    observed$rows <- observed$rows + length(x)
    x > 0
  }
  probe <- function() {
    observed$summaries <- observed$summaries + 1L
    0
  }
  source <- dplyr::filter(dtplyr::lazy_dt(input), predicate(.BY))
  for (helper in c("share_of_parent", "share_of_total")) {
    for (style in c("direct", "across")) {
      dots <- if (style == "direct") {
        rlang::exprs(value = sum(.BY) + probe())
      } else {
        rlang::exprs(dplyr::across(
          dplyr::all_of(".BY"), ~ sum(.x) + probe(), .names = "value"
        ))
      }
      observed$rows <- 0L
      observed$summaries <- 0L
      query <- summarize_with_margins(
        source, !!!dots, fraction = !!rlang::call2(helper, quote(value)),
        .grouping = rollup(g)
      )
      expect_s3_class(query, "dtplyr_step")
      expect_identical(observed$rows, 0L)
      expect_identical(observed$summaries, 0L)
      actual <- dplyr::collect(query)
      expect_equal(actual$value, c(1, 3, 4))
      expect_identical(actual$fraction, c(0.25, 0.75, 1))
      expect_gt(observed$rows, 0L)
      expect_gt(observed$summaries, 0L)
      expect_identical(input, before)
    }
  }
})

test_that(".BY share sources still refuse non-scalars and ineligible types", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), .BY = c(1, 3))
  cases <- list(
    list(direct = quote(c(.BY, .BY)), across = quote(~ c(.x, .x)),
         message = "exactly one value"),
    list(direct = quote(numeric()), across = quote(~ numeric()),
         message = "exactly one value"),
    list(direct = quote(as.logical(sum(.BY))),
         across = quote(~ as.logical(sum(.x))),
         message = "logical"),
    list(direct = quote(as.character(sum(.BY))),
         across = quote(~ as.character(sum(.x))),
         message = "character")
  )
  for (helper in c("share_of_parent", "share_of_total")) {
    for (style in c("direct", "across")) {
      for (case in cases) {
        dots <- if (style == "direct") {
          rlang::exprs(value = !!case$direct)
        } else {
          rlang::exprs(dplyr::across(
            dplyr::all_of(".BY"), !!case$across, .names = "value"
          ))
        }
        query <- summarize_with_margins(
          dtplyr::lazy_dt(input), !!!dots,
          fraction = !!rlang::call2(helper, quote(value)),
          .grouping = rollup(g),
          .check_share_source = FALSE
        )
        expect_s3_class(query, "dtplyr_step")
        error <- expect_error(dplyr::collect(query), case$message,
                              class = "marginplyr_error")
        expect_match(conditionMessage(error), "value")
        expect_match(conditionMessage(error), "fraction")
      }
    }
  }
})

test_that(".BY aliases preserve field lookups and captured caller language", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), .BY = c(1, 3))
  outside <- list(.BY = 10)
  query <- summarize_with_margins(
    dtplyr::lazy_dt(input),
    value = sum(.data[[".BY"]]) + outside$.BY + length(quote(.BY)),
    fraction = share_of_total(value), .grouping = rollup(g)
  )
  actual <- dplyr::collect(query)
  expect_equal(actual$value, c(12, 14, 15))
  expect_equal(actual$fraction, c(12 / 15, 14 / 15, 1))
})

test_that(".BY across validation retains column and function mappings", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), .BY = c(1, 3), x = c(2, 6))
  for (helper in c("share_of_parent", "share_of_total")) {
    query <- summarize_with_margins(
      dtplyr::lazy_dt(input),
      dplyr::across(dplyr::all_of(c(".BY", "x")), list(sum = sum, max = max)),
      dplyr::across(
        dplyr::all_of(c(".BY_sum", "x_sum", ".BY_max", "x_max")),
        !!rlang::sym(helper), .names = "{.col}_share"
      ),
      .grouping = rollup(g)
    )
    actual <- dplyr::collect(query)
    expect_equal(actual[[".BY_sum"]], c(1, 3, 4))
    expect_equal(actual$x_sum, c(2, 6, 8))
    expect_identical(actual[[".BY_sum_share"]], c(0.25, 0.75, 1))
    expect_identical(actual$x_sum_share, c(0.25, 0.75, 1))
    expect_identical(actual[[".BY_max_share"]], c(1 / 3, 1, 1))
    expect_identical(actual$x_max_share, c(1 / 3, 1, 1))
  }
})

test_that("caller functions keep their meaning during .BY aliasing", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), .BY = c(1, 3), x = c(2, 6))
  dtplyr_share_input_name <- function(x) sum(x)
  for (helper in c("share_of_parent", "share_of_total")) {
    query <- summarize_with_margins(
      dtplyr::lazy_dt(input), value = sum(x),
      independent = dtplyr_share_input_name(x),
      fraction = !!rlang::call2(helper, quote(value)), .grouping = rollup(g)
    )
    actual <- dplyr::collect(query)
    expect_equal(actual$value, c(2, 6, 8))
    expect_identical(actual$independent, c(2, 6, 8))
    expect_identical(actual$fraction, c(0.25, 0.75, 1))
  }
})
