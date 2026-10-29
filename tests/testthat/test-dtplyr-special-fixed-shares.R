test_that("dtplyr shares validate cardinality with dot-dot fixed keys", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(`..0` = c("P", "P"), g = c("a", "b"), x = c(1, 3))
  for (helper in c("share_of_parent", "share_of_total")) {
    query <- summarize_with_margins(
      dtplyr::lazy_dt(input), value = sum(x),
      fraction = !!rlang::call2(helper, rlang::sym("value")),
      .by = dplyr::all_of("..0"), .grouping = rollup(g), .sort = "last"
    )
    expect_s3_class(query, "dtplyr_step")
    actual <- dplyr::collect(query)
    expect_identical(actual[["..0"]], rep("P", 3L))
    expect_identical(actual$fraction, c(0.25, 0.75, 1))
  }
})

test_that("dtplyr contextual shares preserve every special fixed key", {
  skip_if_suggest_absent("dtplyr")
  for (column in c(".N", ".I", ".SD", ".GRP", ".NGRP", ".BY")) {
    input <- tibble::tibble(fixed = c("P", "P"), g = c("a", "b"), x = c(1, 3))
    names(input)[1L] <- column
    for (helper in c("share_of_parent", "share_of_total")) {
      for (style in c("direct", "across")) {
        share <- if (identical(style, "direct")) {
          rlang::exprs(fraction = !!rlang::call2(helper, rlang::sym("value")))
        } else {
          rlang::exprs(dplyr::across(
            dplyr::all_of("value"), !!rlang::sym(helper), .names = "fraction"
          ))
        }
        query <- summarize_with_margins(
          dtplyr::lazy_dt(input), value = sum(x), !!!share,
          .by = dplyr::all_of(column), .grouping = rollup(g), .sort = "last"
        )
        expect_s3_class(query, "dtplyr_step")
        actual <- dplyr::collect(query)
        info <- paste(column, helper, style)
        expect_identical(names(actual), c(column, "g", "value", "fraction"),
                         info = info)
        expect_identical(actual[[column]], rep("P", 3L), info = info)
        expect_equal(actual$value, c(1, 3, 4), info = info)
        expect_identical(actual$fraction, c(0.25, 0.75, 1), info = info)
      }
    }
  }
})

test_that("dtplyr special fixed share keys retain typed partitions", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only the dtplyr backend is required.
  input <- data.table::data.table(
    fixed = factor(c(rep("P", 4L), "NA", "NA", NA, NA)),
    day = as.Date(c(0, 0, 1, 1, NA, NA, 2, 2), origin = "2026-01-01"),
    g = c("a", "a", "a", "b", "a", "b", NA, "a"),
    h = rep(c("x", "y"), 4L), x = c(1, 3, 2, 6, 3, 3, 4, 4)
  )
  specials <- c(".N", ".I", ".SD", ".GRP", ".NGRP", ".BY")
  for (index in seq_along(specials)) {
    next_index <- if (index == length(specials)) 1L else index + 1L
    keys <- specials[c(index, next_index)]
    for (empty in c(FALSE, TRUE)) {
      rows <- if (empty) integer() else seq_len(nrow(input))
      table <- data.table::copy(input)[rows]
      data.table::setnames(table, c("fixed", "day"), keys)
      before <- data.table::copy(table)
      source <- dtplyr::lazy_dt(table, immutable = TRUE)
      for (implicit in c(FALSE, TRUE)) {
        for (label in list("Total", NULL, NA_character_)) {
          for (sort in c("none", "first", "last")) {
            run <- function(data, by) {
              if (implicit) {
                data <- dplyr::group_by(data, dplyr::across(dplyr::all_of(by)))
              }
              by_quo <- if (implicit) {
                rlang::quo(NULL)
              } else {
                rlang::quo(dplyr::all_of(by))
              }
              summarize_with_margins(
                data, value = sum(x), level = grouping_id(g, h),
                parent = share_of_parent(value), total = share_of_total(value),
                dplyr::across(value, share_of_parent, .names = "parent_across"),
                dplyr::across(value, share_of_total, .names = "total_across"),
                .by = !!by_quo,
                .grouping = rollup(g, h, h), .duplicates = "keep", .id = "set",
                .margin_label = label, .sort = sort
              )
            }
            control <- tibble::as_tibble(table)
            names(control)[1:2] <- c("fixed", "day")
            expected <- tibble::as_tibble(run(control, c("fixed", "day")))
            names(expected)[1:2] <- keys
            query <- run(source, keys)
            info <- paste(paste(keys, collapse = ","), empty, implicit,
                          deparse(label), sort, sep = "/")
            expect_s3_class(query, "dtplyr_step")
            expect_identical(table, before, info = info)
            actual <- dplyr::collect(query)
            if (identical(sort, "none")) {
              order_rows <- function(data) {
                dplyr::arrange(data, dplyr::across(dplyr::all_of(
                  c(keys, "set", "g", "h")
                )))
              }
              actual <- order_rows(actual)
              expected <- order_rows(expected)
            }
            expect_identical(names(actual), names(expected), info = info)
            expect_equal(actual, expected, info = info)
            for (key in keys) {
              expect_identical(actual[[key]], expected[[key]], info = info)
            }
            shares <- c("parent", "total", "parent_across", "total_across")
            for (share in shares) {
              expect_type(actual[[share]], "double")
            }
            expect_identical(table, before, info = info)
          }
        }
      }
    }
  }
})

test_that("dtplyr special fixed keys support Grand-total-only shares", {
  skip_if_suggest_absent("dtplyr")
  for (column in c(".N", ".I", ".SD", ".GRP", ".NGRP", ".BY")) {
    for (size in c(0L, 2L)) {
      input <- tibble::tibble(fixed = rep(NA_integer_, size), x = seq_len(size))
      names(input)[1L] <- column
      query <- summarize_with_margins(
        dtplyr::lazy_dt(input), value = sum(x),
        total = share_of_total(value),
        dplyr::across(value, share_of_total, .names = "total_across"),
        .by = dplyr::all_of(column), .id = "set"
      )
      expect_s3_class(query, "dtplyr_step")
      actual <- dplyr::collect(query)
      expected_rows <- as.integer(size > 0L)
      expect_identical(actual[[column]], rep(NA_integer_, expected_rows))
      expect_equal(actual$value, if (size > 0L) 3L else integer())
      expect_identical(actual$total, rep(1, expected_rows))
      expect_identical(actual$total_across, actual$total)
    }
  }
})

test_that("dtplyr special fixed shares defer work and preserve tables", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only the dtplyr backend is required.
  input <- data.table::data.table(
    .N = c("P", "P"), g = c("a", "b"), x = c(1, 3)
  )
  before <- data.table::copy(input)
  reads <- 0L
  summaries <- 0L
  predicate <- function(x) {
    reads <<- reads + length(x)
    x > 0
  }
  tally <- function(x) {
    summaries <<- summaries + 1L
    sum(x)
  }
  source <- dtplyr::lazy_dt(input, immutable = TRUE) |>
    dplyr::filter(predicate(x))
  query <- summarize_with_margins(
    source, value = tally(x),
    parent = share_of_parent(value), total = share_of_total(value),
    .by = .N, .grouping = rollup(g), .sort = "last"
  )
  expect_s3_class(query, "dtplyr_step")
  expect_identical(reads, 0L)
  expect_identical(summaries, 0L)
  expect_identical(input, before)
  actual <- dplyr::collect(query)
  expect_gt(reads, 0L)
  expect_gt(summaries, 0L)
  expect_identical(actual$parent, c(0.25, 0.75, 1))
  expect_identical(actual$total, actual$parent)
  expect_identical(input, before)
})

test_that("dtplyr special dimensions still support summaries and shares", {
  skip_if_suggest_absent("dtplyr")
  for (column in c(".N", ".I", ".SD", ".GRP", ".NGRP", ".BY")) {
    input <- tibble::tibble(g = c("a", "b"), x = c(1, 3))
    names(input)[1L] <- column
    run <- function(data) {
      summarize_with_margins(
        data, value = sum(x), n = dplyr::n(),
        parent = share_of_parent(value), total = share_of_total(value),
        dplyr::across(value, share_of_parent, .names = "parent_across"),
        dplyr::across(value, share_of_total, .names = "total_across"),
        .grouping = rollup(dplyr::all_of(column)), .sort = "last"
      )
    }
    actual <- dplyr::collect(run(dtplyr::lazy_dt(input)))
    expect_equal(actual, tibble::as_tibble(run(input)), info = column)
    expect_identical(actual[[column]], c("a", "b", "Total"), info = column)
    expect_equal(actual$value, c(1, 3, 4), info = column)
    expect_identical(actual$total, c(0.25, 0.75, 1), info = column)
  }
})
