test_that("dtplyr expansion reads a .N dimension without losing its name", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(.N = c("b", "a"), value = 1:2)
  source <- dtplyr::lazy_dt(input)
  query <- expand_with_margins(source, .grouping = rollup(.N))
  expect_s3_class(query, "dtplyr_step")
  actual <- dplyr::collect(query)
  expected <- expand_with_margins(input, .grouping = rollup(.N))
  expect_equal(actual, tibble::as_tibble(expected))
  expect_identical(input$.N, c("b", "a"))
  expect_identical(dplyr::collect(source), input)
})

test_that("dtplyr Total share reads a .I source summary", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(value = 2)
  source <- dtplyr::lazy_dt(input)
  query <- summarize_with_margins(
    source, .I = sum(value), p = share_of_total(.I)
  )
  expect_s3_class(query, "dtplyr_step")
  expect_identical(dplyr::collect(query), tibble::tibble(.I = 2, p = 1))
  expect_identical(dplyr::collect(source), input)
})

test_that("dtplyr special dimensions support labels and orders", {
  skip_if_suggest_absent("dtplyr")
  for (column in c(".N", ".I", ".SD", ".GRP", ".NGRP")) {
    input <- tibble::tibble(key = c("b", "a"), value = 1:2)
    names(input)[1L] <- column
    grouping <- rollup(dplyr::all_of(column))
    for (label in list("Total", NULL)) {
      for (sort in c("first", "last")) {
        info <- paste(column, if (is.null(label)) "missing" else "text", sort)
        source <- dtplyr::lazy_dt(input)
        expected_summary <- summarize_with_margins(
          input, n = dplyr::n(), .grouping = grouping,
          .margin_label = label, .sort = sort
        )
        summary <- summarize_with_margins(
          source, n = dplyr::n(), .grouping = grouping,
          .margin_label = label, .sort = sort
        )
        expect_s3_class(summary, "dtplyr_step")
        expect_equal(
          dplyr::collect(summary), tibble::as_tibble(expected_summary),
          info = info
        )
        expected_expansion <- expand_with_margins(
          input, .grouping = grouping, .margin_label = label, .sort = sort
        )
        expansion <- expand_with_margins(
          source, .grouping = grouping, .margin_label = label, .sort = sort
        )
        expect_equal(
          dplyr::collect(expansion), tibble::as_tibble(expected_expansion),
          info = info
        )
        expect_identical(dplyr::collect(source), input, info = info)
      }
    }
  }
})

test_that("dtplyr summaries accept special fixed keys", {
  skip_if_suggest_absent("dtplyr")
  for (column in c(".N", ".I", ".SD", ".GRP", ".NGRP")) {
    input <- tibble::tibble(key = c("b", "a"), value = 1:2)
    names(input)[1L] <- column
    source <- dtplyr::lazy_dt(input)
    for (grouped in c(FALSE, TRUE)) {
      if (grouped) {
        local <- dplyr::group_by(input, dplyr::across(dplyr::all_of(column)))
        lazy <- dplyr::group_by(source, dplyr::across(dplyr::all_of(column)))
        if (!identical(column, ".I")) {
          ordinary <- dplyr::collect(dplyr::summarise(lazy, n = dplyr::n()))
          expect_identical(names(ordinary), c(column, "n"))
          expect_identical(ordinary[[column]], c("a", "b"))
        }
        actual <- dplyr::collect(summarize_with_margins(
          lazy, n = dplyr::n(), .grouping = grouping_set(), .sort = "last"
        ))
        expected <- summarize_with_margins(
          local, n = dplyr::n(), .grouping = grouping_set(), .sort = "last"
        )
      } else {
        actual <- dplyr::collect(summarize_with_margins(
          source, n = dplyr::n(), .by = dplyr::all_of(column),
          .grouping = grouping_set(), .sort = "last"
        ))
        expected <- summarize_with_margins(
          input, n = dplyr::n(), .by = dplyr::all_of(column),
          .grouping = grouping_set(), .sort = "last"
        )
      }
      expect_equal(actual, tibble::as_tibble(expected),
                   info = paste(column, grouped))
    }
  }
})

test_that("dtplyr special source summaries support Parent and Total shares", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), value = c(2, 3))
  for (column in c(".I", ".GRP", ".NGRP")) {
    for (sort in c("none", "first", "last")) {
      for (label in list("Total", NULL)) {
        source <- dtplyr::lazy_dt(input)
        run <- function(data, kind) {
          if (identical(kind, "total")) {
            summarize_with_margins(
              data, !!column := sum(value),
              p = share_of_total(!!rlang::sym(column)),
              .grouping = rollup(g), .sort = sort, .margin_label = label
            )
          } else {
            summarize_with_margins(
              data, !!column := sum(value),
              p = share_of_parent(!!rlang::sym(column)),
              .grouping = rollup(g), .sort = sort, .margin_label = label
            )
          }
        }
        for (kind in c("total", "parent")) {
          expected <- run(input, kind)
          query <- run(source, kind)
          actual <- dplyr::collect(query)
          if (identical(sort, "none")) {
            actual <- dplyr::arrange(actual, g)
            expected <- dplyr::arrange(expected, g)
          }
          info <- paste(column, sort, if (is.null(label)) "NA" else label)
          expect_equal(actual, tibble::as_tibble(expected), info = info)
          expect_identical(dplyr::collect(source), input)
        }
      }
    }
  }
})

test_that("dtplyr special share sources support empty input", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = character(), value = numeric())
  source <- dtplyr::lazy_dt(input)
  for (column in c(".I", ".GRP", ".NGRP")) {
    total <- function(data) {
      summarize_with_margins(
        data, !!column := sum(value),
        p = share_of_total(!!rlang::sym(column))
      )
    }
    parent <- function(data) {
      summarize_with_margins(
        data, !!column := sum(value),
        p = share_of_parent(!!rlang::sym(column)),
        .grouping = rollup(g), .margin_label = NULL
      )
    }
    for (run in list(total, parent)) {
      query <- run(source)
      expect_s3_class(query, "dtplyr_step")
      expect_equal(dplyr::collect(query), tibble::as_tibble(run(input)),
                   info = column)
    }
  }
  expect_identical(dplyr::collect(source), input)
})

test_that("dtplyr special dimensions cover empty inputs and one-set plans", {
  skip_if_suggest_absent("dtplyr")
  for (column in c(".N", ".I", ".SD", ".GRP", ".NGRP")) {
    for (size in c(0L, 1L)) {
      input <- tibble::tibble(key = rep("a", size), value = rep(2L, size))
      names(input)[1L] <- column
      source <- dtplyr::lazy_dt(input)
      grouping <- grouping_set(dplyr::all_of(column))
      expected <- summarize_with_margins(
        input, n = dplyr::n(), .grouping = grouping,
        .margin_label = NULL
      )
      query <- summarize_with_margins(
        source, n = dplyr::n(), .grouping = grouping,
        .margin_label = NULL
      )
      expect_equal(dplyr::collect(query), tibble::as_tibble(expected),
                   info = paste(column, size))
      expected_expansion <- expand_with_margins(
        input, .grouping = grouping, .margin_label = NULL
      )
      expansion <- expand_with_margins(
        source, .grouping = grouping, .margin_label = NULL
      )
      expect_equal(
        dplyr::collect(expansion), tibble::as_tibble(expected_expansion),
        info = paste(column, size)
      )
    }
  }
})

test_that("dtplyr contextual across shares read special source names", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), value = c(2, 3))
  for (column in c(".I", ".GRP", ".NGRP")) {
    run <- function(data) {
      summarize_with_margins(
        data, !!column := sum(value),
        dplyr::across(dplyr::all_of(column), share_of_total,
                      .names = "{.col}_share"),
        .grouping = rollup(g)
      )
    }
    expected <- run(input)
    source <- dtplyr::lazy_dt(input)
    query <- run(source)
    actual <- dplyr::collect(query)
    expect_equal(
      dplyr::arrange(actual, g),
      tibble::as_tibble(dplyr::arrange(expected, g)),
      info = column
    )
    expect_identical(dplyr::collect(source), input)
  }
})

test_that("dtplyr factor labels retain a special dimension name", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(
    .N = factor(c("b", "a"), levels = c("a", "b")),
    value = 1:2
  )
  source <- dtplyr::lazy_dt(input)
  expected <- summarize_with_margins(
    input, n = dplyr::n(), .grouping = rollup(.N), .sort = "last"
  )
  query <- summarize_with_margins(
    source, n = dplyr::n(), .grouping = rollup(.N), .sort = "last"
  )
  expect_equal(dplyr::collect(query), tibble::as_tibble(expected))
})

test_that("dtplyr shares retain original special dimension values", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(.N = c("a", "b"), value = c(2, 3))
  source <- dtplyr::lazy_dt(input)
  run <- function(data) {
    summarize_with_margins(
      data, total = sum(value),
      parent = share_of_parent(total), of_total = share_of_total(total),
      .grouping = rollup(.N), .sort = "last"
    )
  }
  query <- run(source)
  expect_s3_class(query, "dtplyr_step")
  actual <- dplyr::collect(query)
  expect_equal(actual, tibble::as_tibble(run(input)))
  expect_identical(actual$.N, c("a", "b", "Total"))
  expect_equal(actual$total, c(2, 3, 5))
  expect_identical(actual$of_total, c(0.4, 0.6, 1))
  expect_identical(dplyr::collect(source), input)
})

test_that("dtplyr expansion reorders a special payload with missing labels", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(.N = 1:2, g = c("a", "b"))
  source <- dtplyr::lazy_dt(input)
  run <- function(data) {
    expand_with_margins(data, .grouping = rollup(g), .margin_label = NULL)
  }
  query <- run(source)
  expect_s3_class(query, "dtplyr_step")
  expect_equal(dplyr::collect(query), tibble::as_tibble(run(input)))
  expect_identical(dplyr::collect(source), input)
})

# The public result has an order only when a Margin order was requested.
expect_dtplyr_margin_result <- function(query, expected, sort, info) {
  actual <- dplyr::collect(query)
  expected <- tibble::as_tibble(expected)
  if (identical(sort, "none")) {
    order_rows <- function(data) {
      indices <- do.call(order, c(unname(as.list(data)), list(na.last = TRUE)))
      tibble::as_tibble(data[indices, , drop = FALSE])
    }
    actual <- order_rows(actual)
    expected <- order_rows(expected)
  }
  expect_equal(actual, expected, info = info)
}

test_that("dtplyr special dimensions retain typed share keys", {
  skip_if_suggest_absent("dtplyr")
  specials <- c(".N", ".I", ".SD", ".GRP", ".NGRP")
  sorts <- c("none", "first", "last")
  for (column in specials) {
    for (factor_dimension in c(FALSE, TRUE)) {
      for (label in list("Total", NULL, NA_character_)) {
        for (sort in sorts) {
          size <- match(sort, sorts) - 1L
          values <- rep(c("a", "b", "a"), length.out = size)
          if (factor_dimension) {
            values <- factor(values, levels = c("a", "b"))
          }
          input <- tibble::tibble(key = values, value = rep(c(2, 3),
                                                          length.out = size))
          names(input)[1L] <- column
          source <- dtplyr::lazy_dt(input)
          grouping <- rollup(dplyr::all_of(column))
          run <- function(data) {
            summarize_with_margins(
              data, total = sum(value),
              parent = share_of_parent(total),
              of_total = share_of_total(total),
              .grouping = grouping, .margin_label = label,
              .sort = sort, .id = "set"
            )
          }
          query <- run(source)
          expect_s3_class(query, "dtplyr_step")
          info <- paste(column, factor_dimension, if (is.null(label)) {
            "NULL"
          } else if (is.na(label)) {
            "NA"
          } else {
            label
          }, sort)
          expect_dtplyr_margin_result(query, run(input), sort, info)
          expect_identical(dplyr::collect(source), input, info = info)
        }
      }
    }
  }
})

test_that("dtplyr special share dimensions partition composite rollups", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(
    fixed = c("east", "east", "west"),
    .N = c("a", "b", "a"), g = c("x", "y", "x"),
    value = c(2, 3, 4)
  )
  source <- dtplyr::lazy_dt(input)
  run <- function(data) {
    summarize_with_margins(
      data, total = sum(value),
      parent = share_of_parent(total), of_total = share_of_total(total),
      .by = fixed, .grouping = rollup(.N, g),
      .margin_label = NULL, .sort = "last", .id = "set"
    )
  }
  expect_dtplyr_margin_result(run(source), run(input), "last", "composite")
  expect_identical(dplyr::collect(source), input)

  duplicate <- function(data) {
    summarize_with_margins(
      data, total = sum(value),
      parent = share_of_parent(total), of_total = share_of_total(total),
      .by = fixed, .grouping = rollup(.N, .N),
      .duplicates = "keep", .margin_label = NA_character_, .id = "set",
      .sort = "first"
    )
  }
  expect_dtplyr_margin_result(duplicate(source), duplicate(input),
                              "first", "duplicate")
  expect_identical(dplyr::collect(source), input)
})

test_that("dtplyr expansion preserves special names in every column role", {
  skip_if_suggest_absent("dtplyr")
  specials <- c(".N", ".I", ".SD", ".GRP", ".NGRP", ".BY", ".EACHI")
  sorts <- c("none", "first", "last")
  for (column in specials) {
    for (role in c("dimension", "fixed", "payload")) {
      for (label in list("Total", NULL, NA_character_)) {
        for (sort in sorts) {
          size <- match(sort, sorts) - 1L
          input <- tibble::tibble(g = rep(c("a", "b"), length.out = size),
                                  special = seq_len(size), value = seq_len(size))
          if (identical(role, "payload")) {
            input <- input[c("special", "g", "value")]
          }
          names(input)[names(input) == "special"] <- column
          source <- dtplyr::lazy_dt(input)
          grouping <- if (identical(role, "dimension")) {
            rollup(dplyr::all_of(column))
          } else {
            rollup(g)
          }
          run <- function(data) {
            expand_with_margins(
              data,
              .by = if (identical(role, "fixed")) dplyr::all_of(column),
              .grouping = grouping, .margin_label = label,
              .sort = sort, .id = "set"
            )
          }
          info <- paste(column, role, if (is.null(label)) {
            "NULL"
          } else if (is.na(label)) {
            "NA"
          } else {
            label
          }, sort)
          query <- run(source)
          expect_s3_class(query, "dtplyr_step")
          expect_dtplyr_margin_result(query, run(input), sort, info)
          expect_identical(dplyr::collect(source), input, info = info)
        }
      }
    }
  }
})

test_that("dtplyr one-set expansion reorders special payloads without an id", {
  skip_if_suggest_absent("dtplyr")
  for (column in c(".N", ".I", ".SD", ".GRP", ".NGRP", ".BY")) {
    for (size in c(0L, 3L)) {
      input <- tibble::tibble(special = seq_len(size),
                              g = rep(c("a", "b"), length.out = size))
      names(input)[1L] <- column
      source <- dtplyr::lazy_dt(input)
      for (sort in c("none", "first", "last")) {
        run <- function(data) {
          expand_with_margins(
            data, .grouping = grouping_set(g),
            .margin_label = NA_character_, .sort = sort
          )
        }
        query <- run(source)
        expect_s3_class(query, "dtplyr_step")
        expect_dtplyr_margin_result(query, run(input), sort,
                                    paste(column, size, sort))
        expect_identical(dplyr::collect(source), input)
      }
    }
  }
})

test_that("dtplyr special share failures leave the source unchanged", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(.N = c("a", "b"), value = c(2, 3))
  source <- dtplyr::lazy_dt(input)
  query <- summarize_with_margins(
    source, total = as.Date("2026-01-01"),
    p = share_of_total(total), .grouping = rollup(.N)
  )
  expect_s3_class(query, "dtplyr_step")
  expect_error(dplyr::collect(query), class = "marginplyr_error")
  expect_identical(dplyr::collect(source), input)
})
