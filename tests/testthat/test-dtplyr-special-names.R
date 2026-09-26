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
