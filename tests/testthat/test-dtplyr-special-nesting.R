# Compare typed outer keys and cell row multisets as values. Serialization
# bytes can differ for equal vectors, and unordered results have no row order.
special_nesting_rows <- function(result, key = "data") {
  outer <- setdiff(names(result), key)
  lapply(seq_len(nrow(result)), function(i) {
    cell <- tibble::as_tibble(result[[key]][[i]])
    if (ncol(cell) > 0L) {
      cell <- cell[vctrs::vec_order(cell), , drop = FALSE]
    }
    list(
      outer = as.list(result[i, outer, drop = FALSE]),
      cell_names = names(cell),
      cell_rows = nrow(cell),
      cell_values = as.list(cell)
    )
  })
}

expect_special_nesting_rows <- function(actual, expected, sort, key = "data",
                                        info = NULL) {
  actual_rows <- special_nesting_rows(actual, key)
  expected_rows <- special_nesting_rows(expected, key)
  if (!identical(sort, "none")) {
    expect_identical(actual_rows, expected_rows, info = info)
    return(invisible(NULL))
  }
  remaining <- seq_along(expected_rows)
  matches <- length(actual_rows) == length(expected_rows)
  for (row in actual_rows) {
    hit <- remaining[vapply(
      expected_rows[remaining], identical, logical(1), row
    )]
    if (length(hit) == 0L) {
      matches <- FALSE
      break
    }
    remaining <- remaining[remaining != hit[[1L]]]
  }
  expect_true(matches && length(remaining) == 0L, info = info)
}

test_that("dtplyr fixed special keys nest the one-row reproduction", {
  skip_if_suggest_absent("dtplyr")
  for (column in c(".N", ".I", ".SD", ".GRP", ".NGRP")) {
    input <- tibble::tibble(special = "a", v = 1)
    names(input)[1L] <- column
    source <- dtplyr::lazy_dt(input)
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      result <- verb(source, .by = dplyr::all_of(column))
      if (identical(verb, nest_with_margins)) {
        expect_s3_class(result, "dtplyr_step")
        result <- dplyr::collect(result)
      } else {
        expect_s3_class(result, "rowwise_df")
      }
      expect_identical(names(result), c(column, "data"), info = column)
      expect_identical(result[[column]], "a", info = column)
      expect_identical(nrow(result$data[[1L]]), 1L, info = column)
      expect_identical(result$data[[1L]]$v, 1, info = column)
    }
    expect_identical(dplyr::collect(source), input, info = column)
  }
})

test_that("dtplyr special nesting preserves typed groups and source rows", {
  skip_if_suggest_absent("dtplyr")
  # The eight cases cross both roles and plan shapes with 0/1/2/3 rows. The
  # three-row cases contain one missing key and two identical source rows;
  # the two-row dimension case makes a missing detail and Margin display alike.
  cases <- list(
    list("fixed", 0L, FALSE, "Total", NULL, "none", "one"),
    list("fixed", 1L, TRUE, NULL, "set", "first", "one"),
    list("fixed", 2L, FALSE, NA_character_, NULL, "last", "rollup"),
    list("fixed", 3L, TRUE, "Total", "set", "none", "rollup"),
    list("dimension", 0L, TRUE, NULL, NULL, "none", "one"),
    list("dimension", 1L, FALSE, "Total", "set", "last", "one"),
    list("dimension", 2L, TRUE, NA_character_, NULL, "first", "rollup"),
    list("dimension", 3L, FALSE, NULL, "set", "none", "rollup")
  )
  for (column in c(".N", ".I", ".SD", ".GRP", ".NGRP")) {
    for (case in cases) {
      role <- case[[1L]]
      size <- case[[2L]]
      keep <- case[[3L]]
      label <- case[[4L]]
      id <- case[[5L]]
      sort <- case[[6L]]
      plan <- case[[7L]]
      input <- tibble::tibble(
        special = rep(c("a", NA_character_, "a"), length.out = size),
        g = rep(c("x", "y", "x"), length.out = size),
        v = rep(c(1L, 2L, 1L), length.out = size)
      )
      names(input)[1L] <- column
      original <- unserialize(serialize(input, NULL))
      grouping <- if (identical(role, "dimension")) {
        if (identical(plan, "one")) {
          grouping_set(dplyr::all_of(column))
        } else {
          rollup(dplyr::all_of(column))
        }
      } else if (identical(plan, "one")) {
        grouping_set()
      } else {
        rollup(g)
      }
      source <- dtplyr::lazy_dt(input)
      run <- function(verb, data) {
        verb(
          data, .by = if (identical(role, "fixed")) dplyr::all_of(column),
          .grouping = grouping, .keep = keep, .margin_label = label,
          .id = id, .sort = sort
        )
      }
      info <- paste(column, role, size, keep, sort, plan)
      for (verb in list(nest_with_margins, nest_by_with_margins)) {
        actual <- run(verb, source)
        if (identical(verb, nest_with_margins)) {
          expect_s3_class(actual, "dtplyr_step")
          actual <- dplyr::collect(actual)
          expect_identical(dplyr::group_vars(actual), character())
        } else {
          expect_s3_class(actual, "rowwise_df")
        }
        expected <- run(verb, input)
        expect_identical(names(actual), names(expected), info = info)
        expect_special_nesting_rows(actual, expected, sort, info = info)
        sets <- if (identical(plan, "rollup")) 2L else 1L
        expect_identical(
          sum(vapply(actual$data, nrow, integer(1))),
          size * sets, info = info
        )
        if (keep && size > 0L) {
          expect_true(all(vapply(
            actual$data, function(cell) column %in% names(cell), logical(1)
          )), info = info)
        }
      }
      expect_identical(dplyr::collect(source), input, info = info)
      expect_identical(input, original, info = info)
    }
  }
})

test_that("dtplyr special nesting keeps rows when no payload remains", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(.N = c("a", "a", NA_character_))
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    result <- verb(dtplyr::lazy_dt(input), .by = .N)
    if (identical(verb, nest_with_margins)) result <- dplyr::collect(result)
    expect_identical(names(result), c(".N", "data"))
    expect_identical(sort(vapply(result$data, nrow, integer(1))), c(1L, 2L))
    expect_true(all(vapply(result$data, ncol, integer(1)) == 0L))

    result <- verb(dtplyr::lazy_dt(input), .grouping = rollup(.N),
                   .margin_label = NULL, .id = "set")
    if (identical(verb, nest_with_margins)) result <- dplyr::collect(result)
    expect_identical(names(result), c(".N", "set", "data"))
    expect_identical(
      sort(vapply(result$data, nrow, integer(1))), c(1L, 2L, 3L)
    )
    expect_true(all(vapply(result$data, ncol, integer(1)) == 0L))
  }
})

test_that("dtplyr special factor nesting retains levels and missing groups", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(
    .N = addNA(factor(c("a", NA, "a"), levels = c("a", "b"))),
    v = c(1L, 2L, 1L)
  )
  source <- dtplyr::lazy_dt(input)
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    run <- function(data) {
      verb(
        data, .grouping = rollup(.N), .margin_label = list(.N = NULL),
        .keep = TRUE, .id = "set", .sort = "last"
      )
    }
    actual <- run(source)
    if (identical(verb, nest_with_margins)) actual <- dplyr::collect(actual)
    expected <- run(input)
    expect_special_nesting_rows(actual, expected, "last")
    expect_identical(levels(actual$.N), levels(expected$.N))
    expect_identical(
      sum(vapply(actual$data, nrow, integer(1))), 6L
    )
  }
  expect_identical(dplyr::collect(source), input)
})

test_that("dtplyr nesting keeps control names and occupied temporary names", {
  skip_if_suggest_absent("dtplyr")
  for (column in c(".BY", ".EACHI", "T", "F", "g")) {
    input <- tibble::tibble(special = c("a", "b"), v = 1:2)
    names(input)[1L] <- column
    source <- dtplyr::lazy_dt(input)
    actual <- dplyr::collect(nest_with_margins(
      source, .by = dplyr::all_of(column)
    ))
    expect_identical(names(actual), c(column, "data"), info = column)
    expect_identical(actual[[column]], c("a", "b"), info = column)
    expect_identical(dplyr::collect(source), input, info = column)
  }

  input <- tibble::tibble(
    .N = c("a", "a"), g = c("x", "x"), v = c(1L, 1L),
    ..marginplyr_nest_1 = c(4L, 4L),
    ..marginplyr_nest_1_ = c(5L, 5L),
    ..marginplyr_dtplyr_column_1 = c(6L, 6L),
    ..marginplyr_dtplyr_column_1_ = c(7L, 7L),
    ..marginplyr_cell_1 = c(8L, 8L),
    ..marginplyr_cell_1_ = c(9L, 9L)
  )
  source <- dtplyr::lazy_dt(input)
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    run <- function(data) {
      verb(
        data, .by = .N, .grouping = rollup(g), .keep = TRUE,
        .key = "..marginplyr_dtplyr_column_1__", .id = "set", .sort = "last"
      )
    }
    actual <- run(source)
    if (identical(verb, nest_with_margins)) actual <- dplyr::collect(actual)
    expected <- run(input)
    expect_identical(names(actual), c(".N", "g", "set",
                                      "..marginplyr_dtplyr_column_1__"))
    expect_special_nesting_rows(
      actual, expected, "last", "..marginplyr_dtplyr_column_1__"
    )
    expect_identical(
      names(actual[["..marginplyr_dtplyr_column_1__"]][[1L]]), names(input)
    )
  }
  expect_identical(dplyr::collect(source), input)
})
