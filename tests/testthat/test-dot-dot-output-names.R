test_that("local summaries preserve dplyr dot-dot output names", {
  for (data in list(data.frame(value = 1L),
                    dplyr::as_tibble(data.frame(value = 1L)))) {
    expected <- dplyr::summarize(
      data, ... = sum(value), ..1 = sum(value), ..2 = sum(value)
    )
    result <- summarize_with_margins(
      data, ... = sum(value), ..1 = sum(value), ..2 = sum(value)
    )
    expect_identical(names(result), names(expected))
    for (name in names(expected)) {
      expect_identical(result[[name]], expected[[name]])
    }
  }
})

test_that("dot-dot summaries survive one-set and multi-set grouped reports", {
  source <- data.frame(group = c("b", "a"), value = 1:2)
  for (data in list(source, dplyr::as_tibble(source))) {
    for (name in c("...", "..1", "..2")) {
      expected <- dplyr::summarize(data, !!name := sum(value), .by = group)
      for (multiple in c(FALSE, TRUE)) {
        spec <- if (multiple) rollup(group) else grouping_set(group)
        for (sort in c("none", "first", "last")) {
          result <- summarize_with_margins(
            data, !!name := sum(value), .grouping = spec, .sort = sort
          )
          groups <- if (multiple) c("b", "a", "Total") else c("b", "a")
          values <- if (multiple) c(expected[[name]], 3L) else expected[[name]]
          rows <- if (sort == "none") {
            seq_along(groups)
          } else if (multiple && sort == "first") {
            c(3L, 2L, 1L)
          } else {
            if (multiple) c(2L, 1L, 3L) else c(2L, 1L)
          }
          expect_identical(names(result), c("group", name))
          expect_identical(result[["group"]], groups[rows])
          expect_identical(result[[name]], values[rows])
        }
      }
    }
  }
})

test_that("expanded frames and across retain dplyr dot-dot columns", {
  source <- data.frame(group = c("b", "a"), value = 1:2)
  for (data in list(source, dplyr::as_tibble(source))) {
    for (name in c("...", "..1", "..2")) {
      frame <- function(value) stats::setNames(data.frame(sum(value)), name)
      for (spec in list(grouping_set(group), rollup(group))) {
        expected <- dplyr::summarize(data, frame(value), .by = group)
        result <- summarize_with_margins(data, frame(value), .grouping = spec)
        across <- summarize_with_margins(
          data, dplyr::across(value, frame), .grouping = spec
        )
        expect_identical(names(result), c("group", name))
        expect_identical(result[[name]][1:2], expected[[name]])
        expected_across <- dplyr::summarize(
          data, dplyr::across(value, frame), .by = group
        )
        expect_identical(names(across), names(expected_across))
        expect_identical(names(across[["value"]]), name)
        expect_identical(across[["value"]][[name]][1:2],
                         expected_across[["value"]][[name]])
        if (nrow(result) == 3L) {
          expect_identical(result[[name]][[3L]], 3L)
          expect_identical(across[["value"]][[name]][[3L]], 3L)
        }
      }
    }
  }
})

test_that("dot-dot identifiers obey the scalar-string contract", {
  source <- data.frame(group = c("b", "a"), value = 1:2)
  for (data in list(source, dplyr::as_tibble(source))) {
    for (name in c("...", "..1", "..2", "..0", "..01", "..10")) {
      for (spec in list(grouping_set(group), rollup(group))) {
        for (sort in c("none", "first", "last")) {
          result <- summarize_with_margins(
            data, .grouping = spec, .id = name, .sort = sort
          )
          expect_identical(names(result), c("group", name))
          ids <- if (nrow(result) == 2L) c(1L, 1L) else c(1L, 1L, 2L)
          if (sort == "first") ids <- rev(ids)
          expect_identical(result[[name]], ids)
          expanded <- expand_with_margins(
            data, .grouping = spec, .id = name, .sort = sort
          )
          expect_identical(names(expanded), c("group", name, "value"))
          expect_identical(unique(expanded[[name]]), unique(ids))
          for (verb in list(nest_with_margins, nest_by_with_margins)) {
            nested <- verb(data, .grouping = spec, .id = name, .sort = sort)
            expect_identical(names(nested), c("group", name, "data"))
            expect_identical(nested[[name]], ids)
            if (inherits(nested, "rowwise_df")) {
              expect_identical(dplyr::group_vars(nested), c("group", name))
            }
          }
        }
      }
    }
  }
})

test_that("dot-dot outputs retain grouping and identifier collision checks", {
  data <- data.frame(group = c("b", "a"), value = 1:2)
  frame <- function(value, name) {
    stats::setNames(data.frame(sum(value), sum(value)), c("..1", name))
  }
  expect_error(
    summarize_with_margins(
      data, frame(value, "group"), .grouping = rollup(group)
    ),
    "cannot overwrite grouping column", class = "marginplyr_error"
  )
  for (name in c("...", "..1", "..2")) {
    expect_error(
      summarize_with_margins(data, !!name := sum(value), .id = name),
      "conflicts with a summary output", class = "marginplyr_error"
    )
    dynamic <- function(value) stats::setNames(data.frame(sum(value)), name)
    expect_error(
      summarize_with_margins(data, dynamic(value), .id = name),
      "conflicts with a summary output", class = "marginplyr_error"
    )
  }
  expect_error(
    summarize_with_margins(
      data, frame(value, "..marginplyr_sort_1"), .grouping = rollup(group),
      .sort = "last"
    ),
    "conflict with internal grouping columns", class = "marginplyr_error"
  )
})

test_that("dot-dot nesting identifiers do not collide with cell names", {
  data <- data.frame(group = c("b", "a"), value = 1:2)
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    result <- verb(
      data, .grouping = rollup(group), .id = "..1",
      .key = "..marginplyr_column_1", .sort = "last"
    )
    expect_identical(names(result), c("group", "..1", "..marginplyr_column_1"))
    expect_identical(result[["..1"]], c(1L, 1L, 2L))
    expect_identical(result[["..marginplyr_column_1"]][[3L]][["value"]], 1:2)
  }
  empty <- nest_by_with_margins(data.frame(value = integer()), .id = "...")
  expect_identical(names(empty), c("...", "data"))
  expect_identical(empty[["..."]], 1L)
  expect_identical(dplyr::group_vars(empty), "...")
})

test_that("dot-dot names survive contextual-share staging", {
  data <- data.frame(group = c("b", "a"), value = 1:2)
  for (name in c("...", "..1", "..2")) {
    for (sort in c("none", "last", "first")) {
      rows <- if (sort == "none") {
        1:3
      } else if (sort == "last") {
        c(2L, 1L, 3L)
      } else {
        c(3L, 2L, 1L)
      }
      result <- summarize_with_margins(
        data, !!name := sum(value), total = sum(value),
        share = share_of_total(total), .grouping = rollup(group), .sort = sort
      )
      expect_identical(names(result), c("group", name, "total", "share"))
      expect_identical(result[[name]], c(1L, 2L, 3L)[rows])
      expect_equal(result[["share"]], c(1 / 3, 2 / 3, 1)[rows])
      share <- summarize_with_margins(
        data, total = sum(value), !!name := share_of_parent(total),
        .grouping = rollup(group), .sort = sort
      )
      expect_identical(names(share), c("group", "total", name))
      expect_equal(share[[name]], c(1 / 3, 2 / 3, 1)[rows])
      identified <- summarize_with_margins(
        data, total = sum(value), share = share_of_total(total),
        .grouping = rollup(group), .id = name, .sort = sort
      )
      expect_identical(names(identified), c("group", name, "total", "share"))
      expect_identical(identified[[name]], c(1L, 1L, 2L)[rows])
    }
  }
})
