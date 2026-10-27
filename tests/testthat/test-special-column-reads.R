test_that("valid special names remain usable as fixed and variable keys", {
  keys <- list(
    c("b", "a"), c(2L, 1L), factor(c("b", "a")),
    as.Date(c("2026-01-02", "2026-01-01"))
  )
  for (key in keys) {
    for (empty in c(FALSE, TRUE)) {
      data <- data.frame(key = key, value = c(1L, 3L))
      if (empty) data <- data[FALSE, , drop = FALSE]
      names(data)[1L] <- "..0"
      expected <- dplyr::summarize(
        data, total = sum(value), .by = dplyr::all_of("..0")
      )
      for (fixed in c(FALSE, TRUE)) {
        result <- if (fixed) {
          summarize_with_margins(
            data, total = sum(value), .by = dplyr::all_of("..0"),
            .margin_label = NULL
          )
        } else {
          summarize_with_margins(
            data, total = sum(value),
            .grouping = grouping_set(dplyr::all_of("..0")),
            .margin_label = NULL
          )
        }
        expect_identical(names(result), c("..0", "total"))
        expect_identical(result[["..0"]], expected[["..0"]])
        expect_identical(result[["total"]], expected[["total"]])
      }
    }
  }
})

test_that("valid special dimensions support Margin labels and ordering", {
  data <- data.frame(key = c("b", "a"), value = c(1L, 3L))
  names(data)[1L] <- "..0"
  for (sort in c("none", "first", "last")) {
    result <- summarize_with_margins(
      data, total = sum(value), .grouping = rollup(dplyr::all_of("..0")),
      .sort = sort
    )
    rows <- switch(
      sort, none = 1:3, first = c(3L, 2L, 1L), last = c(2L, 1L, 3L)
    )
    expect_identical(names(result), c("..0", "total"))
    expect_identical(result[["..0"]], c("b", "a", "Total")[rows])
    expect_identical(result[["total"]], c(1L, 3L, 4L)[rows])
  }
})

test_that("special sources retain shares and public names", {
  data <- data.frame(group = c("a", "a", "b"), child = c("x", "y", "x"),
                     value = c(1, 3, 6))
  for (name in c("..0", "..00")) {
    for (across in c(FALSE, TRUE)) {
      shares <- if (across) {
        list(quote(dplyr::across(dplyr::all_of(name), share_of_parent,
                                 .names = "parent")),
             quote(dplyr::across(dplyr::all_of(name), share_of_total,
                                 .names = "overall")))
      } else {
        list(parent = rlang::call2("share_of_parent", rlang::sym(name)),
             overall = rlang::call2("share_of_total", rlang::sym(name)))
      }
      result <- summarize_with_margins(
        data, !!name := sum(value), !!!shares,
        .grouping = rollup(group, child), .margin_label = NULL
      )
      expect_identical(
        names(result), c("group", "child", name, "parent", "overall")
      )
      expect_identical(result[[name]], c(1, 3, 6, 4, 6, 10))
      expect_identical(result[["parent"]], c(0.25, 0.75, 1, 0.4, 0.6, 1))
      expect_identical(result[["overall"]], c(0.1, 0.3, 0.6, 0.4, 0.6, 1))
    }
  }
})

test_that("special fixed keys partition shares without alias collisions", {
  data <- data.frame(fixed = c("f", "f", "g"), group = c("a", "b", "a"),
                     value = c(1, 3, 6))
  names(data)[1L] <- "..0"
  result <- summarize_with_margins(
    data, ..00 = sum(value),
    ..marginplyr_column_1 = share_of_total(..00),
    parent = share_of_parent(..00),
    .by = dplyr::all_of("..0"), .grouping = rollup(group), .sort = "last"
  )
  expect_identical(names(result),
                   c("..0", "group", "..00", "..marginplyr_column_1", "parent"))
  expect_identical(result[["..0"]], c("f", "f", "f", "g", "g"))
  expect_identical(result[["..00"]], c(1, 3, 4, 6, 6))
  expect_identical(result[["..marginplyr_column_1"]], c(0.25, 0.75, 1, 1, 1))
  expect_identical(result[["parent"]], c(0.25, 0.75, 1, 1, 1))
})

test_that("special sources retain scalar validation and caller diagnostics", {
  data <- data.frame(group = c("a", "b"), value = c(1, 3))
  for (name in c("..0", "..00")) {
    for (kind in c("parent", "total")) {
      share <- rlang::call2(paste0("share_of_", kind), rlang::sym(name))
      for (source in list(quote("text"), quote(matrix(1)), quote(c(1, 2)))) {
        error <- expect_error(
          summarize_with_margins(
            data, !!name := !!source, share = !!share, .grouping = rollup(group)
          ),
          class = "marginplyr_error"
        )
        expect_identical(error$source_summary, name)
        expect_identical(error$share_output, "share")
        expect_match(
          conditionMessage(error), paste0("source summary `", name, "`"),
          fixed = TRUE
        )
        expect_identical(rlang::call_name(conditionCall(error)),
                         "summarize_with_margins")
      }
    }
  }
})

test_that("upstream reserved input names retain upstream restrictions", {
  for (name in c("...", "..1")) {
    data <- stats::setNames(data.frame(key = "a", value = 1), c(name, "value"))
    expect_error(dplyr::as_tibble(data),
                 class = "vctrs_error_names_cannot_be_dot_dot")
    expect_error(dbplyr::lazy_frame(!!!data),
                 class = "vctrs_error_names_cannot_be_dot_dot")
    expect_error(
      summarize_with_margins(
        data, total = sum(value),
        .grouping = grouping_set(dplyr::all_of(name))
      ),
      class = "vctrs_error_names_cannot_be_dot_dot"
    )
  }
})
