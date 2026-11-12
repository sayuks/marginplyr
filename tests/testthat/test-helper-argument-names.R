test_that("a share helper refuses its argument under a name other than x", {
  data <- data.frame(
    parent = c("x", "x", "y"), group = c("a", "B", "b"), value = 1:3
  )
  for (input in list(data, dbplyr::tbl_lazy(data))) {
    for (helper in c("share_of_total", "share_of_parent")) {
      for (name in c("y", "X", "xx")) {
        call <- rlang::call2(helper, !!name := quote(total))
        error <- expect_error(
          summarize_with_margins(
            input, total = sum(value), share = !!call,
            .grouping = rollup(parent, group)
          ),
          paste0(
            "`share = ", helper, "\\(...\\)` requires exactly one bare name"
          ),
          class = "marginplyr_error"
        )
        expect_identical(
          rlang::call_name(conditionCall(error)),
          "summarize_with_margins"
        )
      }
    }
  }
})

test_that("a share helper's argument is accepted unnamed or as x", {
  data <- data.frame(
    parent = c("x", "x", "y"), group = c("a", "B", "b"), value = 1:3
  )
  total <- summarize_with_margins(
    data, total = sum(value), share = share_of_total(x = total),
    .grouping = rollup(group)
  )
  expect_equal(total$share, c(1, 2, 3, 6) / 6)
  parent <- summarize_with_margins(
    data, total = sum(value), share = share_of_parent(x = total),
    .grouping = rollup(parent, group)
  )
  expect_equal(parent$share, c(1 / 3, 2 / 3, 1, 1 / 2, 1 / 2, 1))
  for (helper in c("share_of_total", "share_of_parent")) {
    named <- rlang::call2(helper, x = quote(total))
    unnamed <- rlang::call2(helper, quote(total))
    expect_identical(
      summarize_with_margins(
        data, total = sum(value), share = !!named,
        .grouping = rollup(parent, group)
      ),
      summarize_with_margins(
        data, total = sum(value), share = !!unnamed,
        .grouping = rollup(parent, group)
      )
    )
  }
})

test_that("grouping_bit() refuses its column under a name other than x", {
  data <- data.frame(group = c("a", "b"), value = 1:2)
  for (input in list(data, dbplyr::tbl_lazy(data))) {
    error <- expect_error(
      summarize_with_margins(
        input, bit = grouping_bit(y = group), .grouping = rollup(group)
      ),
      "`grouping_bit\\(\\)` takes its column unnamed or as `x`",
      class = "marginplyr_error"
    )
    expect_identical(
      rlang::call_name(conditionCall(error)),
      "summarize_with_margins"
    )
  }
  expect_identical(
    summarize_with_margins(
      data, bit = grouping_bit(x = group), .grouping = rollup(group)
    )$bit,
    c(0L, 0L, 1L)
  )
})

test_that("grouping_id() accepts named columns, as its dots do", {
  data <- data.frame(group = c("a", "b"), value = 1:2)
  expect_identical(
    summarize_with_margins(
      data, id = grouping_id(y = group), .grouping = rollup(group)
    )$id,
    c(0L, 0L, 1L)
  )
})
