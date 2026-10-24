test_that("nested Margin summaries keep their own Grouping plan", {
  outer <- tibble::tibble(g = "a")
  inner <- tibble::tibble(g = "b", h = "c", value = 2)
  independent <- function() {
    summarize_with_margins(
      inner,
      bit = grouping_bit(g),
      id = marginplyr::grouping_id(g, h),
      .grouping = cube(g, h)
    )
  }
  expected <- independent()
  expect_identical(expected$bit, c(0L, 0L, 1L, 1L))
  expect_identical(expected$id, c(0L, 1L, 2L, 3L))

  actual <- summarize_with_margins(
    outer,
    nested = list(marginplyr::summarize_with_margins(
      inner,
      bit = grouping_bit(g),
      id = marginplyr::grouping_id(g, h),
      .grouping = cube(g, h)
    )),
    control = list(independent()),
    .grouping = rollup(g)
  )
  expect_length(actual$nested, 2L)
  for (i in seq_len(nrow(actual))) {
    expect_identical(actual$nested[[i]], expected)
    expect_identical(actual$nested[[i]], actual$control[[i]])
  }
})

test_that("nested Grouping bits do not become outer branch constants", {
  data <- tibble::tibble(g = "a")
  actual <- summarize_with_margins(
    data,
    nested = list(summarise_with_margins(
      data,
      bit = marginplyr::grouping_bit(g),
      id = grouping_id(),
      .grouping = rollup(g)
    )),
    .grouping = rollup(g)
  )
  for (nested in actual$nested) {
    expect_identical(nested$bit, c(0L, 1L))
    expect_identical(nested$id, c(0L, 1L))
  }
})

test_that("nested Margin summaries own Parent and Total share validation", {
  outer <- tibble::tibble(g = "a", value = 5)
  inner <- tibble::tibble(h = c("a", "b"), value = c(2, 6))
  independent <- function() {
    summarize_with_margins(
      inner, total = sum(value),
      parent = share_of_parent(total),
      overall = marginplyr::share_of_total(total),
      .grouping = rollup(h)
    )
  }
  expected <- independent()
  expect_identical(expected$parent, c(0.25, 0.75, 1))
  expect_identical(expected$overall, c(0.25, 0.75, 1))

  actual <- summarize_with_margins(
    outer,
    nested = list(summarize_with_margins(
      inner, total = sum(value),
      parent = share_of_parent(total),
      overall = marginplyr::share_of_total(total),
      .grouping = rollup(h)
    )),
    control = list(independent()),
    .grouping = grouping_set(g)
  )
  expect_identical(actual$nested[[1L]], expected)
  expect_identical(actual$nested, actual$control)

  with_outer_share <- summarize_with_margins(
    outer,
    total = sum(value), parent = share_of_total(total),
    nested = list(summarize_with_margins(
      inner, parent = sum(value), overall = share_of_total(parent),
      .grouping = rollup(h)
    )),
    selected = list(dplyr::mutate(
      tibble::tibble(parent = 2), dplyr::across(parent, ~ .x * 2)
    )),
    .grouping = rollup(g)
  )
  for (nested in with_outer_share$nested) {
    expect_identical(nested$parent, c(2, 6, 8))
    expect_identical(nested$overall, c(0.25, 0.75, 1))
  }
  for (selected in with_outer_share$selected) {
    expect_identical(selected, tibble::tibble(parent = 4))
  }
  expect_error(
    summarize_with_margins(
      outer, total = sum(value), parent = share_of_total(total),
      nested = list(dplyr::mutate(tibble::tibble(x = parent), x = x * 2)),
      .grouping = rollup(g)
    ),
    "Ordinary summaries cannot use an earlier Total share",
    class = "marginplyr_error"
  )
})

test_that("operation inputs still evaluate in the enclosing Margin summary", {
  data <- tibble::tibble(g = "a")
  actual <- summarize_with_margins(
    data,
    nested = list((dplyr::mutate)(
      .data = tibble::tibble(g = grouping_bit(g)),
      dplyr::across(dplyr::everything(), ~ .x * 2)
    )),
    partial = list(dplyr::mutate(
      .d = tibble::tibble(g = grouping_bit(g)),
      dplyr::across(dplyr::everything(), ~ .x * 2)
    )),
    computed = list((function(bit = grouping_bit(g)) {
      list(bit = bit, data = dplyr::mutate(
        tibble::tibble(g = 1), dplyr::across(dplyr::everything(), ~ .x * 2)
      ))
    })()),
    captured = list(quote(dplyr::mutate(data, bit = grouping_bit(g)))),
    evaluated = eval(quote(grouping_bit(g))),
    .grouping = rollup(g)
  )
  expect_identical(actual$nested, list(
    tibble::tibble(g = 0), tibble::tibble(g = 2)
  ))
  expect_identical(actual$partial, actual$nested)
  expect_identical(lapply(actual$computed, `[[`, "bit"), list(0L, 1L))
  for (computed in actual$computed) {
    expect_identical(computed$data, tibble::tibble(g = 2))
  }
  expect_identical(actual$captured, rep(list(
    quote(dplyr::mutate(data, bit = grouping_bit(g)))
  ), 2))
  expect_identical(actual$evaluated, c(0L, 1L))
  expect_error(
    summarize_with_margins(
      data, nested = list(dplyr::mutate(dplyr::cur_group())),
      .grouping = rollup(g)
    ),
    "does not support",
    class = "marginplyr_error"
  )
})

test_that("nested lazy summaries do not execute their input", {
  outer <- tibble::tibble(g = "a")
  inner <- dbplyr::lazy_frame(h = "b", value = 2)
  expected <- summarize_with_margins(
    inner, total = sum(value), bit = grouping_bit(h),
    .grouping = rollup(h)
  )
  actual <- summarize_with_margins(
    outer,
    nested = list(summarize_with_margins(
      inner, total = sum(value), bit = grouping_bit(h),
      .grouping = rollup(h)
    )),
    .grouping = rollup(g)
  )
  for (nested in actual$nested) {
    expect_s3_class(nested, "tbl_lazy")
    expect_identical(dbplyr::sql_render(nested), dbplyr::sql_render(expected))
  }
})

test_that("nested dplyr selections use their own data mask", {
  outer <- tibble::tibble(g = "a", value = 1)
  inner <- tibble::tibble(g = 1, x = 2)
  independent <- function() {
    dplyr::mutate(inner, dplyr::across(dplyr::everything(), ~ .x * 2))
  }
  expected <- tibble::tibble(g = 2, x = 4)
  expect_identical(independent(), expected)

  actual <- summarize_with_margins(
    outer,
    nested = list(dplyr::mutate(
      inner, dplyr::across(dplyr::everything(), ~ .x * 2)
    )),
    selected = list(dplyr::select(inner, dplyr::where(is.numeric))),
    counted = list(dplyr::count(
      inner, wt = rowSums(dplyr::across(dplyr::everything()))
    )),
    grouped = list(dplyr::summarise(
      dplyr::group_by(inner, g), group = dplyr::cur_group_id()
    )),
    control = list(independent()),
    .grouping = rollup(g)
  )
  for (i in seq_len(nrow(actual))) {
    expect_identical(actual$nested[[i]], expected)
    expect_identical(actual$nested[[i]], actual$control[[i]])
    expect_identical(actual$selected[[i]], inner)
    expect_identical(actual$counted[[i]], tibble::tibble(n = 3))
    expect_identical(actual$grouped[[i]]$group, 1L)
  }
  expect_error(
    summarize_with_margins(
      outer, nested = list(dplyr::mutate()), .grouping = rollup(g)
    ),
    "no applicable method"
  )
})
