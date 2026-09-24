test_that("computed function positions exclude every Grouping plan column", {
  data <- tibble::tibble(g = c("a", "a", "b"), x = 1:3)
  direct <- summarize_with_margins(
    data, out = ncol(dplyr::pick(dplyr::everything())),
    .grouping = rollup(g), .sort = "last"
  )
  computed <- summarize_with_margins(
    data,
    out = (function(z) function(dummy) z)(
      ncol(dplyr::pick(dplyr::everything()))
    )(0),
    .grouping = rollup(g), .sort = "last"
  )
  selected <- summarize_with_margins(
    data,
    selected = (function(z) function(dummy) z)(
      list(names(dplyr::pick(dplyr::everything())))
    )(0),
    .grouping = rollup(g), .sort = "last"
  )
  expect_identical(direct$out, c(1L, 1L, 1L))
  expect_identical(computed$out, direct$out)
  expect_identical(selected$selected, rep(list("x"), 3L))

  keyed <- tibble::tibble(f = c("p", "p", "p"), g = c("a", "a", "b"),
                          h = c("u", "v", "u"), x = 1:3)
  selected <- summarize_with_margins(
    keyed,
    names = (function(z) function(dummy) z)(
      list(names(dplyr::pick(dplyr::everything())))
    )(0),
    .by = f, .grouping = rollup(g, h), .sort = "last"
  )
  expect_identical(selected$names, rep(list("x"), nrow(selected)))
})

test_that("Grouping helpers are rewritten inside computed function positions", {
  data <- tibble::tibble(g = c("a", "a", "b"), x = c(1L, 2L, 10L))
  direct <- summarize_with_margins(
    data,
    bit = if (grouping_bit(g) == 0L) sum(x) else length(x),
    id = if (grouping_id(g) == 0L) sum(x) else length(x),
    .grouping = rollup(g), .sort = "last"
  )
  computed <- summarize_with_margins(
    data,
    bit = (if (grouping_bit(g) == 0L) sum else length)(x),
    id = (if (grouping_id(g) == 0L) sum else length)(x),
    .grouping = rollup(g), .sort = "last"
  )
  expect_identical(computed, direct)
  expect_identical(computed$bit, c(3L, 10L, 3L))
})

test_that("computed function positions refuse branch-local helpers", {
  data <- tibble::tibble(g = c("a", "a", "b"), x = 1:3)
  direct <- expect_error(summarize_with_margins(
    data, out = cur_group_id(), .grouping = rollup(g)
  ))
  computed <- expect_error(summarize_with_margins(
    data,
    out = (if (cur_group_id() == 1L) sum else length)(x),
    .grouping = rollup(g)
  ))
  expect_s3_class(computed, "marginplyr_error")
  expect_identical(conditionMessage(computed), conditionMessage(direct))
})

test_that("quoted helpers in computed call heads remain language data", {
  data <- tibble::tibble(g = c("a", "a", "b"), x = 1:3)
  result <- summarize_with_margins(
    data,
    out = (function(z) function(dummy) z)(
      paste(deparse1(quote(grouping_bit(g))),
            deparse1(quote(cur_group_id())),
            deparse1(quote(pick(everything()))))
    )(0),
    .grouping = rollup(g)
  )
  expect_identical(
    result$out,
    rep("grouping_bit(g) cur_group_id() pick(everything())", 3L)
  )

  evaluated <- summarize_with_margins(
    data,
    out = (function(z) function(dummy) z)(eval(quote(grouping_bit(g))))(0),
    .grouping = rollup(g), .sort = "last"
  )
  expect_identical(evaluated$out, c(0L, 0L, 1L))
  refused <- expect_error(summarize_with_margins(
    data,
    out = (function(z) function(dummy) z)(eval(quote(cur_group_id())))(0),
    .grouping = rollup(g)
  ))
  expect_s3_class(refused, "marginplyr_error")
})
