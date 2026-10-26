test_that("qualified captures retain literal Grouping helpers", {
  data <- tibble::tibble(a = c("x", "y"), value = c(1, 2))
  capture <- function(input) {
    dplyr::summarise(
      input,
      bit = deparse1(base::bquote(grouping_bit(a))),
      id = deparse1(rlang::expr(grouping_id(a)))
    )
  }
  control <- capture(data)
  result <- summarize_with_margins(
    data,
    bit = deparse1(base::bquote(grouping_bit(a))),
    id = deparse1(rlang::expr(grouping_id(a))),
    .grouping = rollup(a)
  )
  expect_identical(result$bit, rep(control$bit, 3L))
  expect_identical(result$id, rep(control$id, 3L))
})

test_that("qualified captures retain literal Grouping helpers on dtplyr", {
  skip_if_suggest_absent("dtplyr")
  data <- dtplyr::lazy_dt(tibble::tibble(a = c("x", "y"), value = c(1, 2)))
  control <- dplyr::summarise(
    data,
    bit = deparse1(base::bquote(grouping_bit(a))),
    id = deparse1(rlang::expr(grouping_id(a))),
    sub0 = deparse1(base::bquote(f(.(0L)))),
    sub1 = deparse1(base::bquote(f(.(1L))))
  ) |>
    dplyr::collect()
  result <- summarize_with_margins(
    data,
    bit = deparse1(base::bquote(grouping_bit(a))),
    id = deparse1(rlang::expr(grouping_id(a))),
    substituted = deparse1(base::bquote(f(.(grouping_bit(a))))),
    evaluated = eval(base::quote(grouping_bit(a))),
    recovered = eval(rlang::expr(grouping_id(a))),
    .grouping = rollup(a)
  ) |>
    dplyr::collect()
  expect_identical(result$bit, rep(control$bit, 3L))
  expect_identical(result$id, rep(control$id, 3L))
  expect_identical(result$substituted, c(rep(control$sub0, 2L), control$sub1))
  expect_identical(result$evaluated, c(0L, 0L, 1L))
  expect_identical(result$recovered, result$evaluated)
  expect_error(
    summarize_with_margins(
      data,
      text = deparse1(base::bquote(f(..(grouping_bit(a))), splice = TRUE)),
      .grouping = rollup(a)
    ),
    "Cannot analyze splicing", class = "marginplyr_error"
  )
})

test_that("bquote substitutions and recovered language evaluate helpers", {
  data <- tibble::tibble(a = c("x", "y"), value = c(1, 2))
  result <- summarize_with_margins(
    data,
    substituted = deparse1(base::bquote(f(.(grouping_bit(a))))),
    quoted = eval(base::quote(grouping_bit(a))),
    captured = eval(rlang::expr(grouping_id(a))),
    bquoted = eval(base::bquote(grouping_bit(a))),
    unquoted = eval(base::bquote(.(grouping_bit(a)))),
    mixed = eval(base::bquote(grouping_bit(a) + .(0L))),
    both = eval(base::bquote(grouping_bit(a) + .(grouping_bit(a)))),
    recaptured = deparse1(eval(base::bquote(base::quote(.(grouping_bit(a)))))),
    controlled = eval(base::bquote(
      grouping_bit(a) + .(0L), where = baseenv(), splice = FALSE
    )),
    null = deparse1(base::bquote(f(.(NULL), .(grouping_bit(a))))),
    .grouping = rollup(a)
  )
  expect_identical(result$substituted, c("f(0L)", "f(0L)", "f(1L)"))
  expect_identical(result$quoted, c(0L, 0L, 1L))
  expect_identical(result$captured, result$quoted)
  expect_identical(result$bquoted, result$quoted)
  expect_identical(result$unquoted, result$quoted)
  expect_identical(result$mixed, result$quoted)
  expect_identical(result$both, 2L * result$quoted)
  expect_identical(result$recaptured, c("0L", "0L", "1L"))
  expect_identical(result$controlled, result$quoted)
  expect_identical(result$null, c("f(NULL, 0L)", "f(NULL, 0L)", "f(NULL, 1L)"))
})

test_that("only evaluated capture parts depend on preceding shares", {
  data <- tibble::tibble(a = c("x", "y"), value = c(1, 2))
  literal <- summarize_with_margins(
    data,
    total = sum(value),
    share = share_of_total(total),
    held = deparse1(base::bquote(share)),
    captured = deparse1(rlang::expr(share)),
    .grouping = rollup(a)
  )
  expect_identical(literal$held, rep("share", 3L))
  expect_identical(literal$captured, literal$held)
  expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      share = share_of_total(total),
      read = deparse1(base::bquote(f(.(share)))),
      .grouping = rollup(a)
    ),
    "`share`", class = "marginplyr_error"
  )
  expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      share = share_of_total(total),
      read = eval(rlang::expr(share)),
      .grouping = rollup(a)
    ),
    "`share`", class = "marginplyr_error"
  )
  expect_error(
    summarize_with_margins(
      data,
      id = eval(base::bquote(dplyr::cur_group_id())),
      .grouping = rollup(a)
    ),
    "does not support", class = "marginplyr_error"
  )
  expect_error(
    summarize_with_margins(
      data,
      total = sum(value),
      share = share_of_total(total),
      read = eval(base::bquote(share + .(0L))),
      .grouping = rollup(a)
    ),
    "`share`", class = "marginplyr_error"
  )
  expect_error(
    summarize_with_margins(
      data,
      id = eval(base::bquote(dplyr::cur_group_id() + .(0L))),
      .grouping = rollup(a)
    ),
    "does not support", class = "marginplyr_error"
  )
})

test_that("unsupported bquote splicing is refused before rewriting", {
  data <- tibble::tibble(a = c("x", "y"))
  expect_error(
    summarize_with_margins(
      data,
      text = deparse1(base::bquote(f(..(grouping_bit(a))), splice = TRUE)),
      .grouping = rollup(a)
    ),
    "Cannot analyze splicing", class = "marginplyr_error"
  )
  literal <- summarize_with_margins(
    data,
    text = deparse1(base::bquote(f(..(grouping_bit(a))), splice = FALSE)),
    .grouping = rollup(a)
  )
  expect_identical(literal$text, rep("f(..(grouping_bit(a)))", 3L))
  parenthesized <- summarize_with_margins(
    data,
    text = deparse1(base::bquote(f(..(grouping_bit(a))), splice = (FALSE))),
    .grouping = rollup(a)
  )
  expect_identical(parenthesized$text, literal$text)
})

test_that("capture heads and formals retain their literal syntax", {
  data <- tibble::tibble(a = c("x", "y"))
  result <- summarize_with_margins(
    data,
    head = deparse1((base::bquote)(grouping_bit(a))),
    named = deparse1(base::bquote(e = grouping_bit(a))),
    formal = deparse1(base::bquote(function(x = .(grouping_bit(a))) x)),
    captured = deparse1(rlang::get_expr(rlang::quo(grouping_id(a)))),
    .grouping = rollup(a)
  )
  expect_identical(result$head, rep("grouping_bit(a)", 3L))
  expect_identical(result$named, result$head)
  expect_identical(
    result$formal,
    c("function(x = 0L) x", "function(x = 0L) x", "function(x = 1L) x")
  )
  expect_identical(result$captured, rep("grouping_id(a)", 3L))
})

test_that("a caller's expr function keeps ordinary lexical lookup", {
  data <- tibble::tibble(a = c("x", "y"))
  expr <- function(x) x + 10L
  bquote <- function(x) x + 10L
  result <- summarize_with_margins(
    data, value = expr(grouping_bit(a)), .grouping = rollup(a)
  )
  expect_identical(result$value, c(10L, 10L, 11L))
  from_bquote <- summarize_with_margins(
    data, value = bquote(grouping_bit(a)), .grouping = rollup(a)
  )
  expect_identical(from_bquote$value, result$value)
})
