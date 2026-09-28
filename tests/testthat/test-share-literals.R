test_that("literal ordinary sources produce Total and Parent shares", {
  single <- summarize_with_margins(
    data.frame(v = 1), total = 1, share = share_of_total(total)
  )
  expect_identical(names(single), c("total", "share"))
  expect_identical(single$total, 1)
  expect_identical(single$share, 1)
  expect_identical(nrow(single), 1L)

  parent <- summarize_with_margins(
    data.frame(group = c("x", "y"), v = c(1, 3)),
    total = 1L,
    share = share_of_parent(total),
    .grouping = rollup(group),
    .margin_label = NULL
  )
  expect_identical(names(parent), c("group", "total", "share"))
  expect_identical(parent$total, rep(1L, 3L))
  expect_identical(parent$share, rep(1, 3L))
  expect_identical(nrow(parent), 3L)
})

test_that("an unnamed literal survives the ordinary summary wrapper", {
  source <- rlang::new_quosure(1, env = rlang::empty_env())
  result <- summarize_with_margins(
    data.frame(v = 1), !!source
  )

  expect_identical(names(result), "1")
  expect_identical(result[["1"]], 1)
  expect_identical(nrow(result), 1L)
})

test_that("injected literal sources retain missing and Grand total rules", {
  data <- data.frame(group = c("x", "y"), v = c(1, 3))
  cases <- list(
    integer = list(value = 1L, detail = c(1, 1, 1)),
    double = list(value = 1, detail = c(1, 1, 1)),
    missing = list(value = NA_real_, detail = c(NA_real_, NA_real_, 1)),
    nan = list(value = NaN, detail = c(NA_real_, NA_real_, 1)),
    zero = list(value = 0, detail = c(NA_real_, NA_real_, 1))
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    source <- rlang::new_quosure(case$value, env = rlang::empty_env())
    parent <- summarize_with_margins(
      data,
      total = !!source,
      share = share_of_parent(total),
      .grouping = rollup(group),
      .margin_label = NULL
    )
    whole <- summarize_with_margins(
      data,
      total = !!source,
      share = share_of_total(total)
    )

    expect_identical(names(parent), c("group", "total", "share"),
                     info = case_name)
    expect_identical(parent$total, rep(case$value, 3L), info = case_name)
    expect_identical(parent$share, case$detail, info = case_name)
    expect_identical(nrow(parent), 3L, info = case_name)
    expect_identical(names(whole), c("total", "share"), info = case_name)
    expect_identical(whole$total, case$value, info = case_name)
    expect_identical(whole$share, 1, info = case_name)
    expect_identical(nrow(whole), 1L, info = case_name)
  }
})

test_that("literal shares retain declared types on zero-row inputs", {
  data <- data.frame(partition = character(), group = character(), v = double())
  whole <- summarize_with_margins(
    data,
    total = 1,
    share = share_of_total(total),
    .by = partition
  )
  parent <- summarize_with_margins(
    data,
    total = 1L,
    share = share_of_parent(total),
    .by = partition,
    .grouping = rollup(group)
  )

  expect_identical(names(whole), c("partition", "total", "share"))
  expect_identical(nrow(whole), 0L)
  expect_identical(whole$total, double())
  expect_identical(whole$share, double())
  expect_identical(names(parent), c("partition", "group", "total", "share"))
  expect_identical(nrow(parent), 0L)
  expect_identical(parent$total, integer())
  expect_identical(parent$share, double())
})

test_that("literal shares work on tibble input and through the alias", {
  data <- tibble::tibble(group = c("x", "y"), v = c(1, 3))
  before <- data
  result <- summarise_with_margins(
    data,
    total = 1,
    parent = share_of_parent(total),
    whole = share_of_total(total),
    .grouping = rollup(group),
    .margin_label = NULL
  )

  expect_identical(names(result), c("group", "total", "parent", "whole"))
  expect_identical(nrow(result), 3L)
  expect_identical(result$total, c(1, 1, 1))
  expect_identical(result$parent, c(1, 1, 1))
  expect_identical(result$whole, c(1, 1, 1))
  expect_identical(data, before)

  empty <- data[0L, ]
  empty_whole <- summarise_with_margins(
    empty, total = 1, share = share_of_total(total), .by = group
  )
  empty_parent <- summarise_with_margins(
    empty, total = 1L, share = share_of_parent(total),
    .by = group, .grouping = rollup(v)
  )
  expect_identical(nrow(empty_whole), 0L)
  expect_identical(empty_whole$share, double())
  expect_identical(nrow(empty_parent), 0L)
  expect_identical(empty_parent$share, double())
})

test_that("literal shares work on eager data.table without changing input", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(group = c("x", "y"), v = c(1, 3))
  before <- data.table::copy(data)
  result <- summarize_with_margins(
    data,
    total = 1L,
    parent = share_of_parent(total),
    whole = share_of_total(total),
    .grouping = rollup(group),
    .margin_label = NULL
  )

  expect_identical(names(result), c("group", "total", "parent", "whole"))
  expect_identical(nrow(result), 3L)
  expect_identical(result$total, rep(1L, 3L))
  expect_identical(result$parent, rep(1, 3L))
  expect_identical(result$whole, rep(1, 3L))
  expect_identical(data, before)

  empty <- data[0L, ]
  empty_whole <- summarize_with_margins(
    empty, total = 1, share = share_of_total(total), .by = group
  )
  empty_parent <- summarize_with_margins(
    empty, total = 1L, share = share_of_parent(total),
    .by = group, .grouping = rollup(v)
  )
  expect_identical(nrow(empty_whole), 0L)
  expect_identical(empty_whole$share, double())
  expect_identical(nrow(empty_parent), 0L)
  expect_identical(empty_parent$share, double())
  expect_identical(data, before)
})

test_that("literal source failures retain Package condition identity and call", {
  data <- data.frame(group = c("x", "y"), v = c(1, 3))
  cases <- list(
    logical = list(value = TRUE, class = "marginplyr_error",
                   message = "plain integer or double scalar"),
    classed = list(value = as.Date("2026-01-01"), class = "marginplyr_error",
                   message = "plain integer or double scalar"),
    empty = list(value = double(),
                 class = "marginplyr_share_cardinality_error",
                 message = "exactly one value per grouping row"),
    multiple = list(value = c(1, 2),
                    class = "marginplyr_share_cardinality_error",
                    message = "exactly one value per grouping row")
  )

  for (kind in c("parent", "total")) {
    for (case_name in names(cases)) {
      case <- cases[[case_name]]
      source <- rlang::new_quosure(case$value, env = rlang::empty_env())
      error <- if (identical(kind, "parent")) {
        expect_error(summarize_with_margins(
          data,
          total = !!source,
          share = share_of_parent(total),
          .grouping = rollup(group)
        ), case$message)
      } else {
        expect_error(summarize_with_margins(
          data,
          total = !!source,
          share = share_of_total(total)
        ), case$message)
      }

      expect_s3_class(error, case$class)
      expect_s3_class(error, "marginplyr_error")
      expect_identical(error$share_output, "share", info = paste(kind, case_name))
      expect_identical(error$source_summary, "total",
                       info = paste(kind, case_name))
      expect_identical(rlang::call_name(conditionCall(error)),
                       "summarize_with_margins", info = paste(kind, case_name))
      expect_match(conditionMessage(error), "share `share`",
                   info = paste(kind, case_name))
      expect_match(conditionMessage(error), "source summary `total`",
                   info = paste(kind, case_name))
    }
  }
})

test_that("literal wrapping leaves expression and environment controls intact", {
  data <- data.frame(v = 3)
  caller_value <- 4
  quosure_env <- rlang::env(injected_value = 5)
  injected <- rlang::new_quosure(rlang::sym("injected_value"), quosure_env)
  before <- data
  calls <- 0L
  counted <- function(x) {
    calls <<- calls + 1L
    x
  }

  result <- summarize_with_margins(
    data,
    total = counted(v + caller_value + !!injected),
    share = share_of_total(total)
  )
  expect_identical(result$total, 12)
  expect_identical(result$share, 1)
  expect_identical(calls, 1L)
  expect_identical(data, before)

  call_source <- summarize_with_margins(
    data, total = identity(1), share = share_of_total(total)
  )
  expect_identical(call_source$total, 1)
  expect_identical(call_source$share, 1)
})

test_that("dtplyr still evaluates literal share sources", {
  skip_if_suggest_absent("dtplyr")
  data <- dtplyr::lazy_dt(data.frame(v = 1))
  result <- dplyr::collect(summarize_with_margins(
    data, total = 1, share = share_of_total(total)
  ))

  expect_identical(names(result), c("total", "share"))
  expect_identical(nrow(result), 1L)
  expect_identical(result$total, 1)
  expect_identical(result$share, 1)
})
