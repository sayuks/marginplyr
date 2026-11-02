test_that("local selectors capture populated dots before resolving columns", {
  data <- data.frame(x = 1:3)
  expressions <- rlang::exprs(
    (function(...) dplyr::across(..., sum))("x"),
    (function(...) dplyr::across(..1, sum))("x"),
    (function(...) dplyr::across(...))(.cols = "x", .fns = sum)
  )
  for (expression in expressions) {
    expected <- dplyr::summarise(data, !!expression)
    expect_identical(expected$x, 6L)
    actual <- summarize_with_margins(data, !!expression)
    expect_identical(actual, expected, info = rlang::as_label(expression))
  }
})

test_that("populated selectors agree with dplyr across eager summary shapes", {
  fixture <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(7, 9, 11))
  inputs <- list(frame = fixture, tibble = tibble::as_tibble(fixture))
  if (suggest_available("data.table")) {
    inputs$data_table <- data.table::as.data.table(fixture)
  }
  for (helper in c("across", "if_any", "if_all")) {
    fn <- if (helper == "across") quote(sum) else quote(function(x) x > 2)
    arguments <- list(
      positional = list(quote(...), fn),
      named = list(.cols = quote(...), .fns = fn),
      whole = list(quote(...)),
      first = list(quote(..1), fn)
    )
    for (spelling in names(arguments)) {
      body <- rlang::call2(helper, !!!arguments[[spelling]], .ns = "dplyr")
      if (helper != "across") {
        body <- rlang::call2("sum", body)
      }
      forwarded <- if (spelling == "whole") {
        list(.cols = "x", .fns = fn)
      } else {
        list("x")
      }
      expression <- rlang::expr((function(...) !!body)(!!!forwarded))
      for (input in names(inputs)) {
        data <- inputs[[input]]
        for (plan in c("none", "fixed", "rollup")) {
          for (named in c(FALSE, TRUE)) {
            for (preceded in c(FALSE, TRUE)) {
              dots <- if (preceded) rlang::exprs(prior = sum(y)) else list()
              dots <- c(dots, stats::setNames(
                list(expression), if (named) "answer" else ""
              ))
              expected <- if (plan == "none") {
                dplyr::summarise(data, !!!dots)
              } else {
                detail <- dplyr::summarise(data, !!!dots, .by = g)
                if (plan == "fixed") detail else dplyr::bind_rows(
                  detail,
                  dplyr::mutate(dplyr::summarise(data, !!!dots), g = "Total")
                )
              }
              actual <- switch(
                plan,
                none = summarize_with_margins(data, !!!dots),
                fixed = summarize_with_margins(data, !!!dots, .by = g),
                rollup = summarize_with_margins(
                  data, !!!dots, .grouping = rollup(g)
                )
              )
              expect_equal(
                as.data.frame(actual), as.data.frame(expected),
                info = paste(helper, spelling, input, plan, named, preceded)
              )
            }
          }
        }
      }
    }
  }
})

test_that("selection wrappers and function forwarding retain dplyr results", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(7, 9, 11))
  for (helper in c("across", "if_any", "if_all")) {
    fn <- if (helper == "across") quote(sum) else quote(function(x) x > 2)
    selectors <- rlang::exprs(c(...), dplyr::all_of(...), dplyr::all_of(..1))
    for (selector in selectors) {
      body <- rlang::call2(helper, selector, fn, .ns = "dplyr")
      if (helper != "across") {
        body <- rlang::call2("sum", body)
      }
      expression <- rlang::expr((function(...) !!body)("x"))
      expected <- dplyr::summarise(data, !!expression, .by = g)
      actual <- summarize_with_margins(data, !!expression, .by = g)
      expect_equal(actual, expected, info = rlang::as_label(expression))
    }
  }
  expressions <- rlang::exprs(
    (function(...) dplyr::across(x, ...))(sum),
    (function(...) dplyr::across(x, .fns = ...))(sum),
    (function(...) dplyr::across(x, .fns = list(...)))(total = sum),
    (function(...) dplyr::across(...))("x", sum),
    (function(...) sum(dplyr::across(...)))(.cols = "x", .fns = ),
    (function(...) dplyr::across(..2, sum))("y", "x")
  )
  for (expression in expressions) {
    expected <- dplyr::summarise(data, value = !!expression, .by = g)
    actual <- summarize_with_margins(data, value = !!expression, .by = g)
    expect_equal(actual, expected, info = rlang::as_label(expression))
  }
})

test_that("top-level function forwarding keeps dplyr expansion timing", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3)
  run <- function(mode) {
    state <- new.env(parent = emptyenv())
    state$events <- integer()
    factory <- function() {
      offset <- dplyr::n()
      state$events <- c(state$events, offset)
      function(x) sum(x) + offset
    }
    wrapper <- function(...) {
      if (mode == "dplyr") {
        dplyr::summarise(data, dplyr::across(x, ...), .by = g)
      } else {
        summarize_with_margins(data, dplyr::across(x, ...), .by = g)
      }
    }
    list(result = wrapper(factory()), events = state$events)
  }
  expected <- run("dplyr")
  expect_identical(expected$events, 0L)
  expect_identical(run("margin"), expected)
})

test_that("forwarded function arguments retain their names", {
  data <- data.frame(x = 1:3)
  f <- function(x, ...) sum(x) + sum(unlist(list(...)))
  expression <- rlang::expr((function(...) dplyr::across(...))(
    .cols = x, .fns = f, .ignore_empty = 2, .named = 3, .ignore_null = 4,
    .unquote_names = 5, .homonyms = 6, .check_assign = 7
  ))
  expected <- suppressWarnings(dplyr::summarise(data, value = !!expression))
  expect_identical(expected$value$x, 33)
  actual <- suppressWarnings(summarize_with_margins(data, value = !!expression))
  expect_identical(actual, expected)
})

test_that("forwarded empty function arguments remain missing", {
  data <- data.frame(x = 1:3)
  f <- function(x, extra = 10) {
    if (missing(extra)) sum(x) else sum(x) + extra
  }
  expression <- rlang::expr((function(...) dplyr::across(...))(
    .cols = x, .fns = f, extra =
  ))
  expected <- suppressWarnings(dplyr::summarise(data, value = !!expression))
  expect_identical(expected$value$x, 6L)
  actual <- suppressWarnings(summarize_with_margins(data, value = !!expression))
  expect_identical(actual, expected)
})

test_that("forwarded arguments keep their evaluation order and environments", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(7, 9, 11))
  for (helper in c("across", "if_any", "if_all")) {
    for (spelling in c("whole", "first")) {
      run <- function(mode) {
        state <- new.env(parent = emptyenv())
        state$events <- character()
        cols <- "x"
        mark <- function(event, value) {
          state$events <- c(state$events, paste(event, dplyr::n()))
          value
        }
        fn <- if (helper == "across") sum else function(x) x > 2
        args <- if (spelling == "whole") {
          list(quote(...))
        } else {
          list(quote(..1), .fns = quote(mark("functions", fn)))
        }
        body <- rlang::call2(helper, !!!args, .ns = "dplyr")
        if (helper != "across") {
          body <- rlang::call2("sum", body)
        }
        forwarded <- if (spelling == "whole") {
          rlang::exprs(.cols = mark("columns", .env$cols),
                       .fns = mark("functions", fn))
        } else {
          rlang::exprs(mark("columns", .env$cols))
        }
        expression <- rlang::expr((function(...) {
          mark("body", NULL)
          !!body
        })(!!!forwarded))
        if (mode == "dplyr") {
          detail <- dplyr::summarise(data, value = !!expression, .by = g)
          total <- dplyr::summarise(data, value = !!expression)
          total$g <- NA_character_
          result <- dplyr::bind_rows(detail, total)
        } else {
          shares <- switch(
            mode, none = list(),
            total = rlang::exprs(share = share_of_total(total)),
            parent = rlang::exprs(share = share_of_parent(total))
          )
          result <- summarize_with_margins(
            data, value = !!expression, total = sum(y), !!!shares,
            .grouping = rollup(g), .margin_label = NULL
          )
        }
        list(result = result, events = state$events)
      }
      expected <- run("dplyr")
      expect_identical(sum(grepl("body", expected$events)), 3L)
      for (mode in c("none", "total", "parent")) {
        actual <- run(mode)
        expect_equal(actual$result[names(expected$result)], expected$result,
                     info = paste(helper, spelling, mode))
        expect_identical(actual$events, expected$events,
                         info = paste(helper, spelling, mode))
      }
    }
  }
})

test_that("forwarded selectors retain key, collision and share guards", {
  data <- data.frame(fixed = "u", g = c("a", "a", "b"), x = 1:3)
  for (helper in c("across", "if_any", "if_all")) {
    fn <- if (helper == "across") quote(identity) else quote(function(x) x > 0)
    body <- rlang::expr(sum(!!rlang::call2(
      helper, quote(...), .ns = "dplyr"
    )))
    forwarded <- function(column) {
      rlang::expr((function(...) !!body)(.cols = !!column, .fns = !!fn))
    }
    for (column in c("fixed", "g")) {
      expect_error(summarize_with_margins(
        data, value = !!forwarded(column), .by = fixed,
        .grouping = rollup(g)
      ), "doesn't exist", info = paste(helper, column))
    }
    expect_error(summarize_with_margins(
      data, previous = sum(x), source = !!forwarded("previous"),
      share = share_of_total(source), .grouping = rollup(g)
    ), "earlier summary", class = "marginplyr_error", info = helper)
    expect_error(summarize_with_margins(
      data, total = sum(x), share = share_of_total(total),
      observed = !!forwarded("share"), .grouping = rollup(g)
    ), "share", info = helper)
  }
  expect_error(summarize_with_margins(
    data, (function(...) dplyr::across(...))(
      .cols = "x", .fns = sum, .names = "g"
    ), .grouping = rollup(g)
  ), "cannot overwrite grouping", class = "marginplyr_error")
})
