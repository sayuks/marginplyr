test_that("ordinary local selectors use the executing function's arguments", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7))
  cols <- "y"
  result <- summarize_with_margins(
    data,
    (function(cols) dplyr::across(dplyr::all_of(cols), sum))("x"),
    .grouping = grouping_set(g)
  )
  expect_named(result, c("g", "x"))
  expect_identical(result$x, c(3L, 3L))
})

test_that("the .env pronoun refers to the executing function", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7))
  result <- summarize_with_margins(
    data,
    (function(all_of) dplyr::across(dplyr::all_of(.env$all_of), sum))("x"),
    .grouping = grouping_set(g)
  )
  expect_named(result, c("g", "x"))
  expect_identical(result$x, c(3L, 3L))
})

test_that("injected selections retain their captured environments", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7))
  captured <- rlang::env(
    rlang::current_env(), cols = "y", predicate = is.double
  )
  selected <- rlang::new_quosure(quote(dplyr::all_of(cols)), captured)
  typed <- rlang::new_quosure(quote(dplyr::where(predicate)), captured)
  expressions <- rlang::exprs(
    (function(cols) dplyr::across(!!selected, sum))("x"),
    (function(cols) sum(dplyr::pick(!!selected)))("x"),
    (function(cols) sum(dplyr::if_any(!!selected, function(x) x > 4)))("x"),
    (function(cols) sum(dplyr::if_all(!!selected, function(x) x > 4)))("x"),
    (function(predicate) dplyr::across(!!typed, sum))(is.integer)
  )
  for (expression in expressions) {
    expected <- dplyr::summarise(data, value = !!expression, .by = g)
    actual <- summarize_with_margins(
      data, value = !!expression, .grouping = grouping_set(g)
    )
    expect_equal(actual, expected, info = rlang::as_label(expression))
  }
  expect_identical(expected$value$y, c(11, 7))

  selection_call <- rlang::new_quosure(
    quote(dplyr::across(dplyr::all_of(cols), sum)), captured
  )
  nested <- rlang::expr((function(cols) !!selection_call)("x"))
  actual <- summarize_with_margins(
    data, !!nested, .grouping = grouping_set(g)
  )
  expect_named(actual, c("g", "y"))
  expect_identical(actual$y, c(11, 7))

  function_call <- rlang::new_quosure(quote(
    (function(cols) dplyr::across(dplyr::all_of(cols), sum))("x")
  ), captured)
  actual <- summarize_with_margins(
    data, !!function_call, .grouping = grouping_set(g)
  )
  expect_named(actual, c("g", "x"))
  expect_identical(actual$x, c(3L, 3L))
})

test_that("runtime selectors keep dplyr's evaluation counts beside shares", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7))
  selectors <- rlang::exprs(
    dplyr::across(dplyr::all_of(choose(cols)), sum),
    sum(dplyr::pick(dplyr::all_of(choose(cols)))),
    sum(dplyr::if_any(dplyr::all_of(choose(cols)), function(x) x > 2)),
    sum(dplyr::if_all(dplyr::all_of(choose(cols)), function(x) x > 2)),
    dplyr::across(dplyr::where(choose(predicate)), sum)
  )
  for (selector in selectors) {
    run <- function(mode) {
      state <- new.env(parent = emptyenv())
      state$calls <- integer()
      state$selections <- integer()
      choose <- function(x) {
        state$selections <- c(state$selections, dplyr::n())
        x
      }
      expression <- rlang::expr((function(cols, predicate) {
        state$calls <- c(state$calls, dplyr::n())
        !!selector
      })("x", is.integer))
      if (mode == "dplyr") {
        detail <- dplyr::summarise(data, value = !!expression, .by = g)
        total <- dplyr::summarise(data, value = !!expression)
        total$g <- NA_character_
        result <- dplyr::bind_rows(detail, total)
      } else {
        share <- switch(
          mode, none = list(),
          total = rlang::quos(share = share_of_total(total)),
          parent = rlang::quos(share = share_of_parent(total))
        )
        result <- rlang::inject(summarize_with_margins(
          data, value = !!expression, total = sum(y), !!!share,
          .grouping = rollup(g), .margin_label = NULL
        ))
      }
      list(result = result, calls = state$calls, selections = state$selections)
    }
    expected <- run("dplyr")
    expect_identical(expected$calls, c(2L, 1L, 3L))
    expect_identical(expected$selections, expected$calls)
    for (mode in c("none", "total", "parent")) {
      actual <- run(mode)
      expect_equal(actual$result[names(expected$result)], expected$result,
                   info = rlang::as_label(selector))
      expect_identical(actual$calls, expected$calls)
      expect_identical(actual$selections, expected$selections)
    }
  }
})

test_that("runtime selections exclude every Grouping-plan key", {
  data <- data.frame(fixed = c(1, 1, 1), g = c(1, 1, 2), x = 1:3)
  selectors <- rlang::exprs(
    dplyr::across(dplyr::all_of(cols), sum),
    sum(dplyr::pick(dplyr::all_of(cols))),
    sum(dplyr::if_any(dplyr::all_of(cols), function(x) x > 0)),
    sum(dplyr::if_all(dplyr::all_of(cols), function(x) x > 0))
  )
  for (selector in selectors) {
    for (key in c("fixed", "g")) {
      expression <- rlang::expr((function(cols) !!selector)(!!key))
      expect_error(summarize_with_margins(
        data, value = !!expression, .by = fixed, .grouping = rollup(g),
        .margin_label = NULL
      ), "doesn't exist", info = paste(key, rlang::as_label(selector)))
    }
  }
  actual <- summarize_with_margins(
    data, (function(predicate) dplyr::across(dplyr::where(predicate), sum))(
      is.numeric
    ), .by = fixed, .grouping = rollup(g), .margin_label = NULL
  )
  expect_named(actual, c("fixed", "g", "x"))
  expect_identical(actual$x, c(3L, 3L, 6L))
})

test_that("runtime selectors retain share source restrictions", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7))
  actual <- summarize_with_margins(
    data, source = (function(cols) sum(dplyr::pick(dplyr::all_of(cols))))("x"),
    share = share_of_total(source), .grouping = rollup(g)
  )
  expect_identical(actual$source, c(3L, 3L, 6L))
  expect_identical(actual$share, c(0.5, 0.5, 1))

  expect_error(summarize_with_margins(
    data, source = (function(cols) {
      rep(sum(dplyr::pick(dplyr::all_of(cols))), 2L)
    })("x"), share = share_of_total(source), .grouping = rollup(g)
  ), "scalar")
  expect_error(summarize_with_margins(
    data, source = (function(cols) {
      as.character(sum(dplyr::pick(dplyr::all_of(cols))))
    })("x"), share = share_of_total(source), .grouping = rollup(g)
  ), "integer or double")
  expect_error(summarize_with_margins(
    data, previous = sum(x),
    source = (function(cols) sum(dplyr::pick(dplyr::all_of(cols))))("previous"),
    share = share_of_total(source), .grouping = rollup(g)
  ), "earlier summary")
  expect_error(summarize_with_margins(
    data, total = sum(y), share = share_of_total(total),
    observed = (function(cols) sum(dplyr::pick(dplyr::all_of(cols))))("share"),
    .grouping = rollup(g)
  ), "share")
})

test_that("an unrelated selected share leaves runtime selectors to execution", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7))
  cols <- "y"
  state <- new.env(parent = emptyenv())
  state$calls <- integer()
  choose <- function(cols) {
    state$calls <- c(state$calls, dplyr::n())
    cols
  }
  actual <- summarize_with_margins(
    data,
    (function(cols) dplyr::across(dplyr::all_of(choose(cols)), sum))("x"),
    total = sum(y),
    dplyr::across(dplyr::starts_with("total"), share_of_total,
                  .names = "share"),
    .grouping = rollup(g), .margin_label = NULL
  )
  expect_named(actual, c("g", "x", "total", "share"))
  expect_identical(actual$x, c(3L, 3L, 6L))
  expect_equal(actual$share, c(11 / 18, 7 / 18, 1))
  expect_identical(state$calls, c(2L, 1L, 3L))
})
test_that("selected share sources leave absent runtime bindings to functions", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7))
  selectors <- rlang::exprs(
    dplyr::across(dplyr::all_of(cols), sum),
    sum(dplyr::pick(dplyr::all_of(cols))),
    sum(dplyr::if_any(dplyr::all_of(cols), function(x) x > 2)),
    sum(dplyr::if_all(dplyr::all_of(cols), function(x) x > 2)),
    dplyr::across(dplyr::where(predicate), sum)
  )
  for (selector in selectors) {
    expression <- rlang::expr(
      (function(cols, predicate) !!selector)("x", is.integer)
    )
    summary <- rlang::new_quosure(expression, rlang::base_env())
    detail <- dplyr::summarise(data, !!summary, .by = g)
    subtotal <- dplyr::summarise(data, !!summary)
    subtotal$g <- NA_character_
    expected <- dplyr::bind_rows(detail, subtotal)
    for (kind in c("total", "parent")) {
      helper <- rlang::sym(paste0("share_of_", kind))
      actual <- rlang::inject(summarize_with_margins(
        data, !!summary, total = sum(y),
        dplyr::across(dplyr::starts_with("total"), !!helper,
                      .names = "share"),
        .grouping = rollup(g), .margin_label = NULL
      ))
      expect_equal(actual[names(expected)], expected,
                   info = paste(kind, rlang::as_label(selector)))
      expect_equal(actual$share, c(11 / 18, 7 / 18, 1))
    }
  }
})

test_that("selectors inside an across function keep their runtime scope", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7))
  selectors <- rlang::exprs(
    sum(dplyr::across(dplyr::all_of(cols), identity)),
    sum(dplyr::pick(dplyr::all_of(cols))),
    sum(dplyr::if_any(dplyr::all_of(cols), function(x) x > 2)),
    sum(dplyr::if_all(dplyr::all_of(cols), function(x) x > 2)),
    sum(dplyr::across(dplyr::where(predicate), identity))
  )
  for (selector in selectors) {
    for (outer in c("absent", "conflicting")) {
      env <- rlang::env(rlang::base_env())
      if (outer == "conflicting") {
        env$cols <- "y"
        env$predicate <- is.double
      }
      expression <- rlang::expr(dplyr::across(
        y, (function(cols, predicate) function(z) !!selector)("x", is.integer),
        .names = "ordinary"
      ))
      summary <- rlang::new_quosure(expression, env)
      expected <- dplyr::summarise(data, !!summary, .by = g)
      subtotal <- dplyr::summarise(data, !!summary)
      expected_values <- c(expected$ordinary, subtotal$ordinary)
      for (kind in c("total", "parent")) {
        helper <- rlang::sym(paste0("share_of_", kind))
        actual <- rlang::inject(summarize_with_margins(
          data, !!summary, total = sum(y),
          dplyr::across(dplyr::starts_with("total"), !!helper,
                        .names = "share"),
          .grouping = rollup(g), .margin_label = NULL
        ))
        expect_identical(actual$ordinary, expected_values,
                         info = rlang::as_label(selector))
        expect_equal(actual$share, c(11 / 18, 7 / 18, 1))
      }
    }
  }
})

test_that("nested selectors retain across function and selection counts", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7))
  run <- function(mode) {
    state <- new.env(parent = emptyenv())
    state$selections <- integer()
    state$factories <- integer()
    summary <- rlang::quo(dplyr::across(
      y, (function(cols) {
        state$factories <- c(state$factories, dplyr::n())
        choose_cols <- function() {
          state$selections <- c(state$selections, dplyr::n())
          cols
        }
        function(z) {
          sum(dplyr::across(dplyr::all_of(choose_cols()), identity))
        }
      })("x"), .names = "ordinary"
    ))
    if (mode == "dplyr") {
      detail <- dplyr::summarise(data, !!summary, .by = g)
      subtotal <- dplyr::summarise(data, !!summary)
      subtotal$g <- NA_character_
      result <- dplyr::bind_rows(detail, subtotal)
    } else {
      result <- summarize_with_margins(
        data, !!summary, total = sum(y),
        dplyr::across(dplyr::starts_with("total"), share_of_total,
                      .names = "share"),
        .grouping = rollup(g), .margin_label = NULL
      )
    }
    list(result = result, selections = state$selections,
         factories = state$factories)
  }
  expected <- run("dplyr")
  actual <- run("margin")
  expect_identical(expected$result$ordinary, c(3L, 3L, 6L))
  expect_equal(actual$result[names(expected$result)], expected$result)
  expect_identical(actual$selections, expected$selections)
  expect_identical(actual$factories, expected$factories)
})

test_that("nested selectors retain dynamically named share sources", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7))
  output_name <- function() "total"
  actual <- summarize_with_margins(
    data,
    dplyr::across(y, function(z) sum(dplyr::pick(x)), .names = output_name()),
    share = share_of_total(total), .grouping = rollup(g)
  )
  expect_identical(actual$total, c(3L, 3L, 6L))
  expect_identical(actual$share, c(0.5, 0.5, 1))

  output_name <- function() "total_extra"
  actual <- summarize_with_margins(
    data,
    dplyr::across(y, function(z) sum(dplyr::pick(x)), .names = output_name()),
    total = sum(y),
    dplyr::across(dplyr::starts_with("total"), share_of_total,
                  .names = "{.col}_share"),
    .grouping = rollup(g)
  )
  expect_identical(actual$total_extra_share, c(0.5, 0.5, 1))
  expect_equal(actual$total_share, c(11 / 18, 7 / 18, 1))
})

test_that("nested selectors keep injected environments and key exclusions", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = c(5, 6, 7))
  captured <- rlang::env(rlang::current_env(), cols = "y")
  selected <- rlang::new_quosure(quote(dplyr::all_of(cols)), captured)
  summary <- rlang::quo(dplyr::across(
    y, (function(cols) function(z) sum(dplyr::pick(!!selected)))("x"),
    .names = "ordinary"
  ))
  actual <- summarize_with_margins(
    data, !!summary, total = sum(x),
    dplyr::across(dplyr::starts_with("total"), share_of_total,
                  .names = "share"), .grouping = rollup(g)
  )
  expect_identical(actual$ordinary, c(11, 7, 18))

  expect_error(summarize_with_margins(
    data, dplyr::across(y, (function(cols) {
      function(z) sum(dplyr::pick(dplyr::all_of(cols)))
    })("g"), .names = "ordinary"),
    total = sum(x),
    dplyr::across(dplyr::starts_with("total"), share_of_total,
                  .names = "share"), .grouping = rollup(g)
  ), "doesn't exist")
})
