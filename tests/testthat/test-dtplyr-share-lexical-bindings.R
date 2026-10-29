test_that("dtplyr shares preserve a lexical .BY formal", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), .BY = c(1, 3), x = c(2, 6))
  expr <- rlang::parse_expr("(function(.BY) sum(.BY))(c(5, 7))")
  ordinary <- dplyr::summarise(
    dtplyr::lazy_dt(input), value = sum(x),
    independent = !!expr, .by = g
  )
  expect_error(dplyr::collect(ordinary), "locked binding.*'.BY'")
  for (helper in c("share_of_parent", "share_of_total")) {
    summarize <- function(source) {
      summarize_with_margins(
        source, value = sum(x),
        independent = !!expr,
        fraction = !!rlang::call2(helper, quote(value)),
        .grouping = rollup(g)
      )
    }
    expect_equal(summarize(input)$independent, rep(12, 3))
    query <- summarize(dtplyr::lazy_dt(input))
    expect_s3_class(query, "dtplyr_step")
    expect_equal(dplyr::collect(query)$independent, rep(12, 3))
  }
})

test_that("evaluated language retains its .BY bindings with dtplyr shares", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), .BY = c(1, 3), x = c(2, 6))
  expr <- rlang::parse_expr(
    'eval(quote(local({assign(".BY", c(5, 7)); sum(.BY)})))'
  )
  expect_error(
    summarize_with_margins(
      dtplyr::lazy_dt(input), value = sum(x), independent = !!expr,
      fraction = share_of_total(value), .grouping = rollup(g)
    ),
    "input and lexical", class = "marginplyr_error"
  )
})

test_that("dtplyr shares retain refusals of .BY binding removal", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), .BY = c(1, 3), x = c(2, 6))
  expressions <- lapply(c(
    "local({rm(.BY); sum(.BY)})",
    'local({remove(".BY"); sum(.BY)})'
  ), rlang::parse_expr)
  for (expr in expressions) {
    ordinary <- dplyr::summarise(
      dtplyr::lazy_dt(input), independent = !!expr, .by = g
    )
    expect_error(dplyr::collect(ordinary), "locked binding.*'.BY'")
    expect_error(
      summarize_with_margins(
        dtplyr::lazy_dt(input), value = sum(x), independent = !!expr,
        fraction = share_of_total(value), .grouping = rollup(g)
      ),
      "input and lexical", class = "marginplyr_error"
    )
  }
})

test_that("dtplyr shares refuse uncertain reflective .BY assignments", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), .BY = c(1, 3), x = c(2, 6))
  target <- ".BY"
  callback <- function() c(5, 7)
  expressions <- lapply(c(
    'local({assign(".BY", c(5, 7)); sum(.BY)})',
    "local({assign(target, c(5, 7)); sum(.BY)})",
    'local({delayedAssign(".BY", c(5, 7)); sum(.BY)})',
    'local({makeActiveBinding(".BY", callback, environment()); sum(.BY)})'
  ), rlang::parse_expr)
  for (expr in expressions) {
    ordinary <- dplyr::summarise(
      dtplyr::lazy_dt(input), independent = !!expr, .by = g
    )
    expect_error(dplyr::collect(ordinary), "locked binding.*'.BY'")
    for (helper in c("share_of_parent", "share_of_total")) {
      expect_error(
        summarize_with_margins(
          dtplyr::lazy_dt(input), value = sum(x), independent = !!expr,
          fraction = !!rlang::call2(helper, quote(value)),
          .grouping = rollup(g)
        ),
        "input and lexical", class = "marginplyr_error"
      )
    }
  }
})

test_that("dtplyr shares preserve .BY ownership in function defaults", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), .BY = c(1, 3), x = c(2, 6))
  direct <- rlang::parse_expr("(function(.BY = c(5, 7)) sum(.BY))()")
  forwarded <- rlang::parse_expr(
    "(function(.BY = c(5, 7), y = .BY) sum(y))()"
  )
  for (helper in c("share_of_parent", "share_of_total")) {
    direct_query <- summarize_with_margins(
      dtplyr::lazy_dt(input), value = sum(x),
      independent = !!direct,
      fraction = !!rlang::call2(helper, quote(value)), .grouping = rollup(g)
    )
    expect_equal(dplyr::collect(direct_query)$independent, rep(12, 3))
    supported <- summarize_with_margins(
      dtplyr::lazy_dt(input), value = sum(x),
      independent = !!forwarded,
      fraction = !!rlang::call2(helper, quote(value)), .grouping = rollup(g)
    )
    actual <- dplyr::collect(supported)
    expect_equal(actual$independent, rep(12, 3))
    expect_equal(actual$value, c(2, 6, 8))
    expect_identical(actual$fraction, c(0.25, 0.75, 1))
  }
})

test_that("dtplyr shares preserve local .BY binding statements", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), .BY = c(1, 3), x = c(2, 6))
  expressions <- lapply(c(
    "local({.BY <- c(5, 7); sum(.BY)})",
    "(function() {.BY <- c(5, 7); sum(.BY)})()"
  ), rlang::parse_expr)
  for (expr in expressions) {
    for (helper in c("share_of_parent", "share_of_total")) {
      query <- summarize_with_margins(
        dtplyr::lazy_dt(input), value = sum(x), independent = !!expr,
        fraction = !!rlang::call2(helper, quote(value)),
        .grouping = rollup(g)
      )
      expect_equal(dplyr::collect(query)$independent, rep(12, 3))
    }
  }
})

test_that("dtplyr shares retain refusals of data-mask .BY assignments", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), .BY = c(1, 3), x = c(2, 6))
  for (expr in lapply(c(
    "{.BY <- c(5, 7); sum(.BY)}",
    "{for (.BY in c(5, 7)) NULL; .BY}"
  ), rlang::parse_expr)) {
    query <- summarize_with_margins(
      dtplyr::lazy_dt(input), value = sum(x), independent = !!expr,
      fraction = share_of_total(value), .grouping = rollup(g)
    )
    expect_error(dplyr::collect(query), "locked binding.*'.BY'")
  }
})

test_that("captured .BY binding syntax leaves input-measure shares usable", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), .BY = c(1, 3))
  definition <- rlang::parse_expr(
    "(function() is.call(quote(function(.BY) .BY)))()"
  )
  assignment <- rlang::parse_expr("length(quote(.BY <- 12))")
  for (helper in c("share_of_parent", "share_of_total")) {
    query <- summarize_with_margins(
      dtplyr::lazy_dt(input), value = sum(.BY),
      forwarded = (function(y) sum(y))(.BY),
      definition = !!definition,
      assignment = !!assignment,
      fraction = !!rlang::call2(helper, quote(value)), .grouping = rollup(g)
    )
    actual <- dplyr::collect(query)
    expect_equal(actual$value, c(1, 3, 4))
    expect_equal(actual$forwarded, c(1, 3, 4))
    expect_identical(actual$definition, rep(TRUE, 3))
    expect_equal(actual$assignment, rep(3L, 3))
    expect_identical(actual$fraction, c(0.25, 0.75, 1))
  }
})

test_that("unused .BY formals do not block unrelated input-measure shares", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), .BY = c(1, 3))
  independent <- rlang::parse_expr("(function(.BY) 12)(c(5, 7))")
  for (helper in c("share_of_parent", "share_of_total")) {
    for (style in c("direct", "across")) {
      dots <- if (style == "direct") {
        rlang::exprs(value = sum(.BY))
      } else {
        rlang::exprs(dplyr::across(dplyr::all_of(".BY"), sum, .names = "value"))
      }
      query <- summarize_with_margins(
        dtplyr::lazy_dt(input), !!!dots, independent = !!independent,
        fraction = !!rlang::call2(helper, quote(value)), .grouping = rollup(g)
      )
      actual <- dplyr::collect(query)
      expect_equal(actual$value, c(1, 3, 4))
      expect_equal(actual$independent, rep(12, 3))
      expect_identical(actual$fraction, c(0.25, 0.75, 1))
    }
    query <- summarize_with_margins(
      dtplyr::lazy_dt(input), value = sum(.BY) + (!!independent),
      fraction = !!rlang::call2(helper, quote(value)), .grouping = rollup(g)
    )
    actual <- dplyr::collect(query)
    expect_equal(actual$value, c(13, 15, 16))
    expect_identical(actual$fraction, c(13 / 16, 15 / 16, 1))
  }
})

test_that("uncertain .BY bindings cannot read data.table group metadata", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "a", "b", "b"),
                          .BY = c(1, 3, 5, 7), x = c(2, 4, 6, 8))
  observed <- new.env(parent = emptyenv())
  observed$calls <- 0L
  target <- function() {
    observed$calls <- observed$calls + 1L
    ".BY"
  }
  expressions <- lapply(c(
    "local({if (length(x) < 0) .BY <- c(5, 7); length(.BY)})",
    "local({y <- sum(.BY); .BY <- c(5, 7); y + sum(.BY)})",
    "local({assign(target(), c(5, 7)); length(.BY)})"
  ), rlang::parse_expr)
  for (expr in expressions) {
    error <- expect_error(
      summarize_with_margins(
        dtplyr::lazy_dt(input), value = sum(x), independent = !!expr,
        fraction = share_of_total(value), .grouping = rollup(g)
      ),
      "input and lexical", class = "marginplyr_error"
    )
    expect_identical(conditionCall(error)[[1L]], quote(summarize_with_margins))
    expect_identical(observed$calls, 0L)
  }
})
