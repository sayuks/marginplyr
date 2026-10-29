test_that("dtplyr shares retain the refusal of a lexical .BY formal", {
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
    expect_error(dplyr::collect(query), "locked binding.*'.BY'")
  }
})

test_that("evaluated language retains its .BY bindings with dtplyr shares", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), .BY = c(1, 3), x = c(2, 6))
  expr <- rlang::parse_expr(
    'eval(quote(local({assign(".BY", c(5, 7)); sum(.BY)})))'
  )
  query <- summarize_with_margins(
    dtplyr::lazy_dt(input), value = sum(x),
    independent = !!expr,
    fraction = share_of_total(value), .grouping = rollup(g)
  )
  expect_error(dplyr::collect(query), "locked binding.*'.BY'")
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
    query <- summarize_with_margins(
      dtplyr::lazy_dt(input), value = sum(x), independent = !!expr,
      fraction = share_of_total(value), .grouping = rollup(g)
    )
    expect_error(dplyr::collect(query), "locked binding.*'.BY'")
  }
})

test_that("dtplyr shares retain refusals of reflective .BY assignments", {
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
      query <- summarize_with_margins(
        dtplyr::lazy_dt(input), value = sum(x), independent = !!expr,
        fraction = !!rlang::call2(helper, quote(value)),
        .grouping = rollup(g)
      )
      expect_error(dplyr::collect(query), "locked binding.*'.BY'")
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
    refused <- summarize_with_margins(
      dtplyr::lazy_dt(input), value = sum(x),
      independent = !!direct,
      fraction = !!rlang::call2(helper, quote(value)), .grouping = rollup(g)
    )
    expect_error(dplyr::collect(refused), "locked binding.*'.BY'")
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

test_that("dtplyr shares retain refusals of local .BY binding statements", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), .BY = c(1, 3), x = c(2, 6))
  expressions <- lapply(c(
    "{.BY <- c(5, 7); sum(.BY)}",
    "local({.BY <- c(5, 7); sum(.BY)})",
    "(function() {.BY <- c(5, 7); sum(.BY)})()",
    "local({if (sum(x) > 0) .BY <- c(5, 7); sum(.BY)})",
    "{for (.BY in c(5, 7)) NULL; .BY}"
  ), rlang::parse_expr)
  for (expr in expressions) {
    for (helper in c("share_of_parent", "share_of_total")) {
      query <- summarize_with_margins(
        dtplyr::lazy_dt(input), value = sum(x), independent = !!expr,
        fraction = !!rlang::call2(helper, quote(value)),
        .grouping = rollup(g)
      )
      expect_error(dplyr::collect(query), "locked binding.*'.BY'")
    }
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
