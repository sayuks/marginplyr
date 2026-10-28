test_that("a parenthesized across formula remains a Total-share source", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  actual <- summarize_with_margins(
    data, dplyr::across(x, (~sum(.x)), .names = "s"),
    share = share_of_total(s), .grouping = rollup(g)
  )
  expect_identical(actual$s, c(1, 3, 4))
  expect_identical(actual$share, c(0.25, 0.75, 1))
})

expect_across_source_shares <- function(
  data,
  functions = alist(~sum(.x), sum, fn, base::sum, function(x) sum(x))) {
  fn <- sum
  for (fns in functions) {
    for (depth in 0:2) {
      wrapped <- fns
      for (i in seq_len(depth)) wrapped <- rlang::call2("(", wrapped)
      for (head in list(quote(across), quote(dplyr::across))) {
        for (template in list("s", "{.col}_sum", NULL)) {
          output <- if (is.null(template)) {
            "x"
          } else if (template == "s") {
            "s"
          } else {
            "x_sum"
          }
          ordinary <- rlang::call2(head, quote(x), wrapped, .names = template)
          source <- rlang::sym(output)
          query <- summarize_with_margins(
            data, !!ordinary,
            total = share_of_total(!!source),
            parent = share_of_parent(!!source),
            .grouping = rollup("g"), .check_share_source = FALSE
          )
          if (inherits(data, "tbl_lazy")) expect_s3_class(query, "tbl_lazy")
          if (inherits(data, "dtplyr_step")) {
            expect_s3_class(query, "dtplyr_step")
          }
          actual <- dplyr::collect(query)
          actual <- actual[order(actual[[output]]), ]
          expect_identical(names(actual), c("g", output, "total", "parent"))
          expect_equal(actual[[output]], c(1, 3, 4))
          expect_identical(actual$total, c(0.25, 0.75, 1))
          expect_identical(actual$parent, c(0.25, 0.75, 1))
        }
      }
    }
  }
}

test_that("single across functions retain local share source names", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  expect_across_source_shares(data)
  expect_across_source_shares(tibble::as_tibble(data))
})

test_that("single across functions retain dtplyr share source names", {
  skip_if_suggest_absent("dtplyr")
  data <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), x = c(1, 3)))
  # dtplyr does not evaluate parenthesized formulas as ordinary functions.
  expect_across_source_shares(
    data, alist(sum, base::sum, function(x) sum(x))
  )
})

test_that("single across functions retain SQLite share source names", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- dplyr::copy_to(con, data.frame(g = c("a", "b"), x = c(1, 3)))
  # SQL translation does not resolve a caller-bound function alias.
  expect_across_source_shares(
    data, alist(~sum(.x), sum, base::sum, function(x) sum(x))
  )
})

test_that("single across functions retain DuckDB share source names", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- dplyr::copy_to(con, data.frame(g = c("a", "b"), x = c(1, 3)))
  # The native path preserves names for these ordinary function spellings.
  expect_across_source_shares(
    data, alist(base::sum, function(x) sum(x))
  )
})

# jarl-ignore unnecessary_parentheses: Parenthesized prediction is under test.
test_that("parenthesized across prediction does not run caller code", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  observed <- new.env(parent = emptyenv())
  observed$calls <- 0L
  factory <- function() {
    observed$calls <- observed$calls + 1L
    sum
  }
  expect_error(summarize_with_margins(
    data, dplyr::across(x, ((factory())), .names = "s"),
    share = share_of_total(s), .grouping = rollup(g)
  ), "unknown preceding ordinary summary", class = "marginplyr_error")
  expect_identical(observed$calls, 0L)

  delayedAssign("fn", factory())
  expect_error(summarize_with_margins(
    data, dplyr::across(x, ((fn)), .names = "s"),
    share = share_of_parent(s), .grouping = rollup(g)
  ), "unknown preceding ordinary summary", class = "marginplyr_error")
  expect_identical(observed$calls, 0L)

  makeActiveBinding("active_fn", factory, environment())
  expect_error(summarize_with_margins(
    data, dplyr::across(x, ((active_fn)), .names = "s"),
    share = share_of_total(s), .grouping = rollup(g)
  ), "unknown preceding ordinary summary", class = "marginplyr_error")
  expect_identical(observed$calls, 0L)
})

test_that("parenthesized across sources keep eligibility checks", {
  data <- data.frame(g = c("a", "b"), x = c(1, 3))
  for (helper in c("share_of_total", "share_of_parent")) {
    share <- rlang::call2(helper, quote(s))
    expect_error(summarize_with_margins(
      data, dplyr::across(x, (~sum(.x)), .names = "s"), s = sum(x),
      share = !!share, .grouping = rollup(g)
    ), "defined exactly once", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      data, share = !!share,
      dplyr::across(x, (~sum(.x)), .names = "s"), .grouping = rollup(g)
    ), "forward reference", class = "marginplyr_error")
    expect_error(summarize_with_margins(
      data, dplyr::across(x, (~as.character(sum(.x))), .names = "s"),
      share = !!share, .grouping = rollup(g)
    ), "plain integer or double scalar", class = "marginplyr_error")
    expect_error(suppressWarnings(summarize_with_margins(
      data, dplyr::across(x, (~rep(sum(.x), 2L)), .names = "s"),
      share = !!share, .grouping = rollup(g)
    )), "exactly one value per grouping row", class = "marginplyr_error")
  }
})
