# Both ownership boundaries retain local and renamed-input controls, and refuse
# ambiguous input reads through each public share/source spelling.
expect_share_ownership_refusal <- function(expressions, message) {
  # dtplyr imports data.table; only one optional backend is required.
  input <- data.table::data.table(g = c("a", "b"), .BY = c(1, 3), x = c(2, 6))
  before <- data.table::copy(input)
  for (expr in expressions) {
    for (helper in c("share_of_parent", "share_of_total")) {
      for (style in c("direct", "across")) {
        dots <- if (style == "direct") {
          rlang::exprs(value = sum(!!rlang::sym("x")))
        } else {
          rlang::exprs(dplyr::across(dplyr::all_of("x"), sum, .names = "value"))
        }
        summarize <- function(source) {
          summarize_with_margins(
            source, !!!dots, independent = !!expr,
            fraction = !!rlang::call2(helper, quote(value)),
            .grouping = rollup("g")
          )
        }
        local <- summarize(tibble::as_tibble(input))
        expect_equal(local$independent, rep(12, 3))
        safe <- dplyr::rename(
          tibble::as_tibble(input), measure = dplyr::all_of(".BY")
        )
        control <- dplyr::collect(summarize(dtplyr::lazy_dt(safe)))
        expect_equal(control$independent, rep(12, 3))
        expect_equal(control$value, c(2, 6, 8))
        expect_identical(control$fraction, c(0.25, 0.75, 1))
        expect_error(summarize(dtplyr::lazy_dt(input)),
                     message, class = "marginplyr_error")
        expect_identical(input, before)
      }
    }
  }
}

test_that("string and indirect assignments cannot become input-measure reads", {
  skip_if_suggest_absent("dtplyr")
  expressions <- lapply(c(
    'local({".BY" <- c(5, 7); sum(.BY)})',
    'local({c(5, 7) -> ".BY"; sum(.BY)})',
    '(function() {".BY" <- c(5, 7); sum(.BY)})()',
    paste0('local({do.call("<-", list(as.name(".BY"), c(5, 7)), ',
           "envir = environment()); sum(.BY)})"),
    'local({(get("assign"))(".BY", c(5, 7)); sum(.BY)})',
    'local({(match.fun("assign"))(".BY", c(5, 7)); sum(.BY)})'
  ), rlang::parse_expr)
  expect_share_ownership_refusal(expressions, "input and lexical")
})

test_that("explicit evaluation masks cannot become input-measure reads", {
  skip_if_suggest_absent("dtplyr")
  expressions <- lapply(c(
    "eval(quote(sum(.BY)), list(.BY = c(5, 7)))",
    "evalq(sum(.BY), list(.BY = c(5, 7)))",
    "with(list(.BY = c(5, 7)), sum(.BY))"
  ), rlang::parse_expr)
  expect_share_ownership_refusal(expressions, "explicit evaluation")
})

test_that("ownership analysis does not execute caller code", {
  skip_if_suggest_absent("dtplyr")
  input <- tibble::tibble(g = c("a", "b"), .BY = c(1, 3), x = c(2, 6))
  observed <- new.env(parent = emptyenv())
  observed$calls <- 0L
  target <- function() {
    observed$calls <- observed$calls + 1L
    "assign"
  }
  mask <- function() {
    observed$calls <- observed$calls + 1L
    list(.BY = c(5, 7))
  }
  expressions <- lapply(c(
    'local({(get(target()))(".BY", c(5, 7)); sum(.BY)})',
    '(function() {(get(target()))(".BY", c(5, 7)); sum(.BY)})()',
    "eval(quote(sum(.BY)), mask())",
    "evalq(sum(.BY), envir = mask())",
    "with(data = mask(), expr = sum(.BY))"
  ), rlang::parse_expr)
  for (expr in expressions) {
    error <- expect_error(
      summarize_with_margins(
        dtplyr::lazy_dt(input), value = sum(x), independent = !!expr,
        fraction = share_of_total(value), .grouping = rollup(g)
      ),
      "cannot safely distinguish", class = "marginplyr_error"
    )
    expect_identical(conditionCall(error)[[1L]], quote(summarize_with_margins))
    expect_identical(observed$calls, 0L)
  }
})
