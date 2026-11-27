# On the right of a magrittr pipe a pair around a function value is what makes
# the pipe apply it rather than insert `.` into it, so a Margin summary returns
# what `dplyr::summarise()` returns at each grain (#1826).
expect_pipe_value <- function(input, expr) {
  native <- dplyr::summarise(dplyr::collect(input), n = !!expr)$n
  grouped <- dplyr::summarise(
    dplyr::collect(input), n = !!expr, .by = dplyr::all_of("g")
  )$n
  collected <- function(...) {
    dplyr::collect(summarize_with_margins(input, n = !!expr, ...))$n
  }
  expect_identical(collected(.grouping = NULL), native)
  expect_identical(
    collected(.by = dplyr::all_of("g"), .sort = "last"), grouped
  )
  expect_identical(
    collected(.grouping = rollup(dplyr::all_of("g")), .sort = "last"),
    c(grouped, native)
  )
}

# magrittr fixes the pipes' names and their infix spellings are what is under
# test; `v` is a column the summary's data mask resolves.
# nolint start: object_usage_linter, pipe_consistency_linter.
pipe_rhs_cases <- function() {
  `%>%` <- magrittr::`%>%`
  `%T>%` <- magrittr::`%T>%` # nolint: object_name_linter.
  list(
    rlang::quo(v %>% (function(x) sum(x))),
    rlang::quo(v %>% (\(x) sum(x))),
    rlang::quo(v %>% (base::sum)),
    # jarl-ignore internal_function: The `:::` spelling is the case under test.
    rlang::quo(v %>% (base:::sum)),
    rlang::quo(magrittr::`%>%`(v, (function(x) sum(x)))),
    rlang::quo(sum(v %T>% (function(x) invisible(x)))),
    rlang::quo(sum(v %>% (function(x) x * 2))),
    rlang::quo(sum(sapply(v, function(y) y %>% (base::sum))))
  )
}
# nolint end

pipe_rhs_input <- function() {
  data.frame(g = c("a", "a", "b"), v = c(1, 2, 4))
}

test_that("a parenthesized function on a pipe's right keeps its pair", {
  input <- pipe_rhs_input()
  for (expr in pipe_rhs_cases()) {
    expect_pipe_value(input, expr)
    expect_pipe_value(tibble::as_tibble(input), expr)
  }
})

# The infix pipe is under test, and `v` is a column of the summary's data mask.
# nolint start: object_usage_linter, pipe_consistency_linter.
test_that("the pair survives inside an across() lambda body", {
  `%>%` <- magrittr::`%>%`
  input <- tibble::as_tibble(pipe_rhs_input())
  expr <- rlang::quo(
    dplyr::across(v, ~ .x %>% (function(x) sum(x)), .names = "n")
  )
  native <- dplyr::summarise(input, !!expr)
  actual <- summarize_with_margins(input, !!expr, .grouping = NULL)
  expect_identical(actual$n, native$n)
})
# nolint end

test_that("a pipe's parenthesized function keeps its pair on data.table", {
  skip_if_suggest_absent("data.table")
  input <- data.table::as.data.table(pipe_rhs_input())
  for (expr in pipe_rhs_cases()) {
    expect_pipe_value(input, expr)
  }
})

# dtplyr resolves a pipe and a call in a function body on the search path, and
# refuses a qualified name on a pipe's right natively, so the function literals
# are what it can show.
test_that("a pipe's parenthesized function keeps its pair on dtplyr", {
  skip_if_suggest_absent("dtplyr")
  withr::local_package("dplyr")
  withr::local_package("magrittr")
  input <- dtplyr::lazy_dt(pipe_rhs_input())
  for (expr in pipe_rhs_cases()[c(1L, 2L, 5L, 6L, 7L)]) {
    expect_pipe_value(input, expr)
  }
})
