# A deparsed closure or a base accessor's `.Internal()` body in a generated
# head is what makes a dtplyr Margin query unreadable.
expect_no_function_body <- function(text) {
  text <- paste(text, collapse = "\n")
  expect_false(grepl("function (x, ns", text, fixed = TRUE))
  expect_false(grepl(".Internal(", text, fixed = TRUE))
}

readable_query_input <- function() {
  data.frame(g = c("a", "b"), x = c(1, 3))
}

test_that("show_query() of a dtplyr Margin summary names its helpers", {
  skip_if_suggest_absent("dtplyr")
  query <- summarize_with_margins(
    dtplyr::lazy_dt(readable_query_input()),
    value = sum(x), ratio = share_of_total(value),
    .grouping = rollup(g)
  )
  text <- utils::capture.output(print(dplyr::show_query(query)))
  expect_no_function_body(text)
  expect_true(any(grepl("union_dtplyr_margin_labels", text, fixed = TRUE)))
})

test_that("show_query() of a dtplyr Margin nest names its helpers", {
  skip_if_suggest_absent("dtplyr")
  query <- nest_with_margins(
    dtplyr::lazy_dt(readable_query_input()),
    .grouping = rollup(g)
  )
  text <- utils::capture.output(print(dplyr::show_query(query)))
  expect_no_function_body(text)
})

test_that("a dtplyr Margin refusal's backtrace carries no function bodies", {
  skip_if_suggest_absent("dtplyr")
  query <- summarize_with_margins(
    dtplyr::lazy_dt(readable_query_input()),
    value = as.character(sum(x)), ratio = share_of_total(value),
    .grouping = rollup(g)
  )
  cnd <- rlang::catch_cnd(dplyr::collect(query), "marginplyr_error")
  expect_s3_class(cnd, "marginplyr_error")
  calls <- vapply(cnd$trace$call, function(call) {
    paste(deparse(call), collapse = "\n")
  }, character(1))
  expect_no_function_body(calls)
})
