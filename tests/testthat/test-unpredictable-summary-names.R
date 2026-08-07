# `unpredictable_summary_dots()` is the precondition of every adapter
# result-name test: those tests reach an adapter's own guard only because the
# pre-execution check could not name the output first. A helper that quietly
# stopped delivering that would leave its callers green and pointed at the
# wrong check, so the helper is held to the same standard it enforces.

test_that("the dots produce the output names the helper claims", {
  data <- data.frame(group = c("x", "x", "y"), value = c(1, 2, 3))

  from_fn <- rlang::inject(summarize_with_margins(
    data,
    !!!unpredictable_summary_dots("total"),
    .grouping = rollup(group)
  ))
  expect_identical(names(from_fn), c("group", "total"))

  from_col_fn <- rlang::inject(summarize_with_margins(
    data,
    !!!unpredictable_summary_dots("total", .names = NULL),
    .grouping = rollup(group)
  ))
  expect_identical(names(from_col_fn), c("group", "value_total"))

  expect_equal(from_fn$total, c(3, 3, 6))
  expect_equal(from_col_fn$value_total, c(3, 3, 6))
})

test_that("the helper refuses dots the predictor can name", {
  # Driving the failing branch needs a predictor that reports the name, since
  # producing one through the ordinary interface is precisely what this helper
  # is written to prevent.
  expect_error(
    unpredictable_summary_dots(
      "total",
      predict = function(dots, proxy) "total"
    ),
    "predicts `total`"
  )

  # Nothing is claimed when the predictor reports some other name.
  expect_no_error(
    unpredictable_summary_dots(
      "total",
      predict = function(dots, proxy) "something_else"
    )
  )
})

test_that("the predictor does name the literal shape the helper avoids", {
  # Without this, the guard above could pass while asserting nothing: a
  # predictor that named no output at all would accept every shape, including
  # the ones that reach the pre-execution check instead of an adapter. The
  # difference the helper trades on is `.fns` as a variable rather than as a
  # literal `list()`, so the literal has to be visible for the variable's
  # invisibility to mean anything.
  proxy <- data.frame(value = double())
  literal <- list(rlang::quo(dplyr::across(
    dplyr::all_of("value"),
    list(total = sum),
    .names = "{.fn}"
  )))

  expect_true("total" %in% known_summary_output_names(literal, proxy))
})
