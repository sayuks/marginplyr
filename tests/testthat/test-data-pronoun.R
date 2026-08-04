# Generated expressions reference source columns through `.data[["name"]]`
# built directly, not through unquoting inside the pronoun. rlang
# soft-deprecated the latter and signals a lifecycle condition for it, which
# would otherwise reach every caller handling conditions around a Margin verb
# — including one catching `marginplyr_error`.

lifecycle_signals <- function(expr) {
  count <- 0L
  withCallingHandlers(
    force(expr),
    condition = function(cnd) {
      if (inherits(cnd, "lifecycle_stage")) {
        count <<- count + 1L
      }
    }
  )
  count
}

pronoun_data <- function() {
  data.frame(
    g = c("a", "a", "b"),
    h = c("x", "y", "x"),
    v = c(1, 2, 3)
  )
}

test_that("margin_column_pronoun() builds the pronoun call it replaces", {
  expect_equal(
    margin_column_pronoun("region"),
    quote(.data[["region"]])
  )
  expect_equal(lifecycle_signals(margin_column_pronoun("region")), 0L)
})

test_that("Margin verbs signal no lifecycle condition", {
  expect_equal(
    lifecycle_signals(summarize_with_margins(
      pronoun_data(),
      s = sum(v),
      .grouping = rollup(g, h)
    )),
    0L
  )
  expect_equal(
    lifecycle_signals(summarize_with_margins(
      pronoun_data(),
      s = sum(v),
      p = share_of_parent(s),
      .grouping = rollup(g, h)
    )),
    0L
  )
  expect_equal(
    lifecycle_signals(expand_with_margins(
      pronoun_data(),
      .grouping = rollup(g)
    )),
    0L
  )
  expect_equal(
    lifecycle_signals(nest_with_margins(
      pronoun_data(),
      .grouping = rollup(g)
    )),
    0L
  )
})

test_that("results are unchanged by the construction", {
  result <- summarize_with_margins(
    pronoun_data(),
    s = sum(v),
    p = share_of_parent(s),
    .grouping = rollup(g, h)
  )

  expect_equal(result$s, c(1, 2, 3, 3, 3, 6))
  expect_equal(result$p, c(1 / 3, 2 / 3, 1, 0.5, 0.5, 1))
})

test_that("a source column named `.data` does not reach the pronoun", {
  # The pronoun always refers to the mask, so a column of that name is data
  # like any other. It is exercised here because the replacement builds the
  # pronoun symbol itself. `all_of()` is required to select it, exactly as in
  # plain dplyr: a bare `.data` is the pronoun in any tidyselect context.
  data <- data.frame(g = c("a", "b"), .data = c(1, 2), v = c(1, 2))

  result <- summarize_with_margins(
    data,
    s = sum(v),
    .by = dplyr::all_of(".data"),
    .grouping = rollup(g)
  )

  expect_equal(nrow(result), 4L)
  expect_equal(sum(result$s), 6)
})
