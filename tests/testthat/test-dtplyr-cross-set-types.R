# A summary that is a bare `NA` in some grouping sets takes, on dtplyr, the
# common type a local data frame gives it; the other value is integer, double
# or character in turn.
test_that("dtplyr combines a bare NA with a typed summary across sets", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(g = c("a", "b", "c"), v = 1:3)
  cases <- list(
    integer = list(value = quote(sum(v)), expected = c(1L, 2L, 3L, NA)),
    double = list(value = quote(mean(v)), expected = c(1, 2, 3, NA)),
    character = list(
      value = quote(as.character(sum(v))), expected = c("1", "2", "3", NA)
    )
  )
  for (case in cases) {
    results <- lapply(list(dtplyr::lazy_dt(data), data), function(input) {
      as.data.frame(dplyr::collect(summarize_with_margins(
        input,
        s = if (grouping_bit(g) == 1L) NA else !!case$value,
        .grouping = rollup(g)
      )))
    })
    expect_identical(results[[1L]]$g, c("a", "b", "c", "Total"))
    expect_identical(results[[1L]]$s, case$expected)
    expect_identical(results[[1L]], results[[2L]])
  }
})

test_that("dtplyr combines a bare NA across sets under .by and .sort", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(
    k = c("p", "p", "q"), g = c("a", "b", "a"), v = c(1L, 2L, 4L)
  )
  result <- dplyr::collect(summarize_with_margins(
    dtplyr::lazy_dt(data),
    s = if (grouping_bit(g) == 1L) NA else sum(v),
    .by = k, .grouping = rollup(g), .sort = "last"
  ))
  expect_identical(result$k, c("p", "p", "p", "q", "q"))
  expect_identical(result$g, c("a", "b", "Total", "a", "Total"))
  expect_identical(result$s, c(1L, 2L, NA, 4L, NA))
})

test_that("dtplyr refuses summaries with no common type across sets", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(g = c("a", "b"), v = 1:2)
  query <- summarize_with_margins(
    dtplyr::lazy_dt(data),
    s = if (grouping_bit(g) == 1L) "none" else sum(v),
    .grouping = rollup(g)
  )
  expect_error(
    dplyr::collect(query),
    "Summary `s` has no common type across grouping sets",
    class = "marginplyr_error"
  )
})
