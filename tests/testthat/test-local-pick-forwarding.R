test_that("summary-local pick captures numbered dots promises", {
  pick <- dplyr::pick
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = 5:7)
  expressions <- rlang::exprs(
    (function(...) sum(pick(..1)))(x),
    (function(...) sum(dplyr::pick(..2)))(y, x),
    (function(...) sum(dplyr::pick(..1, ..2)))(x, y),
    (function(...) sum(dplyr::pick(...)))(x, y)
  )
  for (expression in expressions) {
    for (named in c(FALSE, TRUE)) {
      for (preceded in c(FALSE, TRUE)) {
        dots <- if (preceded) rlang::exprs(prior = sum(y)) else list()
        dots <- c(dots, stats::setNames(
          list(expression), if (named) "value" else ""
        ))
        expected <- dplyr::summarise(data, !!!dots, .by = g)
        actual <- summarize_with_margins(data, !!!dots, .by = g)
        expect_identical(actual, expected, info = paste(
          rlang::as_label(expression), named, preceded
        ))
      }
    }
  }
})

test_that("numbered pick forwarding retains selection scope across margins", {
  data <- data.frame(g = c("a", "a", "b"), x = 1:3, y = 5:7)
  expressions <- rlang::exprs(
    (function(...) sum(dplyr::pick(..1)))(.env$column),
    (function(...) sum(dplyr::pick(..2)))(stop("unused dot"), x),
    (function(...) {
      (function(...) sum(dplyr::pick(..1)))(..2)
    })(y, x)
  )
  column <- "y"
  for (expression in expressions) {
    expected <- dplyr::bind_rows(
      dplyr::summarise(data, value = !!expression, .by = g),
      dplyr::mutate(dplyr::summarise(data, value = !!expression), g = "Total")
    )
    actual <- summarize_with_margins(
      data, value = !!expression, total = sum(x),
      share = share_of_total(total), .grouping = rollup(g)
    )
    expect_identical(actual[names(expected)], expected)
  }
  expect_error(summarize_with_margins(
    data, value = (function(...) sum(dplyr::pick(..2)))(x, absent)
  ), "Column `absent` doesn't exist", fixed = TRUE)
  expect_error(summarize_with_margins(
    data, value = (function(...) sum(dplyr::pick(..1)))("g"),
    .grouping = rollup(g)
  ), "Column `g` doesn't exist", fixed = TRUE)
})
