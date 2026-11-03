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
