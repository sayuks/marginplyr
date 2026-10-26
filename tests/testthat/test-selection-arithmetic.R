# Ordinary arithmetic calls choose b only when selection sees a's real type.
arithmetic_selections <- function() {
  position <- rlang::expr(
    if (is.numeric(tidyselect::peek_data()$a)) 1L else 2L
  )
  rlang::exprs(
    base::`*`(!!position, 1L),
    (`*`)(!!position, 1L),
    base::`^`(!!position, 1L),
    (`^`)(!!position, 1L),
    (base::`*`)(!!position, 1L)
  )
}

test_that("ordinary arithmetic selects fixed keys from input types", {
  data <- tibble::tibble(a = c("x", "x"), b = 1:2)

  for (selection in arithmetic_selections()) {
    spellings <- list(selection, rlang::expr((!!selection)))
    for (spelling in spellings) {
      expect_identical(inspect_grouping(data, .by = !!spelling)$fixed, "(b)")
      result <- summarize_with_margins(data, n = dplyr::n(), .by = !!spelling)
      expect_identical(result, tibble::tibble(b = 1:2, n = c(1L, 1L)))
      expect_identical(
        result,
        dplyr::summarize(data, n = dplyr::n(), .by = !!spelling)
      )
    }
  }
})

test_that("ordinary arithmetic selects typed Grouping dimensions without overlap", {
  data <- tibble::tibble(a = c("x", "x"), b = 1:2)

  for (selection in arithmetic_selections()) {
    inspection <- inspect_grouping(
      data, .by = a, .grouping = rollup(!!selection)
    )
    expect_identical(inspection$fixed, c("(a)", "(a)"))
    expect_identical(inspection$included, c("(b)", "()"))
    result <- summarize_with_margins(
      data,
      n = dplyr::n(),
      .by = a,
      .grouping = rollup(!!selection),
      .margin_label = NULL
    )
    expect_identical(
      result,
      tibble::tibble(a = c("x", "x", "x"), b = c(1L, 2L, NA_integer_),
                     n = c(1L, 1L, 2L))
    )
  }
})
