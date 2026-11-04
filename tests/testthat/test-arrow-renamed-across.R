test_that("Arrow across reads the source of a renamed selection", {
  skip_if_suggest_absent("arrow")
  source <- arrow::Table$create(g = c("a", "b"), x = 1:2, y = 10:11)
  result <- summarize_with_margins(
    source, dplyr::across(c(y = x), sum), .by = g
  )
  expect_s3_class(result, "arrow_dplyr_query")
  actual <- dplyr::arrange(dplyr::collect(result), g)
  expect_named(actual, c("g", "y"))
  expect_equal(actual$y, c(1, 2))
})

test_that("Arrow renamed selections keep their sources through a rollup", {
  skip_if_suggest_absent("arrow")
  source <- arrow::Table$create(
    k = "fixed", g = c("a", "b"), x = 1:2, y = 10:11, z = 100:101
  )
  selected <- c(y = "x")
  summaries <- rlang::exprs(
    dplyr::across(c(y = x), sum),
    dplyr::across(dplyr::all_of(selected), sum),
    dplyr::across(c(y = x, -z), sum),
    dplyr::across(x, sum, .names = "y")
  )
  for (input in list(source, dplyr::filter(source, x > 0L))) {
    for (summary in summaries) {
      result <- summarize_with_margins(
        input, !!summary, .by = k, .grouping = rollup(g)
      )
      expect_s3_class(result, "arrow_dplyr_query")
      actual <- dplyr::collect(result)
      expect_named(actual, c("k", "g", "y"))
      expect_equal(
        dplyr::arrange(actual, y),
        tibble::tibble(k = "fixed", g = c("a", "b", "Total"), y = 1:3)
      )
    }
  }
})

test_that("Arrow names functions using the selection aliases", {
  skip_if_suggest_absent("arrow")
  source <- arrow::Table$create(g = "a", x = 1:2, y = 10:11)
  default <- summarize_with_margins(
    source,
    dplyr::across(c(y = x, x = y), list(total = sum, min)),
    .by = g
  )
  expect_s3_class(default, "arrow_dplyr_query")
  expect_equal(
    dplyr::collect(default),
    tibble::tibble(g = "a", y_total = 3L, y_2 = 1L, x_total = 21L, x_2 = 10L)
  )
  custom <- summarize_with_margins(
    source,
    dplyr::across(c(y = x), list(total = sum, min), .names = "{.fn}_{.col}"),
    .by = g
  )
  expect_s3_class(custom, "arrow_dplyr_query")
  expect_equal(
    dplyr::collect(custom),
    tibble::tibble(g = "a", total_y = 3L, `2_y` = 1L)
  )
})

test_that("Arrow diagnoses renamed selections it cannot preserve", {
  skip_if_suggest_absent("arrow")
  source <- arrow::Table$create(g = c("a", "b"), x = 1:2, y = 10:11)
  expect_error(
    summarize_with_margins(
      source, dplyr::across(c(y = x, other = x), sum), .by = g
    ),
    "selects an input column more than once",
    class = "marginplyr_error"
  )
  expect_error(
    summarize_with_margins(
      source, dplyr::across(c(y = x), c(sum)), .by = g
    ),
    "cannot determine the output names",
    class = "marginplyr_error"
  )
})
