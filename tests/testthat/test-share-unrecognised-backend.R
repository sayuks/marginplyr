test_that("an unrecognised backend joins share denominators on missing keys", {
  data <- tibble::tibble(
    f = c(NA, NA, "P"), g = c("a", "b", "a"), x = c(1, 3, 4)
  )
  source <- other_backend_input(data)
  expect_identical(grouping_backend(source)$kind, "other")
  summarize <- function(data, ...) {
    summarize_with_margins(
      data, value = sum(x), parent = share_of_parent(value),
      total = share_of_total(value), .grouping = rollup(f, g), ...
    )
  }
  expected <- summarize(data)
  # Its dialect cannot be asked whether it refuses a non-numeric source.
  actual <- dplyr::collect(summarize(source, .check_share_source = FALSE))
  expect_equal(actual, expected)
  expect_equal(actual$parent[is.na(actual$f) & actual$g != "Total"],
               c(0.25, 0.75))
})
