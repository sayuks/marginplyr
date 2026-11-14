test_that("an unrecognised input reads context names from its names", {
  data <- tibble::tibble(region = c("E", "W"), value = 1:2)
  input <- other_backend_input(data)
  expect_identical(grouping_backend(input)$kind, "other")
  selections <- rlang::quos(
    tidyselect::peek_vars(),
    names(tidyselect::peek_data())
  )
  for (selection in selections) {
    expect_identical(
      inspect_grouping(input, .grouping = grouping_set(!!selection)),
      inspect_grouping(data, .grouping = grouping_set(region, value))
    )
  }
})
