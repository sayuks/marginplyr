test_that("list-matrix missing labels match ordinary row construction", {
  key <- matrix(list(7L), nrow = 1L)
  input <- tibble::tibble(key = key, payload = 19L)
  ordinary <- dplyr::union_all(
    input, dplyr::mutate(input, key = vctrs::vec_init(key, 1L))
  )

  expect_identical(dim(ordinary$key), c(2L, 1L))
  expect_identical(ordinary$key[[1L]], 7L)
  expect_null(ordinary$key[[2L]])
  expect_identical(ordinary$payload, c(19L, 19L))
  expect_identical(
    expand_with_margins(input, .grouping = rollup(key),
                        .margin_label = NULL, .sort = "none"),
    ordinary
  )
})
