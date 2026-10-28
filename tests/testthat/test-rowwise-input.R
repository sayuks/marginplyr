test_that("named rowwise inputs exclude enclosing grouping columns", {
  data <- tibble::tibble(g = "a", x = 1)
  actual <- summarize_with_margins(
    data,
    width = ncol(dplyr::rowwise(data = dplyr::pick(dplyr::everything()))),
    .grouping = grouping_set(g)
  )
  expect_identical(actual$width, 1L)
})

expect_rowwise_enclosing_scope <- function(data) {
  selections <- rlang::exprs(
    dplyr::pick(dplyr::everything()),
    dplyr::across(dplyr::everything())
  )
  specs <- list(single = grouping_set("g"), rollup = rollup("g"))
  for (name in names(specs)) {
    spec <- specs[[name]]
    rows <- if (name == "single") 2L else 4L
    for (input_name in c("", "data", "d", "da", "dat")) {
      for (selection in selections) {
        nested <- rlang::call2(
          "rowwise", !!!stats::setNames(list(selection), input_name),
          .ns = "dplyr"
        )
        actual <- summarize_with_margins(
          data, width = ncol(!!nested), .by = "fixed", .grouping = spec
        )
        expect_identical(actual$width, rep(1L, rows), info = input_name)
      }
      input <- quote(tibble::tibble(
        g = grouping_bit(g), id = marginplyr::grouping_id(g),
        dplyr::pick(dplyr::everything())
      ))
      nested <- rlang::call2(
        "rowwise", !!!stats::setNames(list(input), input_name),
        rlang::expr(dplyr::everything()), .ns = "dplyr"
      )
      actual <- summarize_with_margins(
        data, nested = list(!!nested), .by = "fixed", .grouping = spec,
        .id = "set"
      )
      for (i in seq_len(nrow(actual))) {
        inner <- actual$nested[[i]]
        expected_bit <- actual$set[[i]] - 1L
        expect_identical(names(inner), c("g", "id", "x"))
        expect_identical(dplyr::group_vars(inner), c("g", "id", "x"))
        expect_identical(inner$g, expected_bit)
        expect_identical(inner$id, expected_bit)
        expect_identical(inner$x, if (actual$fixed[[i]] == "east") 1 else 3)
      }
    }
  }
}

test_that("rowwise inputs share the enclosing plan on local frames", {
  data <- data.frame(fixed = c("east", "west"), g = c("a", "b"), x = c(1, 3))
  expect_rowwise_enclosing_scope(data)
  expect_rowwise_enclosing_scope(tibble::as_tibble(data))
})

test_that("rowwise inputs share the enclosing plan on data.table inputs", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(
    fixed = c("east", "west"), g = c("a", "b"), x = c(1, 3)
  )
  expect_rowwise_enclosing_scope(data)
})
