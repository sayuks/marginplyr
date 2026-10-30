test_that("one-dimensional table expansion uses a typed-missing row", {
  key <- table(c("a", "b", "b"))
  input <- tibble::tibble(key = key, value = 1:2)
  control <- dplyr::union_all(
    input, dplyr::mutate(input, key = vctrs::vec_init(key, 2L))
  )

  expect_identical(
    expand_with_margins(input, .grouping = grouping_set(key),
                        .margin_label = NULL),
    input
  )
  result <- expand_with_margins(
    input, .grouping = rollup(key), .margin_label = NULL, .sort = "none"
  )
  expect_identical(result, control)
  expect_identical(class(result$key), "table")
  expect_identical(dim(result$key), 4L)
  expect_identical(as.integer(result$key), c(1L, 2L, NA_integer_, NA_integer_))
  expect_identical(result$value, c(1L, 2L, 1L, 2L))
})

test_that("table expansion retains Grouping-set order and occurrences", {
  keys <- list(table(character()), table(rep("a", 3L)),
               table(c("a", "b", "b", "c")))
  plans <- list(
    list(spec = rollup(key), included = c(TRUE, FALSE)),
    list(spec = grouping_sets(grouping_set(), grouping_set(key)),
         included = c(FALSE, TRUE)),
    list(spec = grouping_sets(grouping_set(key), grouping_set(),
                              grouping_set(key), grouping_set()),
         included = c(TRUE, FALSE, TRUE, FALSE))
  )
  for (key in keys) {
    size <- length(key)
    input <- tibble::tibble(key = key, value = seq_len(size))
    for (plan in plans) {
      for (id in list(NULL, "set")) {
        branches <- lapply(seq_along(plan$included), function(i) {
          branch <- if (plan$included[[i]]) input else dplyr::mutate(
            input, key = vctrs::vec_init(key, size)
          )
          if (!is.null(id)) branch <- dplyr::mutate(branch, set = i)
          branch
        })
        control <- Reduce(dplyr::union_all, branches)
        if (!is.null(id)) control <- dplyr::relocate(control, set, .after = key)
        for (label in list(NULL, NA_character_)) {
          expect_identical(
            expand_with_margins(input, .grouping = grouping_set(key),
                                .margin_label = label),
            input
          )
          result <- expand_with_margins(
            input, .grouping = plan$spec, .margin_label = label,
            .duplicates = "keep", .id = id, .sort = "none"
          )
          expect_identical(result, control)
          expect_identical(class(result$key), class(control$key))
          expect_identical(dim(result$key), length(plan$included) * size)
          expect_identical(result$value, rep(seq_len(size), length(branches)))
          if (!is.null(id)) {
            expect_identical(result$set, rep(seq_along(branches), each = size))
          }
        }
      }
    }
  }
})
