test_that("unsorted expansion uses a typed-missing factor matrix row", {
  key <- factor("a", levels = c("a", "unused"))
  dim(key) <- c(1L, 1L)
  input <- tibble::tibble(key = key)
  control <- dplyr::union_all(
    input, dplyr::mutate(input, key = vctrs::vec_init(key, 1L))
  )

  result <- expand_with_margins(
    input, .grouping = rollup(key), .margin_label = NULL, .sort = "none"
  )
  expect_identical(result, control)
  expect_identical(dim(result$key), c(2L, 1L))
  expect_identical(levels(result$key), c("a", "unused"))
  expect_identical(as.integer(result$key), c(1L, NA_integer_))
})

test_that("factor-array expansion obeys typed-missing label rules", {
  cases <- expand.grid(
    size = c(0L, 1L, 3L), array = c(FALSE, TRUE),
    ordered = c(FALSE, TRUE), na_level = c(FALSE, TRUE)
  )
  for (i in seq_len(nrow(cases))) {
    case <- cases[i, ]
    shape <- c(case$size, if (case$array) c(2L, 2L) else 2L)
    declared <- c("a", "b", "unused", if (case$na_level) NA_character_)
    key <- factor(
      rep(c("a", "b", NA_character_), length.out = prod(shape)),
      levels = declared, exclude = if (case$na_level) NULL else NA,
      ordered = case$ordered
    )
    dim(key) <- shape
    input <- tibble::tibble(key = key, value = seq_len(case$size))
    control <- dplyr::union_all(
      dplyr::mutate(input, set = 1L),
      dplyr::mutate(input, key = vctrs::vec_init(key, case$size), set = 2L)
    )
    control <- dplyr::relocate(control, set, .after = key)
    for (label in list(NULL, NA_character_)) {
      if (case$na_level && !is.null(label)) {
        for (grouping in list(grouping_set(key), rollup(key))) {
          for (check in c(FALSE, TRUE)) {
            expect_error(
              expand_with_margins(
                input, .grouping = grouping, .margin_label = label,
                .check_margin_label = check, .sort = "none"
              ),
              "already a factor level", class = "marginplyr_error"
            )
          }
        }
        next
      }
      included <- expand_with_margins(
        input, .grouping = grouping_set(key), .margin_label = label,
        .sort = "none"
      )
      expect_identical(included, input)
      result <- expand_with_margins(
        input, .grouping = rollup(key), .margin_label = label,
        .id = "set", .sort = "none"
      )
      expect_identical(result, control)
      expect_identical(dim(result$key), c(2L * case$size, shape[-1L]))
      expect_identical(levels(result$key), declared)
      expect_identical(is.ordered(result$key), case$ordered)
      expect_identical(
        as.integer(vctrs::vec_slice(result$key, seq_len(case$size))),
        as.integer(key)
      )
      expect_identical(
        as.integer(vctrs::vec_slice(
          result$key, case$size + seq_len(case$size)
        )),
        rep(NA_integer_, length(key))
      )
      expect_identical(result$value, rep(seq_len(case$size), 2L))
      expect_identical(result$set, rep(1:2, each = case$size))
    }
    expect_error(
      expand_with_margins(input, .grouping = rollup(key),
                          .margin_label = "All", .sort = "none"),
      "matrix or array grouping dimensions", class = "marginplyr_error"
    )
  }
})

test_that("shaped integer expansion remains a typed-missing control", {
  for (size in c(0L, 1L, 3L)) {
    for (tail in list(1L, c(2L, 2L))) {
      shape <- c(size, tail)
      input <- tibble::tibble(key = array(seq_len(prod(shape)), dim = shape))
      control <- dplyr::union_all(
        input, dplyr::mutate(input, key = vctrs::vec_init(key, size))
      )
      for (label in list(NULL, NA_character_)) {
        result <- expand_with_margins(
          input, .grouping = rollup(key), .margin_label = label, .sort = "none"
        )
        expect_identical(result, control)
        expect_identical(dim(result$key), c(2L * size, tail))
      }
    }
  }
})
