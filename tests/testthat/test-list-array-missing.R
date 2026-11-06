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

# Local keys with list cells, optionally nested inside packed dimensions.
list_array_key <- function(size, tail, depth) {
  shape <- c(size, tail)
  key <- array(
    rep(list(2L, NULL, NA_integer_, 1L), length.out = prod(shape)),
    dim = shape
  )
  for (level in seq_len(depth)) {
    key <- tibble::tibble(component = key, note = rep("source", size))
  }
  key
}

# Missingness of one whole packed row for the ordinary ordering control.
list_array_row_missing <- function(key) {
  if (is.data.frame(key)) {
    return(Reduce(`&`, lapply(key, list_array_row_missing)))
  }
  vctrs::vec_detect_missing(key)
}

test_that("list-array expansion retains occurrences, shape and Margin order", {
  spec <- grouping_sets(
    grouping_set(key), grouping_set(), grouping_set()
  )
  for (size in c(0L, 1L, 3L)) {
    for (tail in list(integer(), 0L, 1L, 2L, c(0L, 2L), c(1L, 2L))) {
      for (depth in 0:2) {
        key <- list_array_key(size, tail, depth)
        input <- tibble::tibble(
          fixed = rep(c("b", NA, "a"), length.out = size),
          key = key, payload = seq_len(size)
        )
        before <- serialize(input, NULL)
        omitted <- dplyr::mutate(input, key = vctrs::vec_init(key, size))
        ordinary <- dplyr::union_all(
          dplyr::mutate(input, set = 1L),
          dplyr::union_all(dplyr::mutate(omitted, set = 2L),
                           dplyr::mutate(omitted, set = 3L))
        )
        ordinary <- dplyr::relocate(ordinary, set, .after = key)
        for (label in list(NULL, NA_character_)) {
          for (sort in c("none", "first", "last")) {
            expected <- ordinary
            if (sort != "none") {
              absent <- expected$set != 1L
              bit <- if (sort == "first") dplyr::desc(absent) else absent
              expected <- dplyr::arrange(
                expected, is.na(fixed), fixed, !!bit,
                !!list_array_row_missing(expected$key), key, set
              )
            }
            result <- expand_with_margins(
              input, .by = fixed, .grouping = spec,
              .margin_label = label, .duplicates = "keep",
              .id = "set", .sort = sort
            )
            expect_identical(result, expected)
            expect_identical(nrow(result), 3L * size)
            expect_identical(table(result$set), table(ordinary$set))
            expect_identical(serialize(input, NULL), before)
          }
        }
      }
    }
  }
})

test_that("all Margin verbs accept list-array missing rows", {
  for (size in c(0L, 1L, 3L)) {
    for (tail in list(integer(), 1L, c(1L, 2L))) {
      for (depth in 0:2) {
        key <- list_array_key(size, tail, depth)
        input <- tibble::tibble(key = key, payload = seq_len(size))
        before <- serialize(input, NULL)
        included <- dplyr::summarise(input, n = dplyr::n(), .by = key)
        total <- dplyr::mutate(
          dplyr::summarise(input, n = dplyr::n()),
          key = vctrs::vec_init(key, 1L)
        )
        expected <- dplyr::union_all(
          dplyr::mutate(included, set = 1L),
          dplyr::mutate(total, set = 2L)
        )
        cells <- dplyr::summarise(
          input, data = list(tibble::tibble(key = key, payload = payload)),
          .by = key
        )
        for (label in list(NULL, NA_character_)) {
          summarized <- summarize_with_margins(
            input, n = dplyr::n(), .grouping = rollup(key),
            .margin_label = label, .id = "set", .sort = "none"
          )
          expect_identical(summarized$key, expected$key)
          expect_identical(summarized$set, expected$set)
          expect_identical(summarized$n, expected$n)
          expanded <- expand_with_margins(
            input, .grouping = rollup(key), .margin_label = label,
            .id = "set", .sort = "none"
          )
          expect_identical(expanded$payload, rep(seq_len(size), 2L))
          for (verb in list(nest_with_margins, nest_by_with_margins)) {
            nested <- verb(
              input, .grouping = rollup(key), .margin_label = label,
              .id = "set", .sort = "none", .keep = TRUE
            )
            expect_identical(nested$key, expected$key)
            expect_identical(nested$set, expected$set)
            expect_identical(lengths(lapply(nested$data, row.names)),
                             expected$n)
            expect_identical(nested$data, c(cells$data, list(input)))
          }
          expect_identical(serialize(input, NULL), before)
        }
      }
    }
  }
})

test_that("list arrays remain valid included, fixed and payload columns", {
  for (size in c(0L, 3L)) {
    for (tail in list(integer(), 0L, 1L, c(1L, 2L))) {
      for (depth in 0:2) {
        key <- list_array_key(size, tail, depth)
        input <- tibble::tibble(
          key = key, other = rep(c("b", NA, "a"), length.out = size),
          payload = seq_len(size)
        )
        before <- serialize(input, NULL)
        for (label in list(NULL, NA_character_)) {
          expect_identical(
            expand_with_margins(input, .grouping = grouping_set(key),
                                .margin_label = label, .sort = "none"),
            input
          )
          ordinary <- dplyr::union_all(
            input, dplyr::mutate(input, other = NA_character_)
          )
          fixed <- expand_with_margins(
            input, .by = key, .grouping = rollup(other),
            .margin_label = label, .sort = "none"
          )
          expect_identical(fixed, ordinary)
          payload <- expand_with_margins(
            input, .grouping = rollup(other),
            .margin_label = label, .sort = "none"
          )
          expect_identical(payload, dplyr::relocate(ordinary, other))
          expect_identical(serialize(input, NULL), before)
        }
      }
    }
  }
})

test_that("list-array admission retains structured-label and raw refusals", {
  for (depth in 0:2) {
    key <- list_array_key(2L, c(1L, 2L), depth)
    input <- tibble::tibble(key = key)
    expect_error(
      expand_with_margins(input, .grouping = rollup(key),
                          .margin_label = "All"),
      if (depth == 0L) "matrix or array grouping dimensions" else
        "packed grouping dimensions",
      class = "marginplyr_error"
    )
    input$key <- tibble::tibble(list = key, raw = as.raw(c(0, 1)))
    for (label in list(NULL, NA_character_)) {
      expect_error(
        expand_with_margins(input, .grouping = rollup(key),
                            .margin_label = label),
        "typed-missing", class = "marginplyr_error"
      )
    }
  }
})
