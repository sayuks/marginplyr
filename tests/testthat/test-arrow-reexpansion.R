test_that("a sorted Arrow expansion can be expanded again", {
  skip_if_suggest_absent("arrow")
  source <- arrow::Table$create(
    data.frame(g = c("b", "a"), h = c(2L, 1L), payload = c(20, 10))
  )
  first <- expand_with_margins(
    source, .grouping = rollup(g), .margin_label = NULL, .sort = "last"
  )
  query <- expand_with_margins(
    first, .grouping = rollup(h), .margin_label = NULL, .id = "second"
  )
  expected <- data.frame(
    h = c(2L, 1L, 2L, 1L, rep(NA_integer_, 4L)),
    second = rep(1:2, each = 4L),
    g = rep(c("b", "a", NA_character_, NA_character_), 2L),
    payload = rep(c(20, 10, 20, 10), 2L)
  )
  expect_s3_class(query, "arrow_dplyr_query")
  expect_identical(arrow_partition_bag(dplyr::collect(query)),
                   arrow_partition_bag(expected))
})

test_that("Arrow Margin results remain reusable across expansion choices", {
  skip_if_suggest_absent("arrow")
  path <- tempfile(fileext = ".parquet")
  on.exit(unlink(path))
  for (empty in c(FALSE, TRUE)) {
    sources <- arrow_reexpansion_sources(path, empty)
    for (shape in names(sources)) {
      for (summary in c(FALSE, TRUE)) {
        for (first_sort in c("none", "first", "last")) {
          for (id in list(NULL, "first")) {
            first <- arrow_reexpansion_first(
              sources[[shape]], summary, first_sort, id
            )
            for (sort in c("none", "first", "last")) {
              info <- paste(empty, shape, summary, first_sort, id, sort)
              query <- expand_with_margins(
                first, .grouping = rollup(h), .margin_label = NULL,
                .id = "second", .sort = sort
              )
              expected <- data.frame(
                h = c(2L, 1L, 2L, 1L, rep(NA_integer_, 4L)),
                second = rep(1:2, each = 4L),
                g = rep(c("b", "a", NA_character_, NA_character_), 2L),
                payload = rep(c(20, 10, 20, 10), 2L)
              )
              if (!is.null(id)) {
                expected$first <- rep(c(1L, 1L, 2L, 2L), 2L)
                expected <- expected[c("h", "second", "g", "first", "payload")]
              }
              if (empty) expected <- expected[0L, ]
              expect_s3_class(query, "arrow_dplyr_query")
              actual <- dplyr::collect(query)
              expect_identical(arrow_partition_bag(actual),
                               arrow_partition_bag(expected), info = info)
              if (!empty && sort != "none") {
                # Only the second Margin order is promised; g and payload can
                # tie within h, so their relative positions are not asserted.
                detail <- c(1L, 1L, 2L, 2L)
                total <- rep(NA_integer_, 4L)
                want_h <- if (sort == "first") {
                  c(total, detail)
                } else {
                  c(detail, total)
                }
                want_ids <- if (sort == "first") {
                  rep(2:1, each = 4L)
                } else {
                  rep(1:2, each = 4L)
                }
                expect_identical(actual$h, want_h, info = info)
                expect_identical(actual$second, want_ids, info = info)
              }
            }
          }
        }
      }
    }
  }
})

test_that("Arrow expansion boundaries preserve control plans and repeats", {
  skip_if_suggest_absent("arrow")
  path <- tempfile(fileext = ".parquet")
  on.exit(unlink(path))
  for (source in arrow_reexpansion_sources(path)) {
    for (summary in c(FALSE, TRUE)) {
      first <- arrow_reexpansion_first(source, summary, "last", "first")
      for (input in list(first, dplyr::collapse(first))) {
        for (repeats in c(FALSE, TRUE)) {
          grouping <- if (repeats) {
            grouping_sets(grouping_set(h), grouping_set(), grouping_set(h))
          } else {
            grouping_set(h)
          }
          query <- expand_with_margins(
            input, .grouping = grouping, .margin_label = NULL,
            .id = "second", .duplicates = "keep", .sort = "last"
          )
          expected <- data.frame(
            h = c(2L, 1L, 2L, 1L), second = rep(1L, 4L),
            g = c("b", "a", NA_character_, NA_character_),
            first = c(1L, 1L, 2L, 2L), payload = c(20, 10, 20, 10)
          )
          if (repeats) {
            total <- expected
            total$h <- NA_integer_
            total$second <- 2L
            repeated <- expected
            repeated$second <- 3L
            expected <- rbind(expected, total, repeated)
          }
          expect_identical(arrow_partition_bag(dplyr::collect(query)),
                           arrow_partition_bag(expected))
        }
      }
    }
    direct <- expand_with_margins(
      source, .grouping = rollup(h), .margin_label = NULL, .id = "second"
    )
    expected <- data.frame(h = c(2L, 1L, NA_integer_, NA_integer_),
                           second = c(1L, 1L, 2L, 2L),
                           g = c("b", "a", "b", "a"),
                           payload = c(20, 10, 20, 10))
    expect_identical(arrow_partition_bag(dplyr::collect(direct)),
                     arrow_partition_bag(expected))
  }
})
