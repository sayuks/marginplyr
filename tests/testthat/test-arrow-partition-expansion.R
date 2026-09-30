test_that("Arrow partition expansion preserves a typed missing Margin label", {
  skip_if_suggest_absent("arrow")
  fixture <- arrow_partition_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE))
  expected <- data.frame(p = c(1L, NA_integer_), occurrence = 1:2,
                         row_id = c(1L, 1L))
  query <- expand_with_margins(
    fixture$source, .grouping = rollup(p), .id = "occurrence",
    .margin_label = NULL
  )
  expect_s3_class(query, "arrow_dplyr_query")
  expect_identical(arrow_partition_bag(dplyr::collect(query)),
                   arrow_partition_bag(expected))
  expect_identical(arrow_partition_bag(dplyr::collect(dplyr::compute(query))),
                   arrow_partition_bag(expected))
})

test_that("partition expansion preserves occurrences, labels and order", {
  skip_if_suggest_absent("arrow")
  for (p in list(c(2L, 1L, 1L), c("B", "A", "A"))) {
    fixture <- arrow_partition_fixture(p, 1:3)
    sources <- arrow_partition_controls(fixture)
    queries <- list()
    expectations <- list()
    for (label in list(NULL, NA_character_, "Total")) {
      values <- if (is.null(label) || is.na(label)) {
        c(p, rep(p[NA_integer_], 3L), p)
      } else {
        c(as.character(p), rep("Total", 3L), as.character(p))
      }
      expected <- data.frame(p = values, occurrence = rep(1:3, each = 3L),
                             row_id = rep(1:3, 3L))
      for (sort in c("none", "first", "last")) {
        for (id in list(NULL, "occurrence")) {
          for (source in sources) {
            query <- expand_with_margins(
              source,
              .grouping = grouping_sets(
                grouping_set(p), grouping_set(), grouping_set(p)
              ),
              .duplicates = "keep", .id = id, .sort = sort,
              .margin_label = label
            )
            expect_s3_class(query, "arrow_dplyr_query")
            want <- expected
            if (is.null(id)) want$occurrence <- NULL
            queries[[length(queries) + 1L]] <- query
            expectations[[length(expectations) + 1L]] <- want
            for (actual in list(dplyr::collect(query),
                                dplyr::collect(dplyr::compute(query)))) {
              expect_identical(
                arrow_partition_bag(actual), arrow_partition_bag(want)
              )
              if (sort != "none") {
                # Detail values ascend; total rows precede/follow all details.
                detail <- rep(sort(p), each = 2L)
                total <- values[4:6]
                ordered <- if (sort == "first") {
                  c(total, detail)
                } else {
                  c(detail, total)
                }
                if (!is.null(label) && !is.na(label)) {
                  ordered <- as.character(ordered)
                }
                expect_identical(actual$p, ordered)
                if (!is.null(id)) {
                  detail_ids <- c(1L, 1L, 3L, 3L, 1L, 3L)
                  ordered_ids <- if (sort == "first") {
                    c(rep(2L, 3L), detail_ids)
                  } else {
                    c(detail_ids, rep(2L, 3L))
                  }
                  expect_identical(actual$occurrence, ordered_ids)
                }
              }
            }
          }
        }
      }
    }
    for (i in rev(seq_along(queries))) {
      expect_identical(arrow_partition_bag(dplyr::collect(queries[[i]])),
                       arrow_partition_bag(expectations[[i]]))
    }
    expect_identical(tools::md5sum(fixture$files), fixture$hashes)
    expect_identical(arrow_partition_bag(dplyr::collect(fixture$source)),
                     arrow_partition_bag(fixture$data))
    unlink(fixture$path, recursive = TRUE)
  }
})

test_that("partition fixed keys and summary aliases retain their values", {
  skip_if_suggest_absent("arrow")
  fixture <- arrow_partition_fixture(c(2L, 1L, 1L), 1:3)
  on.exit(unlink(fixture$path, recursive = TRUE))
  expected <- data.frame(p = c(2L, 1L, 1L, 2L, 1L, 1L),
                         row_id = c(1:3, rep(NA_integer_, 3L)),
                         occurrence = rep(1:2, each = 3L))
  for (source in arrow_partition_controls(fixture)) {
    fixed <- expand_with_margins(
      source, .by = p, .grouping = rollup(row_id), .id = "occurrence",
      .margin_label = NULL, .sort = "last"
    )
    expect_identical(arrow_partition_bag(dplyr::collect(fixed)),
                     arrow_partition_bag(expected))
    expect_identical(arrow_partition_bag(dplyr::collect(dplyr::compute(fixed))),
                     arrow_partition_bag(expected))
    for (verb in list(summarize_with_margins, summarise_with_margins)) {
      query <- verb(source, count = dplyr::n(), total = sum(row_id),
                    .grouping = rollup(p), .id = "occurrence",
                    .margin_label = NULL)
      results <- list(
        dplyr::collect(query), dplyr::collect(dplyr::compute(query))
      )
      for (actual in results) {
        actual$count <- as.numeric(actual$count)
        actual$total <- as.numeric(actual$total)
        expected_summary <- data.frame(
          p = c(1L, 2L, NA_integer_), occurrence = c(1L, 1L, 2L),
          count = c(2, 1, 3), total = c(5, 1, 6)
        )
        expect_identical(arrow_partition_bag(actual),
                         arrow_partition_bag(expected_summary))
      }
    }
  }
  expect_identical(tools::md5sum(fixture$files), fixture$hashes)
})

test_that("empty partition expansions preserve declared types", {
  skip_if_suggest_absent("arrow")
  for (p in list(1L, "A")) {
    fixture <- arrow_partition_fixture(p)
    for (label in list(NULL, "Total")) {
      for (sort in c("none", "first", "last")) {
        for (id in list(NULL, "occurrence")) {
          query <- expand_with_margins(
            dplyr::filter(fixture$source, row_id < 0L),
            .grouping = rollup(p), .margin_label = label, .id = id, .sort = sort
          )
          expected <- data.frame(p = if (is.null(label)) p[0L] else character(),
                                 row_id = integer())
          if (!is.null(id)) {
            expected$occurrence <- integer()
            expected <- expected[c("p", "occurrence", "row_id")]
          }
          expect_identical(as.data.frame(dplyr::collect(query)), expected)
          expect_identical(
            as.data.frame(dplyr::collect(dplyr::compute(query))), expected
          )
        }
      }
    }
    expect_identical(tools::md5sum(fixture$files), fixture$hashes)
    unlink(fixture$path, recursive = TRUE)
  }
})

test_that("Arrow expansion private names cannot shadow public columns", {
  skip_if_suggest_absent("arrow")
  fixture <- arrow_partition_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE))
  source <- dplyr::mutate(fixture$source, ..marginplyr_arrow_dimension_1 = 7L)
  query <- expand_with_margins(
    source, .grouping = rollup(p), .id = "..marginplyr_arrow_dimension_1_",
    .margin_label = NULL
  )
  expected <- data.frame(p = c(1L, NA_integer_),
                         ..marginplyr_arrow_dimension_1_ = 1:2,
                         row_id = c(1L, 1L),
                         ..marginplyr_arrow_dimension_1 = c(7L, 7L))
  expect_identical(arrow_partition_bag(dplyr::collect(query)),
                   arrow_partition_bag(expected))
})
