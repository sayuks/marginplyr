test_that("Dataset schemas omitting Hive fields preserve expansion semantics", {
  skip_if_suggest_absent("arrow")
  partitions <- c(
    "sid", "..marginplyr_arrow_dimension_1", "..marginplyr_arrow_dimension_1_",
    "..marginplyr_arrow_id_1", "..marginplyr_arrow_id_1_",
    "..marginplyr_sort_1", "..marginplyr_sort_1_", "unrelated"
  )
  fixture <- arrow_hidden_partition_fixture(partitions)
  on.exit(unlink(fixture$path, recursive = TRUE))
  sources <- arrow_narrow_partition_sources(fixture, partitions)
  sources$projected <- fixture$source
  sources$materialized <- dplyr::compute(sources$explicit)
  sources$table <- arrow::arrow_table(g = c(9L, 8L), row_id = 1:2)
  for (shape in names(sources)) {
    expect_identical(names(sources[[shape]]), c("g", "row_id"))
    expect_identical(arrow_partition_bag(dplyr::collect(sources[[shape]])),
                     data.frame(g = 8:9, row_id = 2:1))
    for (empty in c(FALSE, TRUE)) {
      source <- sources[[shape]]
      if (empty) source <- dplyr::filter(source, row_id < 0L)
      for (label in list(NULL, "Total")) {
        for (sort in c("none", "first", "last")) {
          for (id in list(NULL, "sid", "..1")) {
            info <- paste(shape, empty, label, sort, id)
            query <- expand_with_margins(
              source, .grouping = rollup(g), .margin_label = label,
              .sort = sort, .id = id
            )
            values <- if (is.null(label)) {
              c(9L, 8L, NA_integer_, NA_integer_)
            } else {
              c("9", "8", "Total", "Total")
            }
            expected <- data.frame(g = values, row_id = c(1L, 2L, 1L, 2L))
            if (!is.null(id)) {
              expected[[id]] <- c(1L, 1L, 2L, 2L)
              expected <- expected[c("g", id, "row_id")]
            }
            if (empty) expected <- expected[FALSE, , drop = FALSE]
            expect_s3_class(query, "arrow_dplyr_query")
            expect_identical(names(query), names(expected), info = info)
            for (actual in list(dplyr::collect(query),
                                dplyr::collect(dplyr::compute(query)))) {
              expect_identical(arrow_partition_bag(actual),
                               arrow_partition_bag(expected), info = info)
              if (!empty && sort != "none") {
                positions <- if (sort == "first") {
                  c(3L, 4L, 2L, 1L)
                } else {
                  c(2L, 1L, 3L, 4L)
                }
                expect_identical(actual$g, values[positions], info = info)
                if (!is.null(id)) {
                  expect_identical(actual[[id]], expected[[id]][positions],
                                   info = info)
                }
              }
            }
          }
        }
      }
    }
  }
})

test_that("individual omitted Hive names cannot replace occurrence values", {
  skip_if_suggest_absent("arrow")
  for (partition in c("sid", "..marginplyr_arrow_dimension_1",
                      "..marginplyr_sort_1", "unrelated")) {
    for (empty in c(FALSE, TRUE)) {
      fixture <- arrow_hidden_partition_fixture(partition, empty = empty)
      for (input in arrow_narrow_partition_sources(fixture, partition)) {
        for (sort in c("none", "first", "last")) {
          for (id in list(NULL, "sid")) {
            grouping <- grouping_sets(
              grouping_set(g), grouping_set(), grouping_set(g)
            )
            query <- expand_with_margins(
              input, .grouping = grouping, .duplicates = "keep",
              .margin_label = NULL, .id = id, .sort = sort
            )
            expected <- data.frame(
              g = c(9L, 8L, NA_integer_, NA_integer_, 9L, 8L),
              row_id = rep(1:2, 3L)
            )
            if (!is.null(id)) {
              expected$sid <- rep(1:3, each = 2L)
              expected <- expected[c("g", "sid", "row_id")]
            }
            if (empty) expected <- expected[FALSE, , drop = FALSE]
            actual <- dplyr::collect(query)
            expect_identical(arrow_partition_bag(actual),
                             arrow_partition_bag(expected))
            if (!empty && sort != "none") {
              want <- c(8L, 8L, 9L, 9L)
              margins <- rep(NA_integer_, 2L)
              want <- if (sort == "first") {
                c(margins, want)
              } else {
                c(want, margins)
              }
              expect_identical(actual$g, want)
              if (!is.null(id)) {
                ids <- c(1L, 3L, 1L, 3L)
                ids <- if (sort == "first") {
                  c(2L, 2L, ids)
                } else {
                  c(ids, 2L, 2L)
                }
                expect_identical(actual$sid, ids)
              }
            }
          }
        }
      }
      unlink(fixture$path, recursive = TRUE)
    }
  }
})

test_that("URI-encoded Hive keys cannot replace public identifiers", {
  skip_if_suggest_absent("arrow")
  path <- tempfile("unrelated%literal=value-")
  on.exit(unlink(path, recursive = TRUE))
  for (i in 1:2) {
    directory <- file.path(path, paste0("s%69d=", i))
    dir.create(directory, recursive = TRUE)
    arrow::write_parquet(data.frame(g = 10L - i, row_id = i),
                         file.path(directory, "facts.parquet"))
  }
  source <- arrow::open_dataset(
    path, schema = arrow::schema(g = arrow::int32(), row_id = arrow::int32()),
    partitioning = arrow::hive_partition(sid = arrow::int32())
  )
  query <- expand_with_margins(
    source, .grouping = rollup(g), .id = "sid", .margin_label = NULL,
    .sort = "last"
  )
  actual <- dplyr::collect(query)
  expected <- data.frame(g = c(8L, 9L, NA_integer_, NA_integer_),
                         sid = c(1L, 1L, 2L, 2L), row_id = c(2L, 1L, 1L, 2L))
  expect_identical(arrow_partition_bag(actual), arrow_partition_bag(expected))
  expect_identical(actual$g, expected$g)
  expect_identical(actual$sid, expected$sid)
})
