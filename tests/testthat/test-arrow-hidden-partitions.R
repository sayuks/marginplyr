test_that("hidden partitions preserve expanded values, identifiers and order", {
  skip_if_suggest_absent("arrow")
  partitions <- c(
    "sid", "..marginplyr_arrow_dimension_1", "..marginplyr_arrow_dimension_1_",
    "..marginplyr_arrow_id_1", "..marginplyr_arrow_id_1_",
    "..marginplyr_sort_1", "..marginplyr_sort_1_"
  )
  fixture <- arrow_hidden_partition_fixture(partitions)
  on.exit(unlink(fixture$path, recursive = TRUE))
  renamed <- stats::setNames(
    partitions, paste0("partition_", seq_along(partitions))
  )
  sources <- list(
    selected = fixture$source,
    renamed = dplyr::rename(fixture$dataset, dplyr::all_of(renamed))
  )
  for (source_name in names(sources)) {
    for (empty in c(FALSE, TRUE)) {
      source <- sources[[source_name]]
      if (empty) source <- dplyr::filter(source, row_id < 0L)
      for (label in list(NULL, "Total")) {
        values <- if (is.null(label)) {
          c(9L, 8L, NA_integer_, NA_integer_)
        } else {
          c("9", "8", "Total", "Total")
        }
        for (sort in c("none", "first", "last")) {
          for (id in list(NULL, "sid", "...", "..1", "..42")) {
            info <- paste(source_name, empty, label, sort, id)
            query <- expand_with_margins(
              source, .grouping = rollup(g), .id = id, .margin_label = label,
              .sort = sort
            )
            expect_s3_class(query, "arrow_dplyr_query")
            expected <- data.frame(g = values, row_id = c(1L, 2L, 1L, 2L))
            if (!is.null(id)) {
              expected[[id]] <- c(1L, 1L, 2L, 2L)
              expected <- expected[c("g", id, "row_id")]
            }
            if (source_name == "renamed") {
              for (name in names(renamed)) {
                expected[[name]] <- c(1L, 2L, 1L, 2L)
              }
            }
            if (empty) expected <- expected[FALSE, , drop = FALSE]
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

test_that("hidden partition expansion agrees with ordinary Arrow controls", {
  skip_if_suggest_absent("arrow")
  for (partition in c("sid", "..marginplyr_arrow_dimension_1")) {
    fixture <- arrow_hidden_partition_fixture(partition)
    source <- fixture$source
    expected <- data.frame(g = c(9L, 8L, NA_integer_, NA_integer_),
                           sid = c(1L, 1L, 2L, 2L), row_id = c(1L, 2L, 1L, 2L))
    ordinary <- dplyr::union_all(
      dplyr::mutate(source, sid = 1L),
      dplyr::mutate(source, g = NA_integer_, sid = 2L)
    )
    actual <- dplyr::collect(ordinary)[names(expected)]
    expect_identical(arrow_partition_bag(actual),
                     arrow_partition_bag(expected))
    sources <- list(
      source, dplyr::compute(source),
      dplyr::union_all(arrow::arrow_table(g = integer(), row_id = integer()),
                       source),
      dplyr::left_join(arrow::arrow_table(row_id = 1:2), source, by = "row_id")
    )
    for (input in sources) {
      query <- expand_with_margins(
        input, .grouping = rollup(g), .id = "sid", .margin_label = NULL
      )
      expect_identical(arrow_partition_bag(dplyr::collect(query)),
                       arrow_partition_bag(expected))
      included <- expand_with_margins(
        input, .grouping = grouping_set(g), .id = "sid", .margin_label = NULL
      )
      expect_identical(arrow_partition_bag(dplyr::collect(included)),
                       arrow_partition_bag(expected[1:2, ]))
    }
    expect_identical(arrow_partition_bag(dplyr::collect(source)),
                     data.frame(g = 8:9, row_id = 2:1))
    unlink(fixture$path, recursive = TRUE)
  }
})
