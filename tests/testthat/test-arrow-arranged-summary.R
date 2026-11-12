test_that("arranged Arrow summaries retain public keys and Margin order", {
  skip_if_suggest_absent("arrow")
  path <- tempfile(fileext = ".parquet")
  on.exit(unlink(path))
  for (empty in c(FALSE, TRUE)) {
    data <- data.frame(g = c("b", "a"), v = c(3, 1))
    if (empty) data <- data[FALSE, ]
    arrow::write_parquet(data, path)
    sources <- list(table = arrow::arrow_table(data),
                    dataset = arrow::open_dataset(path))
    for (shape in names(sources)) {
      for (descending in c(FALSE, TRUE)) {
        source <- sources[[shape]]
        arranged <- if (descending) {
          dplyr::arrange(source, dplyr::desc(g))
        } else {
          dplyr::arrange(source, g)
        }
        ordinary <- dplyr::collect(dplyr::summarize(
          arranged, total = sum(v), .by = g
        ))
        expected <- data.frame(g = c("a", "b"), total = c(1, 3))
        if (empty) expected <- expected[FALSE, ]
        expect_identical(arrow_partition_bag(ordinary), expected)
        for (input in list(arranged, source, dplyr::collapse(arranged))) {
          for (role in c("fixed", "included", "rollup")) {
            for (sort in c("none", "first", "last")) {
              info <- paste(empty, shape, descending, role, sort)
              query <- if (role == "fixed") {
                summarize_with_margins(
                  input, total = sum(v), .by = g, .sort = sort
                )
              } else {
                grouping <- if (role == "included") {
                  grouping_set(g)
                } else {
                  rollup(g)
                }
                summarize_with_margins(
                  input, total = sum(v), .grouping = grouping,
                  .margin_label = NULL, .sort = sort
                )
              }
              want <- expected
              if (role == "rollup") {
                total <- data.frame(
                  g = NA_character_, total = if (empty) 0 else 4
                )
                want <- if (sort == "first") {
                  rbind(total, want)
                } else {
                  rbind(want, total)
                }
              }
              expect_s3_class(query, "arrow_dplyr_query")
              expect_identical(names(query), c("g", "total"), info = info)
              actual <- dplyr::collect(query)
              expect_identical(arrow_partition_bag(actual),
                               arrow_partition_bag(want), info = info)
              if (sort != "none") {
                expect_identical(as.data.frame(actual), want, info = info)
              }
            }
          }
        }
      }
    }
  }
})

test_that("the observed label check reads an arranged Arrow input", {
  skip_if_suggest_absent("arrow")
  data <- data.frame(g = c("Total", "a", "b"), v = c(3L, 1L, 2L))
  sources <- list(
    table = arrow::arrow_table(data),
    record_batch = arrow::record_batch(data),
    dataset = arrow::InMemoryDataset$create(data)
  )
  verbs <- list(
    summarize = function(x, check, label = "Total") {
      summarize_with_margins(
        x, s = sum(v), .grouping = rollup(g), .margin_label = label,
        .check_margin_label = check
      )
    },
    expand = function(x, check, label = "Total") {
      expand_with_margins(
        x, .grouping = rollup(g), .margin_label = label,
        .check_margin_label = check
      )
    }
  )
  for (shape in names(sources)) {
    for (key in c("column", "expression")) {
      arranged <- if (key == "column") {
        dplyr::arrange(sources[[shape]], v)
      } else {
        dplyr::arrange(sources[[shape]], dplyr::desc(v * 2L))
      }
      # The first two rows by `v` exclude the one holding the label.
      selected <- head(dplyr::arrange(sources[[shape]], v), 2L)
      for (verb in names(verbs)) {
        info <- paste(shape, key, verb)
        run <- verbs[[verb]]
        expect_identical(
          arrow_partition_bag(dplyr::collect(run(arranged, TRUE, "All"))),
          arrow_partition_bag(dplyr::collect(run(arranged, FALSE, "All"))),
          info = info
        )
        expect_identical(
          arrow_partition_bag(dplyr::collect(run(selected, TRUE))),
          arrow_partition_bag(dplyr::collect(run(selected, FALSE))),
          info = info
        )
        expect_error(
          run(arranged, TRUE), "already present", class = "marginplyr_error",
          info = info
        )
      }
    }
  }
})

test_that("the observed label check reads an Arrow input with a repeated key", {
  skip_if_suggest_absent("arrow")
  path <- tempfile(fileext = ".parquet")
  on.exit(unlink(path))
  data <- data.frame(
    g = c("Total", "a", "b"), w = c(2L, 1L, 1L), v = c(3L, 1L, 2L)
  )
  arrow::write_parquet(data, path)
  sources <- list(
    table = arrow::arrow_table(data),
    in_memory_dataset = arrow::InMemoryDataset$create(data),
    file_dataset = arrow::open_dataset(path)
  )
  shapes <- list(
    rearranged = function(x) dplyr::arrange(dplyr::arrange(x, v), w, v),
    reversed = function(x) dplyr::arrange(x, v, dplyr::desc(v)),
    doubled = function(x) dplyr::arrange(x, v, v)
  )
  verbs <- list(
    summarize = function(x, check, label = "Total") {
      summarize_with_margins(
        x, s = sum(v), .grouping = rollup(g), .margin_label = label,
        .check_margin_label = check
      )
    },
    expand = function(x, check, label = "Total") {
      expand_with_margins(
        x, .grouping = rollup(g), .margin_label = label,
        .check_margin_label = check
      )
    }
  )
  for (source in names(sources)) {
    for (shape in names(shapes)) {
      arranged <- shapes[[shape]](sources[[source]])
      # The first two rows by `v` exclude the one holding the label.
      selected <- head(arranged, 2L)
      for (verb in names(verbs)) {
        info <- paste(source, shape, verb)
        run <- verbs[[verb]]
        expect_identical(
          arrow_partition_bag(dplyr::collect(run(arranged, TRUE, "All"))),
          arrow_partition_bag(dplyr::collect(run(arranged, FALSE, "All"))),
          info = info
        )
        expect_identical(
          arrow_partition_bag(dplyr::collect(run(selected, TRUE))),
          arrow_partition_bag(dplyr::collect(run(selected, FALSE))),
          info = info
        )
        expect_error(
          run(arranged, TRUE), "already present", class = "marginplyr_error",
          info = info
        )
      }
    }
  }
})
