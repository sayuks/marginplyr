test_that("Arrow expansions preserve supported dot-dot occurrence names", {
  skip_if_suggest_absent("arrow")
  for (empty in c(FALSE, TRUE)) {
    data <- data.frame(value = 1:2, fixed = c("x", "x"), g = c("b", "a"))
    if (empty) data <- data[FALSE, , drop = FALSE]
    table <- arrow::arrow_table(data)
    dataset <- arrow::InMemoryDataset$create(table)
    sources <- list(table, dplyr::filter(dataset, .data$value > 0L))
    for (source in sources) {
      for (name in c("...", "..1", "..42")) {
        ordinary <- dplyr::collect(dplyr::mutate(source, !!name := 1L))
        expect_identical(names(ordinary), c(names(data), name))
        expect_identical(ordinary[[name]], rep(1L, nrow(data)))
        for (spec in list(NULL, grouping_set(g), rollup(g))) {
          multiple <- !is.null(spec) && identical(spec, rollup(g))
          by <- if (is.null(spec)) character() else "fixed"
          for (label in list(NULL, "Total")) {
            for (sort in c("none", "first", "last")) {
              info <- paste(empty, class(source)[[1L]], name, multiple,
                            label, sort)
              query <- expand_with_margins(
                source, .by = dplyr::all_of(by), .grouping = spec, .id = name,
                .sort = sort, .margin_label = label
              )
              expect_s3_class(query, "arrow_dplyr_query")
              actual <- dplyr::collect(query)
              expected <- if (is.null(spec)) {
                data.frame(id = c(1L, 1L), value = 1:2,
                           fixed = c("x", "x"), g = c("b", "a"))
              } else {
                data.frame(fixed = c("x", "x"), g = c("b", "a"),
                           id = c(1L, 1L), value = 1:2)
              }
              if (multiple) {
                expected <- data.frame(
                  fixed = rep("x", 4L),
                  g = c("b", "a", rep(if (is.null(label)) NA_character_
                                      else label, 2L)),
                  id = c(1L, 1L, 2L, 2L), value = c(1L, 2L, 1L, 2L)
                )
              }
              names(expected)[names(expected) == "id"] <- name
              if (empty) expected <- expected[FALSE, , drop = FALSE]
              expect_identical(names(actual), names(expected), info = info)
              expect_type(actual[[name]], "integer")
              rows <- order(actual[[name]], actual[["value"]])
              expect_identical(as.data.frame(actual[rows, ]), expected,
                               info = info)
              if (!empty && !is.null(spec) && sort != "none") {
                detail <- c("a", "b")
                totals <- if (multiple) {
                  rep(if (is.null(label)) NA_character_ else label, 2L)
                } else {
                  character()
                }
                ordered <- if (sort == "first") {
                  c(totals, detail)
                } else {
                  c(detail, totals)
                }
                ids <- if (multiple) c(1L, 1L, 2L, 2L) else c(1L, 1L)
                if (sort == "first") ids <- rev(ids)
                expect_identical(actual[["g"]], ordered, info = info)
                expect_identical(actual[[name]], ids, info = info)
              }
            }
          }
        }
      }
    }
  }
})

test_that("Arrow expansion preserves identifier and label collision checks", {
  skip_if_suggest_absent("arrow")
  source <- arrow::arrow_table(data.frame(g = "a", value = 1L))
  for (name in c("g", "value")) {
    expect_error(
      expand_with_margins(source, .grouping = rollup(g), .id = name),
      class = "marginplyr_error"
    )
  }
  for (name in c("...", "..1", "..42")) {
    colliding <- dplyr::mutate(source, !!name := 7L)
    expect_error(
      expand_with_margins(colliding, .grouping = rollup(g), .id = name),
      class = "vctrs_error_names_cannot_be_dot_dot"
    )
    source <- arrow::arrow_table(data.frame(g = "Total", value = 1L))
    expect_error(
      expand_with_margins(
        source, .grouping = rollup(g), .id = name,
        .check_margin_label = TRUE
      ),
      class = "marginplyr_error"
    )
  }
})

test_that("Arrow partition labels survive dot-dot occurrence names", {
  skip_if_suggest_absent("arrow")
  fixture <- arrow_partition_fixture()
  on.exit(unlink(fixture$path, recursive = TRUE))
  query <- expand_with_margins(
    fixture$source, .grouping = rollup(p), .margin_label = NULL, .id = "..42"
  )
  actual <- dplyr::collect(query)
  expect_identical(names(actual), c("p", "..42", "row_id"))
  rows <- order(actual[["..42"]])
  expect_identical(actual[["p"]][rows], c(1L, NA_integer_))
  expect_identical(actual[["..42"]][rows], 1:2)
  expect_identical(actual[["row_id"]][rows], c(1L, 1L))
})

test_that("Arrow dot-dot identifiers preserve supported .data dimensions", {
  skip_if_suggest_absent("arrow")
  source <- arrow::arrow_table(data.frame(.data = "x", b = "y"))
  for (name in c("...", "..1", "..42")) {
    query <- expand_with_margins(
      source, .grouping = grouping_set(dplyr::all_of(c("b", ".data"))),
      .id = name
    )
    actual <- dplyr::collect(query)
    expect_identical(names(actual), c("b", ".data", name))
    expect_identical(actual[["b"]], "y")
    expect_identical(actual[[".data"]], "x")
    expect_identical(actual[[name]], 1L)
  }
})

test_that("Arrow dot-dot identifiers cannot shadow private staging columns", {
  skip_if_suggest_absent("arrow")
  source <- arrow::arrow_table(data.frame(
    g = c("b", "a"), ..marginplyr_arrow_id_1 = 1:2,
    ..marginplyr_arrow_id_1_ = 3:4
  ))
  query <- expand_with_margins(
    source, .grouping = rollup(g), .id = "..1", .margin_label = NULL
  )
  actual <- dplyr::collect(query)
  rows <- order(actual[["..1"]], actual[["..marginplyr_arrow_id_1"]])
  expect_identical(names(actual), c(
    "g", "..1", "..marginplyr_arrow_id_1", "..marginplyr_arrow_id_1_"
  ))
  expect_identical(actual[["..1"]][rows], c(1L, 1L, 2L, 2L))
  expect_identical(actual[["..marginplyr_arrow_id_1"]][rows], c(1L, 2L, 1L, 2L))
  expect_identical(actual[["..marginplyr_arrow_id_1_"]][rows],
                   c(3L, 4L, 3L, 4L))
})
