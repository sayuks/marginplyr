test_that("one-set expansion preserves an Arrow fixed-size-list dimension", {
  skip_if_suggest_absent("arrow")
  type <- arrow::fixed_size_list_of(arrow::int32(), 2L)
  for (empty in c(FALSE, TRUE)) {
    values <- if (empty) list() else list(1:2, 3:4)
    rows <- seq_along(values)
    source <- arrow::Table$create(
      g = arrow::Array$create(values, type = type), row_id = rows
    )
    for (label in list(NULL, NA_character_)) {
      query <- expand_with_margins(
        source, .grouping = grouping_set(g), .margin_label = label,
        .id = "occurrence"
      )
      expect_s3_class(query, "arrow_dplyr_query")
      for (result in list(query, dplyr::compute(query))) {
        expect_true(arrow::schema(result)$GetFieldByName("g")$type$Equals(type))
        actual <- dplyr::collect(result)
        actual <- actual[order(actual$row_id), ]
        expect_identical(lapply(seq_along(actual$g), function(i) actual$g[[i]]),
                         values)
        expect_identical(actual$row_id, rows)
        expect_identical(actual$occurrence, rep(1L, length(rows)))
      }
    }
  }
})

test_that("Arrow rollup expansion writes typed missing fixed-size lists", {
  skip_if_suggest_absent("arrow")
  type <- arrow::fixed_size_list_of(arrow::int32(), 2L)
  for (empty in c(FALSE, TRUE)) {
    values <- if (empty) list() else list(1:2, 3:4)
    rows <- seq_along(values)
    source <- arrow::Table$create(
      g = arrow::Array$create(values, type = type), row_id = rows
    )
    for (label in list(NULL, NA_character_)) {
      query <- expand_with_margins(
        source, .grouping = rollup(g), .margin_label = label,
        .id = "occurrence"
      )
      expect_s3_class(query, "arrow_dplyr_query")
      for (result in list(query, dplyr::compute(query))) {
        expect_true(arrow::schema(result)$GetFieldByName("g")$type$Equals(type))
        actual <- dplyr::collect(result)
        actual <- actual[order(actual$occurrence, actual$row_id), ]
        expect_identical(lapply(seq_along(actual$g), function(i) actual$g[[i]]),
                         c(values, rep(list(NULL), length(rows))))
        expect_identical(actual$row_id, rep(rows, 2L))
        expect_identical(actual$occurrence, rep(1:2, each = length(rows)))
      }
    }
  }
})

test_that("fixed-size-list inspection, payload and fixed keys remain valid", {
  skip_if_suggest_absent("arrow")
  type <- arrow::fixed_size_list_of(arrow::int32(), 2L)
  for (empty in c(FALSE, TRUE)) {
    values <- if (empty) list() else list(1:2, 3:4)
    rows <- seq_along(values)
    source <- arrow::Table$create(
      g = arrow::Array$create(values, type = type), row_id = rows
    )
    inspection <- inspect_grouping(source, .grouping = grouping_set(g))
    expect_identical(inspection$included, "(g)")
    expect_identical(inspection$omitted, "()")
    expect_identical(
      inspect_grouping(source, .grouping = grouping_set(where(is.list))),
      inspection
    )
    for (label in list(NULL, NA_character_)) {
      for (fixed in c(FALSE, TRUE)) {
        query <- expand_with_margins(
          source, .by = dplyr::all_of(if (fixed) "g" else character()),
          .grouping = rollup(row_id),
          .margin_label = label, .id = "occurrence"
        )
        expect_s3_class(query, "arrow_dplyr_query")
        for (result in list(query, dplyr::compute(query))) {
          expect_true(
            arrow::schema(result)$GetFieldByName("g")$type$Equals(type)
          )
          actual <- dplyr::collect(result)
          for (occurrence in 1:2) {
            lists <- actual$g[actual$occurrence == occurrence]
            expect_identical(lapply(seq_along(lists), function(i) lists[[i]]),
                             values)
          }
          expect_identical(actual$row_id[actual$occurrence == 1L], rows)
          expect_identical(actual$row_id[actual$occurrence == 2L],
                           rep(NA_integer_, length(rows)))
        }
      }
    }
  }
})
