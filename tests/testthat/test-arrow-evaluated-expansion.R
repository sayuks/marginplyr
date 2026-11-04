test_that("Arrow expansion copies one evaluated payload per input row", {
  skip_if_suggest_absent("arrow")
  source <- arrow::Table$create(
    g = c("a", "b"),
    row_id = 1:2,
    measured = structure(1:2, units = "kg"),
    nested = arrow::Array$create(
      list(1:2, 3L), type = arrow::list_of(arrow::int32())
    )
  )
  draw <- arrow::Expression$create("random")
  source <- dplyr::mutate(source, payload = !!draw)
  query <- expand_with_margins(
    source, .grouping = rollup(g), .id = "occurrence", .sort = "last"
  )
  expect_s3_class(query, "arrow_dplyr_query")

  collect_rows <- function(query) {
    rows <- dplyr::collect(query)
    rows[order(rows$occurrence, rows$row_id), ]
  }
  first <- collect_rows(query)
  second <- collect_rows(query)
  for (rows in list(first, second)) {
    expect_identical(rows$occurrence, c(1L, 1L, 2L, 2L))
    expect_identical(rows$row_id, c(1L, 2L, 1L, 2L))
    expect_identical(rows$payload[1:2], rows$payload[3:4])
    expect_identical(as.list(rows$nested), list(1:2, 3L, 1:2, 3L))
    expect_identical(rows$measured, structure(c(1:2, 1:2), units = "kg"))
  }
  expect_false(identical(first$payload, second$payload))
})

test_that("Arrow expansion keeps volatile row membership across occurrences", {
  skip_if_suggest_absent("arrow")
  source <- arrow::Table$create(g = rep("a", 64L), row_id = 1:64)
  draw <- arrow::Expression$create("random")
  source <- dplyr::filter(source, !!draw < 0.5)
  query <- expand_with_margins(
    source, .grouping = rollup(g), .id = "occurrence"
  )
  expect_s3_class(query, "arrow_dplyr_query")

  rows <- dplyr::collect(query)
  detail <- sort(rows$row_id[rows$occurrence == 1L])
  margin <- sort(rows$row_id[rows$occurrence == 2L])
  expect_identical(detail, margin)
})
