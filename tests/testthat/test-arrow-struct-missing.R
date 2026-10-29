# The fixture distinguishes a null parent from an all-null, valid STRUCT.
# Native conditional assignment supplies the source null before the verb runs.
arrow_struct_table <- function(empty = FALSE) {
  type <- arrow::struct(x = arrow::int16(), y = arrow::utf8())
  null <- arrow::Scalar$create(NA, type = arrow::null())$cast(type)
  table <- arrow::Table$create(
    g = arrow::Array$create(
      data.frame(x = c(2L, 1L, NA, NA), y = c("b", "a", NA, NA)),
      type = type
    ),
    fixed = arrow::Array$create(
      data.frame(x = rep(7L, 4L), y = rep(NA_character_, 4L)), type = type
    ),
    row_id = 1:4
  )
  table <- dplyr::mutate(
    table, g = dplyr::if_else(.data$row_id == 3L, null, .data$g),
    payload = .data$g
  )
  if (empty) {
    table <- dplyr::filter(table, .data$row_id < 0L)
  }
  dplyr::compute(table)
}

arrow_struct_sources <- function(table) {
  dataset <- arrow::InMemoryDataset$create(table)
  list(
    table = table,
    batch = arrow::as_record_batch(table),
    dataset = dataset,
    table_query = dplyr::select(table, dplyr::everything()),
    dataset_query = dplyr::select(dataset, dplyr::everything())
  )
}

test_that("Arrow omitted STRUCT dimensions have null parents and exact types", {
  skip_if_suggest_absent("arrow")
  for (empty in c(FALSE, TRUE)) {
    table <- arrow_struct_table(empty)
    type <- arrow::schema(table)$GetFieldByName("g")$type
    rows <- if (empty) integer() else 1:4
    native_null <- arrow::Scalar$create(NA, type = arrow::null())$cast(type)
    control <- dplyr::mutate(table, g = native_null, missing = is.na(g))
    expect_identical(dplyr::collect(control)$missing, rep(TRUE, length(rows)))
    expect_true(arrow::schema(control)$GetFieldByName("g")$type$Equals(type))
    for (source in arrow_struct_sources(table)) {
      for (label in list(NULL, NA_character_)) {
        for (sort in c("none", "first", "last")) {
          query <- expand_with_margins(
            source, .by = fixed, .grouping = rollup(g),
            .margin_label = label, .id = "sid", .sort = sort
          )
          expect_s3_class(query, "arrow_dplyr_query")
          for (result in list(query, dplyr::compute(query))) {
            for (column in c("g", "fixed", "payload")) {
              expect_true(
                arrow::schema(result)$GetFieldByName(column)$type$Equals(type)
              )
            }
            actual <- dplyr::collect(result)
            if (sort != "none") {
              ids <- rep(if (sort == "first") 2:1 else 1:2, each = 4L)
              expect_identical(actual$sid, if (empty) integer() else ids)
              expect_identical(actual$row_id[actual$sid == 1L],
                               if (empty) integer() else c(2L, 1L, 4L, 3L))
            }
            actual <- actual[order(actual$sid, actual$row_id), ]
            expect_identical(actual$sid, rep(1:2, each = length(rows)))
            expect_identical(actual$row_id, rep(rows, 2L))
            expected_x <- if (empty) integer() else c(2L, 1L, NA, NA)
            expected_y <- if (empty) character() else c("b", "a", NA, NA)
            expect_identical(
              actual$g$x, c(expected_x, rep(NA_integer_, length(rows)))
            )
            expect_identical(
              actual$g$y, c(expected_y, rep(NA_character_, length(rows)))
            )
            expect_identical(actual$payload$x, rep(expected_x, 2L))
            expect_identical(actual$payload$y, rep(expected_y, 2L))
            expect_identical(actual$fixed$x, rep(7L, 2L * length(rows)))
            expect_identical(
              actual$fixed$y, rep(NA_character_, 2L * length(rows))
            )

            # R collection erases parent validity; observe it in Arrow first.
            validity <- dplyr::collect(dplyr::mutate(
              result, missing = is.na(g), fixed_missing = is.na(fixed),
              payload_missing = is.na(payload)
            ))
            validity <- validity[order(validity$sid, validity$row_id), ]
            source_missing <- if (empty) {
              logical()
            } else {
              c(FALSE, FALSE, TRUE, FALSE)
            }
            expect_identical(
              validity$missing, c(source_missing, rep(TRUE, length(rows)))
            )
            expect_identical(
              validity$fixed_missing, rep(FALSE, 2L * length(rows))
            )
            expect_identical(validity$payload_missing, rep(source_missing, 2L))
            missing <- dplyr::collect(dplyr::filter(result, is.na(g)))
            expect_identical(
              sort(missing$sid), if (empty) integer() else c(1L, rep(2L, 4L))
            )
            expect_identical(
              missing$row_id[missing$sid == 1L], if (empty) integer() else 3L
            )
          }
        }
      }
    }
  }
})

test_that("Arrow included-only STRUCT expansion preserves parent validity", {
  skip_if_suggest_absent("arrow")
  for (empty in c(FALSE, TRUE)) {
    table <- arrow_struct_table(empty)
    type <- arrow::schema(table)$GetFieldByName("g")$type
    for (source in arrow_struct_sources(table)) {
      for (label in list(NULL, NA_character_)) {
        query <- expand_with_margins(
          source, .by = fixed, .grouping = grouping_set(g),
          .margin_label = label, .id = "sid"
        )
        for (result in list(query, dplyr::compute(query))) {
          expect_true(
            arrow::schema(result)$GetFieldByName("g")$type$Equals(type)
          )
          actual <- dplyr::collect(dplyr::mutate(result, missing = is.na(g)))
          actual <- actual[order(actual$row_id), ]
          expect_identical(
            actual$missing,
            if (empty) logical() else c(FALSE, FALSE, TRUE, FALSE)
          )
          expect_identical(actual$sid, rep(1L, if (empty) 0L else 4L))
          expect_equal(actual$g, dplyr::collect(table)$g)
        }
      }
    }
  }
})
