# The cells of the database guide's *Backend support matrix* and *Converted
# dimension text* tables that rest on a backend's behaviour rather than on
# marginplyr's: the text a backend's own cast gives a converted dimension, and
# whether it reports column types to a `where()` predicate. Each backend has a
# test of its own, so none requires more than one member of
# `optional_backends()`.

matrix_dimensions <- function() {
  data.frame(
    number = 10,
    flag = TRUE,
    day = as.Date("2024-01-01"),
    count = 10L
  )
}

# The text each dimension carries on its one non-margin row, from a rollup of
# that dimension alone under the default character `.margin_label`.
converted_text <- function(input) {
  dimensions <- c("number", "flag", "day", "count")
  vapply(dimensions, function(dimension) {
    result <- dplyr::collect(summarize_with_margins(
      input,
      rows = dplyr::n(),
      .grouping = rollup(dplyr::all_of(dimension))
    ))
    text <- result[[dimension]]
    text[text != "Total"]
  }, character(1))
}

# Whether a type predicate in `.grouping` is answered. Only the refusal for a
# backend that reports no column types counts as `FALSE`; any other error
# fails the test.
predicate_accepted <- function(input) {
  tryCatch(
    {
      summarize_with_margins(
        input,
        rows = dplyr::n(),
        .grouping = rollup(dplyr::where(is.logical))
      )
      TRUE
    },
    marginplyr_error = function(error) {
      if (!grepl("doesn't report column types", conditionMessage(error))) {
        stop(error)
      }
      FALSE
    }
  )
}

test_that("a local converted dimension keeps R's own text", {
  expect_identical(
    converted_text(matrix_dimensions()),
    c(number = "10", flag = "TRUE", day = "2024-01-01", count = "10")
  )
  expect_true(predicate_accepted(matrix_dimensions()))
})

test_that("a dtplyr converted dimension matches the local text", {
  skip_if_suggest_absent("dtplyr")
  input <- dtplyr::lazy_dt(matrix_dimensions())
  expect_identical(
    converted_text(input),
    c(number = "10", flag = "TRUE", day = "2024-01-01", count = "10")
  )
  expect_true(predicate_accepted(input))
})

test_that("a DuckDB converted dimension carries DuckDB's cast to text", {
  skip_if_suggest_absent("duckdb")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  input <- dplyr::copy_to(con, matrix_dimensions(), "matrix_dimensions")
  expect_identical(
    converted_text(input),
    c(number = "10.0", flag = "true", day = "2024-01-01", count = "10")
  )
  expect_true(predicate_accepted(input))
})

test_that("a SQLite converted dimension carries SQLite's cast to text", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- dplyr::copy_to(con, matrix_dimensions(), "matrix_dimensions")
  expect_identical(
    converted_text(input),
    c(number = "10.0", flag = "1", day = "19723.0", count = "10")
  )
  expect_false(predicate_accepted(input))
})

test_that("an Arrow converted dimension carries Arrow's cast to text", {
  skip_if_suggest_absent("arrow")
  input <- arrow::arrow_table(matrix_dimensions())
  expect_identical(
    converted_text(input),
    c(number = "10", flag = "true", day = "2024-01-01", count = "10")
  )
  expect_true(predicate_accepted(input))
})
