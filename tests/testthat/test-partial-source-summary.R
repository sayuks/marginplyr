check_partial_source_summaries <- function(source, portable = FALSE) {
  included <- grouping_set(tidyselect::all_of("g"))
  plans <- list(NULL, included, rollup(tidyselect::all_of("g")))
  for (output in c(".", ".da", ".dat", ".data")) {
    dots <- stats::setNames(rlang::quos(sum(.data$v, na.rm = TRUE)), output)
    ordinary <- dplyr::summarize(source, !!!dots)
    expect_identical(names(dplyr::collect(ordinary)), output)
    expect_equal(dplyr::collect(ordinary)[[output]], 5)
    for (plan in plans) {
      for (sort in c("none", "first", "last")) {
        for (id in list(NULL, "sid")) {
          if (portable && is.null(id)) next
          query <- summarize_with_margins(
            source, !!!dots, .grouping = plan, .sort = sort, .id = id,
            .duplicates = if (portable) "keep" else "drop"
          )
          expect_s3_class(query, if (inherits(source, "dtplyr_step")) {
            "dtplyr_step"
          } else {
            "tbl_lazy"
          })
          if (inherits(source, "dtplyr_step")) {
            expect_silent(dplyr::show_query(query))
          } else {
            expect_s3_class(dbplyr::sql_render(query), "sql")
          }
          results <- list(dplyr::collect(query))
          # Ordinary SQL compute also refuses an exact `.data` output.
          if (!(output == ".data" && inherits(source, "tbl_sql"))) {
            results <- c(results, list(dplyr::collect(dplyr::compute(query))))
          }
          for (result in results) {
            keys <- if (is.null(plan)) character() else "g"
            expect_identical(names(result), c(keys, id, output))
            if (!is.null(id)) expect_type(result[[id]], "integer")
            expect_equal(sort(result[[output]]), if (is.null(plan)) {
              5
            } else if (identical(plan, included)) {
              c(2, 3)
            } else {
              c(2, 3, 5)
            })
            if (!is.null(plan)) {
              keys <- if (identical(plan, included)) {
                c("a", "b")
              } else {
                c("a", "b", "Total")
              }
              values <- if (identical(plan, included)) c(3, 2) else c(3, 2, 5)
              expect_equal(result[[output]][match(keys, result$g)], values)
            }
            if (!is.null(plan) && sort != "none") {
              expected <- if (identical(plan, included)) {
                c("a", "b")
              } else if (sort == "first") {
                c("Total", "a", "b")
              } else {
                c("a", "b", "Total")
              }
              expect_identical(result$g, expected)
            }
          }
        }
      }
    }
  }
}

test_that("dtplyr retains partial source-formal summary names", {
  skip_if_suggest_absent("dtplyr")
  check_partial_source_summaries(dtplyr::lazy_dt(
    data.frame(g = c("b", "a"), v = c(2, 3))
  ))
})

test_that("SQLite retains partial source-formal summary names", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(con, data.frame(g = c("b", "a"), v = c(2, 3)))
  check_partial_source_summaries(source)
})

test_that("DuckDB retains partial source-formal names on both adapters", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(con, data.frame(g = c("b", "a"), v = c(2, 3)))
  check_partial_source_summaries(source)
  check_partial_source_summaries(source, portable = TRUE)
})
