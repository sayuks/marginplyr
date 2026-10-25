# DESCRIBE observes SQL types that collection alone cannot distinguish:
# TIME and INTERVAL both arrive as difftime, and BLOB arrives as a list.
duckdb_scalar_column_type <- function(query) {
  schema <- DBI::dbGetQuery(
    dbplyr::remote_con(query),
    paste("DESCRIBE", dbplyr::sql_render(query))
  )
  schema$column_type[schema$column_name == "g"]
}

duckdb_scalar_cases <- function() {
  list(
    TIME = list(
      sql = "TIME '12:30:00'", value = as.difftime(45000, units = "secs"),
      missing = as.difftime(NA_real_, units = "secs")
    ),
    INTERVAL = list(
      sql = "INTERVAL '2 days 3 hours'",
      value = as.difftime(183600, units = "secs"),
      missing = as.difftime(NA_real_, units = "secs")
    ),
    BLOB = list(
      sql = "'abc'::BLOB", value = list(charToRaw("abc")), missing = list(NULL)
    )
  )
}

test_that("DuckDB scalar expansion preserves values and SQL types", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  for (type in names(duckdb_scalar_cases())) {
    case <- duckdb_scalar_cases()[[type]]
    source <- dplyr::tbl(con, dbplyr::sql(paste0(
      "SELECT ", case$sql, " AS g, 'east' AS fixed, 7 AS value"
    )))
    input <- dplyr::collect(source)
    expect_identical(input$g, case$value, info = type)
    expect_identical(duckdb_scalar_column_type(source), type)
    plan <- inspect_grouping(source, .by = fixed, .grouping = rollup(g))
    expect_identical(plan$included, c("(g)", "()"))

    control <- dplyr::union_all(source, dplyr::mutate(source, g = NA))
    expected <- dplyr::collect(control)
    expect_identical(nrow(expected), 2L)
    expect_identical(expected$g[1L], case$value)
    expect_identical(expected$g[2L], case$missing)

    for (label in list(NULL, NA_character_)) {
      summary <- summarize_with_margins(
        source, rows = dplyr::n(), .by = fixed, .grouping = rollup(g),
        .margin_label = label, .id = "set id"
      )
      summary <- dplyr::collect(dplyr::arrange(summary, .data[["set id"]]))
      expect_identical(summary$g, expected$g)
      expect_equal(summary$rows, c(1, 1))

      query <- expand_with_margins(
        source, .by = fixed, .grouping = rollup(g),
        .margin_label = label, .id = "set id"
      )
      expect_s3_class(query, "tbl_lazy")
      expect_identical(duckdb_scalar_column_type(query), type)
      stored <- dplyr::compute(query)
      expect_identical(duckdb_scalar_column_type(stored), type)
      for (result in list(query, stored)) {
        result <- dplyr::collect(dplyr::arrange(result, .data[["set id"]]))
        expect_identical(result$g, expected$g)
        expect_identical(result$fixed, c("east", "east"))
        expect_identical(result$value, c(7L, 7L))
        expect_identical(result[["set id"]], 1:2)
      }
    }
  }
})

test_that("DuckDB scalar expansion keeps empty schemas and repeated rows", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  for (type in names(duckdb_scalar_cases())) {
    case <- duckdb_scalar_cases()[[type]]
    source <- dplyr::tbl(con, dbplyr::sql(paste0(
      "SELECT ", case$sql, " AS g, 'east' AS fixed, 7 AS value"
    )))
    source <- dplyr::union_all(source, source)
    input <- dplyr::collect(source)
    plans <- list(
      grouping_set(g),
      grouping_sets(grouping_set(), grouping_set(g), grouping_set(g))
    )
    for (label in list(NULL, NA_character_)) {
      for (index in seq_along(plans)) {
        for (empty in c(FALSE, TRUE)) {
          data <- if (empty) dplyr::filter(source, FALSE) else source
          query <- expand_with_margins(
            data, .by = fixed, .grouping = plans[[index]],
            .duplicates = "keep", .margin_label = label, .id = "set id"
          )
          expect_s3_class(query, "tbl_lazy")
          expect_identical(duckdb_scalar_column_type(query), type)
          stored <- dplyr::compute(query)
          expect_identical(duckdb_scalar_column_type(stored), type)
          for (result in list(query, stored)) {
            result <- dplyr::collect(dplyr::arrange(result, .data[["set id"]]))
            expect_identical(names(result), c("fixed", "g", "set id", "value"))
            if (empty) {
              expect_identical(result$g, input$g[integer()])
              expect_identical(result[["set id"]], integer())
            } else if (index == 1L) {
              expect_identical(result$g, input$g)
              expect_identical(result[["set id"]], c(1L, 1L))
              expect_identical(result$value, c(7L, 7L))
            } else {
              expect_identical(result$g[1:2], rep(case$missing, 2L))
              expect_identical(result$g[3:6], rep(input$g, 2L))
              expect_identical(result[["set id"]], rep(1:3, each = 2L))
              expect_identical(result$value, rep(7L, 6L))
            }
            expect_identical(result$fixed, rep("east", nrow(result)))
          }
        }
      }
    }
  }
})
