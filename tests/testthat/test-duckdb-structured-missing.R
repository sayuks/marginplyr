# Collection maps STRUCT to packed frames and ARRAY to matrices; DESCRIBE
# establishes the SQL key type independently of those R representations.
duckdb_structured_type <- function(query) {
  schema <- DBI::dbGetQuery(
    dbplyr::remote_con(query), paste("DESCRIBE", dbplyr::sql_render(query))
  )
  schema$column_type[schema$column_name == "g"]
}

duckdb_structured_cases <- function() {
  list(
    struct = list(expr = "struct_pack(x := v)", type = "STRUCT(x INTEGER)"),
    array = list(expr = "CAST([v, v + 1] AS INTEGER[2])", type = "INTEGER[2]"),
    scalar = list(expr = "v", type = "INTEGER")
  )
}

test_that("DuckDB portable structured rollups match native and typed NULL", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection(array = "matrix")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  for (case in duckdb_structured_cases()) {
    for (empty in c(FALSE, TRUE)) {
      sql <- paste(
        "SELECT", case$expr, "AS g, v AS row_id",
        "FROM (VALUES (2), (1)) t(v)", if (empty) "WHERE FALSE" else ""
      )
      source <- dplyr::tbl(con, dbplyr::sql(sql))
      ordinary <- dplyr::collect(dplyr::summarise(
        source, rows = dplyr::n(), .by = g
      ))
      expect_equal(ordinary$rows, rep(1, if (empty) 0L else 2L))
      control <- DBI::dbGetQuery(con, paste0(
        "SELECT g, COUNT(*) AS rows, 1 AS sid FROM (", sql, ") t GROUP BY g ",
        "UNION ALL SELECT NULL::", case$type,
        ", COUNT(*), 2 FROM (", sql, ") t"
      ))
      for (label in list(NULL, NA_character_)) {
        for (sort in c("none", "first", "last")) {
          for (duplicates in c("error", "drop", "keep")) {
            query <- summarize_with_margins(
              source, rows = dplyr::n(), .grouping = rollup(g),
              .margin_label = label, .id = "sid",
              .duplicates = duplicates, .sort = sort
            )
            expect_s3_class(query, "tbl_lazy")
            expect_match(
              as.character(dbplyr::sql_render(query)),
              if (duplicates == "keep") "UNION ALL" else "GROUPING SETS",
              fixed = TRUE
            )
            for (result in list(query, dplyr::compute(query))) {
              expect_identical(duckdb_structured_type(result), case$type)
              actual <- dplyr::collect(result)
              missing <- dplyr::collect(dplyr::transmute(
                result, sid, missing = is.na(g)
              ))
              expect_identical(missing$missing, missing$sid == 2L)
              detail <- actual$sid == 1L
              expect_equal(actual$rows[detail], ordinary$rows)
              expect_equal(actual$rows[!detail], if (empty) 0 else 2)
              expect_equal(
                vctrs::vec_sort(vctrs::vec_slice(actual$g, which(detail))),
                vctrs::vec_sort(ordinary$g)
              )
              expect_equal(
                vctrs::vec_slice(actual$g, which(!detail)),
                vctrs::vec_slice(control$g, which(control$sid == 2L))
              )
              expect_identical(
                sort(actual$sid), if (empty) 2L else c(1L, 1L, 2L)
              )
              if (sort != "none") {
                expected_ids <- if (empty) 2L else if (sort == "first") {
                  c(2L, 1L, 1L)
                } else {
                  c(1L, 1L, 2L)
                }
                expect_identical(actual$sid, expected_ids)
                expect_equal(
                  vctrs::vec_slice(actual$g, which(detail)),
                  vctrs::vec_sort(ordinary$g)
                )
              }
            }
          }
        }
      }
    }
  }
})

test_that("DuckDB structured expansion retains types, values and order", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection(array = "matrix")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  for (case in duckdb_structured_cases()) {
    for (empty in c(FALSE, TRUE)) {
      sql <- paste(
        "SELECT", case$expr, "AS g, v AS row_id",
        "FROM (VALUES (2), (1)) t(v)", if (empty) "WHERE FALSE" else ""
      )
      source <- dplyr::tbl(con, dbplyr::sql(sql))
      control <- DBI::dbGetQuery(con, paste0(
        "SELECT g, row_id, 1 AS sid FROM (", sql, ") t UNION ALL ",
        "SELECT NULL::", case$type, ", row_id, 2 FROM (", sql, ") t"
      ))
      control <- tibble::as_tibble(control)
      control <- control[order(control$sid, control$row_id), ]
      for (label in list(NULL, NA_character_)) {
        included <- expand_with_margins(
          source, .grouping = grouping_set(g), .margin_label = label
        )
        expect_equal(dplyr::collect(included), dplyr::collect(source))
        for (sort in c("none", "first", "last")) {
          query <- expand_with_margins(
            source, .grouping = rollup(g), .margin_label = label,
            .id = "sid", .sort = sort
          )
          expect_s3_class(query, "tbl_lazy")
          for (result in list(query, dplyr::compute(query))) {
            expect_identical(duckdb_structured_type(result), case$type)
            actual <- dplyr::collect(result)
            missing <- dplyr::collect(dplyr::transmute(
              result, sid, missing = is.na(g)
            ))
            expect_identical(missing$missing, missing$sid == 2L)
            if (sort != "none") {
              ids <- rep(if (sort == "first") 2:1 else 1:2, each = 2L)
              expect_identical(actual$sid, if (empty) integer() else ids)
              expect_identical(actual$row_id[actual$sid == 1L],
                               if (empty) integer() else 1:2)
            }
            actual <- actual[order(actual$sid, actual$row_id), ]
            expect_equal(actual$g, control$g)
            expect_identical(actual$row_id, control$row_id)
            expect_identical(actual$sid, control$sid)
          }
        }
      }
    }
  }
})

test_that("DuckDB structured keys retain their non-missing label refusal", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection(array = "matrix")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  for (expr in c("struct_pack(x := 1)", "CAST([1, 2] AS INTEGER[2])")) {
    source <- dplyr::tbl(con, dbplyr::sql(paste("SELECT", expr, "AS g")))
    expect_error(
      expand_with_margins(source, .grouping = rollup(g)),
      "non-missing", class = "marginplyr_error"
    )
    expect_error(
      summarize_with_margins(
        source, rows = dplyr::n(), .grouping = rollup(g),
        .id = "sid", .duplicates = "keep"
      ),
      "non-missing", class = "marginplyr_error"
    )
  }
})
