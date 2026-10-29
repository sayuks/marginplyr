test_that("SQLite expansion preserves an empty-first ordered input LIMIT", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("c", "b", "a"), v = 1:3), "facts"
  )
  selected <- head(dplyr::arrange(source, .data$v), 2L)
  query <- expand_with_margins(
    selected, .grouping = grouping_sets(grouping_set(), grouping_set("g")),
    .margin_label = NULL
  )
  expected <- data.frame(g = c(NA_character_, NA_character_, "b", "c"),
                         v = c(1L, 2L, 2L, 1L))
  result <- dplyr::arrange(dplyr::collect(query), .data$g, .data$v)
  expected <- dplyr::arrange(expected, .data$g, .data$v)
  expect_equal(as.data.frame(result), expected)
})

test_that("SQLite expansion retains ordered and limited input rows", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  check_expansion_input_limits(con)
  check_expansion_shapes(con)
})

test_that("DuckDB expansion retains ordered and limited input rows", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  check_expansion_input_limits(con)
  check_expansion_shapes(con)
})

test_that("SQLite expansion retains Boolean-word input columns", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (key in c("TRUE", "FALSE")) {
    data <- data.frame(g = c("c", "b", "a"), key = c("x", "y", "z"), v = 1:3)
    names(data)[[2L]] <- key
    source <- dplyr::copy_to(con, data, paste0("boolean_facts_", key))
    selected <- head(dplyr::arrange(source, .data$v), 2L)
    for (input in list(source, selected)) {
      local <- dplyr::collect(input)
      for (role in c("dimension", "payload", "fixed")) {
        dimension <- if (role == "dimension") key else "g"
        spec <- rollup(dplyr::all_of(dimension))
        by <- if (role == "fixed") key else NULL
        for (label in list(NULL, "Total")) {
          for (sort in c("none", "last")) {
            expected <- expand_with_margins(
              local, .by = dplyr::all_of(by), .grouping = spec,
              .margin_label = label, .id = "set", .sort = sort
            )
            query <- expand_with_margins(
              input, .by = dplyr::all_of(by), .grouping = spec,
              .margin_label = label, .id = "set", .sort = sort
            )
            expect_selected_expansion(query, expected, "none")
          }
        }
      }
    }
  }
})
