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

expect_selected_expansion <- function(query, expected, sort) {
  canonical <- function(data) {
    dplyr::arrange(as.data.frame(data), !!!rlang::syms(names(expected)))
  }
  for (result in list(
    dplyr::collect(query),
    dplyr::collect(dplyr::compute(query, temporary = TRUE, analyze = FALSE))
  )) {
    expect_identical(names(result), names(expected))
    expect_equal(canonical(result), canonical(expected))
    if (sort != "none") {
      expect_identical(result$g, expected$g)
    }
  }
  for (n in c(0L, 3L, 4L, 5L)) {
    result <- dplyr::collect(query, n = n)
    expect_identical(names(result), names(expected))
    expect_identical(nrow(result), min(n, nrow(expected)))
    if (n == 0L) {
      if ("set" %in% names(expected)) {
        expect_identical(result$set, integer())
      }
    } else if (n == 3L) {
      expect_identical(nrow(dplyr::anti_join(
        result, expected, by = names(expected), na_matches = "na"
      )), 0L)
      expect_identical(anyDuplicated(result), 0L)
    } else {
      expect_equal(canonical(result), canonical(expected))
    }
  }
}

check_expansion_input_limits <- function(con) {
  source <- dplyr::copy_to(
    con, data.frame(g = c("c", "b", "a"), v = 1:3), "facts"
  )
  selected <- head(dplyr::arrange(source, .data$v), 2L)
  local <- dplyr::collect(selected)
  expect_identical(local$v, 1:2)
  specifications <- list(
    grouping_sets(grouping_set(), grouping_set("g")),
    grouping_sets(grouping_set("g"), grouping_set()),
    rollup("g")
  )
  materialized <- dplyr::compute(selected, temporary = TRUE, analyze = FALSE)
  for (spec in specifications) {
    for (label in list(NULL, NA_character_, "Total")) {
      for (id in list(NULL, "set")) {
        for (sort in c("none", "first", "last")) {
          expected <- expand_with_margins(
            local, .grouping = spec, .margin_label = label,
            .id = id, .sort = sort
          )
          for (input in list(selected, dplyr::collapse(selected))) {
            query <- expand_with_margins(
              input, .grouping = spec, .margin_label = label,
              .id = id, .sort = sort
            )
            expect_selected_expansion(query, expected, sort)
          }
        }
      }
    }
    control <- expand_with_margins(
      materialized, .grouping = spec, .margin_label = NULL, .id = "set"
    )
    expect_selected_expansion(control, expand_with_margins(
      local, .grouping = spec, .margin_label = NULL, .id = "set"
    ), "none")
  }

  windowed <- head(dplyr::mutate(
    dplyr::arrange(source, .data$v), rank = dplyr::row_number()
  ), 2L)
  query <- expand_with_margins(
    windowed, .grouping = rollup("g"), .margin_label = NULL, .id = "set"
  )
  expect_selected_expansion(query, expand_with_margins(
    dplyr::collect(windowed), .grouping = rollup("g"),
    .margin_label = NULL, .id = "set"
  ), "none")

  one_set <- expand_with_margins(
    selected, .grouping = grouping_set("g"), .margin_label = NULL
  )
  ranked <- dplyr::arrange(dplyr::collect(
    dplyr::mutate(one_set, rank = dplyr::row_number())
  ), .data$rank)
  expect_identical(ranked$g, c("c", "b"))
  expect_equal(ranked$rank, 1:2)
}

test_that("SQLite expansion retains ordered and limited input rows", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  check_expansion_input_limits(con)
})

test_that("DuckDB expansion retains ordered and limited input rows", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  check_expansion_input_limits(con)
})
