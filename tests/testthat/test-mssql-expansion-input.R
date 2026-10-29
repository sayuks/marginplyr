# SQL Server coverage here establishes generated SQL, not live execution.
test_that("SQL Server expansion drops unbounded source ORDER BY", {
  source <- dplyr::tbl(
    dbplyr::simulate_mssql(), "facts", vars = c("g", "v")
  )
  ordered <- dplyr::arrange(source, dplyr::desc(.data$v))
  expect_match(dbplyr::sql_render(ordered), "ORDER BY [v] DESC", fixed = TRUE)

  for (spec in list(NULL, grouping_set("g"), rollup("g"))) {
    query <- expand_with_margins(ordered, .grouping = spec)
    expect_no_match(dbplyr::sql_render(query), "ORDER BY", fixed = TRUE)
    expect_identical(as.character(dplyr::tbl_vars(query)), c("g", "v"))
    ranked <- dplyr::mutate(query, rank = dplyr::row_number())
    expect_match(
      dbplyr::sql_render(ranked),
      "ROW_NUMBER() OVER (ORDER BY [v] DESC)", fixed = TRUE
    )
  }
  expect_match(dbplyr::sql_render(ordered), "ORDER BY [v] DESC", fixed = TRUE)
})

test_that("SQL Server expansion preserves TOP selection and its ordering", {
  source <- dplyr::tbl(
    dbplyr::simulate_mssql(), "facts", vars = c("g", "v")
  )
  ordered <- dplyr::arrange(source, dplyr::desc(.data$v))
  selected <- head(ordered, 2L)
  inputs <- list(
    unordered = source, ordered = ordered,
    limited = head(source, 2L), ordered_limited = selected,
    collapsed = dplyr::collapse(selected),
    reordered = dplyr::arrange(selected, .data$g)
  )

  for (name in names(inputs)) {
    for (spec in list(NULL, grouping_set("g"), rollup("g"))) {
      query <- expand_with_margins(inputs[[name]], .grouping = spec)
      sql <- gsub("\\s+", " ", as.character(dbplyr::sql_render(query)))
      if (name %in% c("unordered", "ordered")) {
        expect_no_match(sql, "TOP", fixed = TRUE)
        expect_no_match(sql, "ORDER BY", fixed = TRUE)
      } else {
        expect_match(sql, "SELECT TOP 2 * FROM [facts]", fixed = TRUE)
        if (name == "limited") {
          expect_no_match(sql, "ORDER BY", fixed = TRUE)
        } else {
          expect_match(
            sql, "SELECT TOP 2 * FROM [facts] ORDER BY [v] DESC",
            fixed = TRUE
          )
          expect_no_match(sql, "ORDER BY [g]", fixed = TRUE)
        }
      }
      expect_identical(as.character(dplyr::tbl_vars(query)), c("g", "v"))
    }
  }
})

test_that("SQL Server expansion preserves an opaque OFFSET/FETCH input", {
  selection <- paste(
    "SELECT [g], [v] FROM [facts] ORDER BY [v] DESC",
    "OFFSET 1 ROWS FETCH NEXT 2 ROWS ONLY"
  )
  source <- dplyr::tbl(
    dbplyr::simulate_mssql(), dbplyr::sql(selection), vars = c("g", "v")
  )
  source <- dbplyr::window_order(source, dplyr::desc(.data$v))
  for (spec in list(NULL, grouping_set("g"), rollup("g"))) {
    query <- expand_with_margins(source, .grouping = spec)
    sql <- gsub("\\s+", " ", as.character(dbplyr::sql_render(query)))
    expect_match(sql, selection, fixed = TRUE)
    expect_no_match(sql, "TOP", fixed = TRUE)
    ranked <- dplyr::mutate(query, rank = dplyr::row_number())
    expect_match(
      dbplyr::sql_render(ranked),
      "ROW_NUMBER() OVER (ORDER BY [v] DESC)", fixed = TRUE
    )
  }
})

test_that("SQL Server expansion applies Margin order outside its source", {
  source <- dplyr::tbl(
    dbplyr::simulate_mssql(), "facts", vars = c("g", "v")
  ) |>
    dplyr::arrange(dplyr::desc(.data$v))
  for (sort in c("first", "last")) {
    query <- expand_with_margins(
      source, .grouping = rollup("g"), .id = "set", .sort = sort
    )
    sql <- gsub("\\s+", " ", as.character(dbplyr::sql_render(query)))
    expect_length(gregexpr("ORDER BY", sql, fixed = TRUE)[[1L]], 1L)
    expect_match(sql, "\\) AS \\[[^]]+\\] ORDER BY ", perl = TRUE)
    bit <- "CASE WHEN ([set] IN (2)) THEN 1 WHEN NOT ([set] IN (2)) THEN 0 END"
    expect_match(sql, paste0(
      " ORDER BY ", bit, if (sort == "first") " DESC" else "", ","
    ), fixed = TRUE)
    expect_identical(as.character(dplyr::tbl_vars(query)), c("g", "set", "v"))
    expect_length(dbplyr::op_sort(query), 0L)
  }
})
