test_that("DuckDB rollups preserve wide numeric share denominators", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  cases <- list(
    "DECIMAL(38,0)" = list(
      maximum = "99999999999999999999999999999999999999", small_share = 1e-38
    ),
    HUGEINT = list(
      maximum = "170141183460469231731687303715884105727",
      small_share = 5.877471754111438e-39
    ),
    UHUGEINT = list(
      maximum = "340282366920938463463374607431768211455",
      small_share = 2.938735877055719e-39
    )
  )

  for (type in names(cases)) {
    case <- cases[[type]]
    source <- dplyr::tbl(con, dbplyr::sql(paste0(
      "SELECT part, g, CASE WHEN nextval('wide_share_read') > 0 THEN ",
      "CAST(v AS ", type, ") END AS amount FROM (VALUES ",
      "('wide', 'a', '", case$maximum, "'), ('wide', 'b', '1'), ",
      "('wide', 'c', NULL), ('zero', 'a', '0'), ('missing', 'a', NULL)",
      ") AS input(part, g, v)"
    )), vars = c("part", "g", "amount"))

    for (duplicates in c("error", "keep")) {
      DBI::dbExecute(con, "CREATE OR REPLACE SEQUENCE wide_share_read")
      query <- summarize_with_margins(
        source, value = max(amount, na.rm = TRUE),
        parent = share_of_parent(value), total = share_of_total(value),
        .by = part, .grouping = rollup(g), .duplicates = duplicates,
        .id = "set", .margin_label = NULL
      )
      expect_s3_class(query, "tbl_lazy")
      expect_match(
        dbplyr::sql_render(query),
        if (duplicates == "keep") "UNION ALL" else "GROUPING SETS",
        fixed = TRUE
      )
      expect_error(DBI::dbGetQuery(con, "SELECT currval('wide_share_read')"),
                   "not yet defined")

      result <- dplyr::collect(query) |>
        dplyr::arrange(part, set, g)
      ordinary <- summarize_with_margins(
        source, value = max(amount, na.rm = TRUE),
        .by = part, .grouping = rollup(g), .duplicates = duplicates,
        .id = "set", .margin_label = NULL
      ) |>
        dplyr::collect() |>
        dplyr::arrange(part, set, g)
      expect_identical(result[names(ordinary)], ordinary)

      small <- result$part == "wide" & result$set == 1L & result$g == "b"
      for (ratio in list(result$parent, result$total)) {
        expect_type(ratio, "double")
        # Compare on a unit scale so a zero ratio cannot pass by tolerance.
        expect_equal(ratio[small] / case$small_share, 1)
        expect_identical(
          ratio[!small], c(NA_real_, 1, 1, NA_real_, 1, NA_real_, 1)
        )
      }
    }
  }
})
