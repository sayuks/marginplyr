test_that("SQLite Boolean-word keys survive input limits and ordering", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  for (key in list(c("a", "b"), c(NA_integer_, NA_integer_))) {
    source <- dplyr::copy_to(
      con, data.frame(`TRUE` = key, v = 2:3, check.names = FALSE),
      paste0("boolean_input_", typeof(key))
    )
    for (limited in c(FALSE, TRUE)) {
      input <- if (limited) utils::head(source, 1L) else
        dplyr::arrange(source, `TRUE`)
      for (sort in c("none", "last")) {
        query <- summarize_with_margins(
          input, `FALSE` = sum(v, na.rm = TRUE),
          .grouping = rollup(`TRUE`), .id = "set",
          .margin_label = NULL, .sort = sort
        )
        expected_key <- if (limited) key[1L] else unique(key)
        expected_value <- if (limited) 2L else
          if (is.integer(key)) 5L else c(2L, 3L)
        expected <- data.frame(
          `TRUE` = c(expected_key, key[NA_integer_]),
          set = c(rep(1L, length(expected_key)), 2L),
          `FALSE` = c(expected_value, if (limited) 2L else 5L),
          check.names = FALSE
        )
        for (result in list(dplyr::collect(query),
                            dplyr::collect(dplyr::compute(query)))) {
          actual <- as.data.frame(result)
          if (identical(sort, "none")) {
            actual <- actual[vctrs::vec_order(actual), , drop = FALSE]
            expected <- expected[vctrs::vec_order(expected), , drop = FALSE]
            rownames(actual) <- rownames(expected) <- NULL
          }
          expect_identical(actual, expected)
        }
      }
    }
  }
})

test_that("SQLite Boolean-word source values need no public source column", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), `TRUE` = 2:3, check.names = FALSE),
    "boolean_input_payload"
  )
  query <- summarize_with_margins(
    dplyr::arrange(source, `TRUE`), `FALSE` = sum(.data$`TRUE`, na.rm = TRUE),
    .grouping = rollup(g), .sort = "last"
  )
  expected <- data.frame(g = c("a", "b", "Total"), `FALSE` = c(2L, 3L, 5L),
                         check.names = FALSE)
  for (result in list(dplyr::collect(query),
                      dplyr::collect(dplyr::compute(query)))) {
    expect_identical(as.data.frame(result), expected)
  }
})
