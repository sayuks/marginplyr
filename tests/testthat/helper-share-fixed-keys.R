# Shared public acceptance for SQLite, DuckDB, and the live PostgreSQL runner.
expect_sql_share_fixed_keys <- function(con) {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  for (fixed in c("fixed", ".env", ".data")) {
    data <- data.frame(
      fixed = rep(c("a", "b", NA_character_), each = 4L),
      g = rep(c("x", "x", "y", "y"), 3L),
      h = rep(c("one", "two"), 6L),
      value = c(1, 3, 2, 2, 2, 4, 6, 12, 3, 3, 6, 12)
    )
    names(data)[[1L]] <- fixed
    remote <- dplyr::copy_to(
      con, data, "share_fixed_keys", temporary = TRUE, overwrite = TRUE
    )
    expected <- data.frame(
      fixed = rep(c("a", "b", NA_character_), each = 7L),
      g = rep(c("x", "x", "x", "y", "y", "y", "Total"), 3L),
      h = rep(c("one", "two", "Total", "one", "two", "Total", "Total"), 3L),
      set = rep(c(1L, 1L, 2L, 1L, 1L, 2L, 3L), 3L),
      amount = c(1, 3, 4, 2, 2, 4, 8, 2, 4, 6, 6, 12, 18, 24,
                 3, 3, 6, 6, 12, 18, 24)
    )
    names(expected)[[1L]] <- fixed
    for (kind in c("none", "parent", "total")) {
      # `amount` names the preceding summary in the contextual helper.
      # nolint start: object_usage_linter.
      shares <- switch(kind,
        none = list(),
        parent = rlang::exprs(share = marginplyr::share_of_parent(amount)),
        total = rlang::exprs(share = marginplyr::share_of_total(amount))
      )
      # nolint end
      query <- marginplyr::summarize_with_margins(
        remote, amount = sum(.data$value, na.rm = TRUE), !!!shares,
        .by = dplyr::all_of(fixed),
        .grouping = marginplyr::rollup(dplyr::all_of("g"), dplyr::all_of("h")),
        .id = "set", .sort = "last", .check_share_source = FALSE
      )
      info <- paste(fixed, kind)
      testthat::expect_s3_class(query, "tbl_lazy")
      sent <- marginplyr::last_sent_queries()
      testthat::expect_identical(tail(sent$purpose, 1L), "result", info = info)
      before_result <- head(sent$sql, -1L)
      reads_input <- grepl("share_fixed_keys", before_result, fixed = TRUE)
      testthat::expect_true(all(grepl(
        "LIMIT 0", before_result[reads_input], fixed = TRUE
      )), info = info)
      actual <- as.data.frame(dplyr::collect(query))
      testthat::expect_identical(
        names(actual), c(names(expected), names(shares)), info = info
      )
      testthat::expect_equal(actual[names(expected)], expected, info = info)
      if (kind != "none") {
        ratios <- if (kind == "parent") {
          c(1 / 4, 3 / 4, 1 / 2, 1 / 2, 1 / 2, 1 / 2, 1,
            1 / 3, 2 / 3, 1 / 4, 1 / 3, 2 / 3, 3 / 4, 1,
            1 / 2, 1 / 2, 1 / 4, 1 / 3, 2 / 3, 3 / 4, 1)
        } else {
          c(1 / 8, 3 / 8, 1 / 2, 1 / 4, 1 / 4, 1 / 2, 1,
            1 / 12, 1 / 6, 1 / 4, 1 / 4, 1 / 2, 3 / 4, 1,
            1 / 8, 1 / 8, 1 / 4, 1 / 4, 1 / 2, 3 / 4, 1)
        }
        testthat::expect_equal(actual$share, ratios, info = info)
        testthat::expect_type(actual$share, "double")
      }
    }
  }
}
