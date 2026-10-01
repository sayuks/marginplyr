test_that("SQLite one-set Margin summaries preserve an ordered input LIMIT", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(id = 1:2, g = c("a", "b"), v = c(2L, 9L)), "facts"
  )
  limited <- head(dplyr::arrange(source, dplyr::desc(.data$v), .data$id), 1L)
  expected <- DBI::dbGetQuery(con, paste(
    "SELECT g, SUM(v) AS total FROM",
    "(SELECT * FROM facts ORDER BY v DESC, id LIMIT 1) x GROUP BY g"
  ))

  query <- summarize_with_margins(
    limited, total = sum(.data$v, na.rm = TRUE), .grouping = grouping_set("g")
  )
  expect_equal(as.data.frame(dplyr::collect(query)), expected)
})

test_that("SQLite limited and collapsed inputs survive Margin retrieval", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(id = 1:2, g = c("a", "b"), v = c(2L, 9L)), "facts"
  )
  limited <- head(dplyr::arrange(source, dplyr::desc(.data$v), .data$id), 1L)
  selected_sql <- "SELECT * FROM facts ORDER BY v DESC, id LIMIT 1"
  expected_summary <- DBI::dbGetQuery(con, paste(
    "SELECT g, SUM(v) AS total FROM (", selected_sql, ") x GROUP BY g",
    "UNION ALL SELECT 'Total', SUM(v) FROM (", selected_sql, ") x"
  ))
  expected_expansion <- DBI::dbGetQuery(con, paste(
    "SELECT g, id, v FROM (", selected_sql, ") x",
    "UNION ALL SELECT 'Total', id, v FROM (", selected_sql, ") x"
  ))

  for (input in list(limited, dplyr::collapse(limited))) {
    for (sort in c("none", "first", "last")) {
      for (id in list(NULL, "sid")) {
        queries <- list(
          summarize_with_margins(
            input, total = sum(.data$v, na.rm = TRUE),
            .grouping = rollup("g"), .sort = sort, .id = id
          ),
          expand_with_margins(
            input, .grouping = rollup("g"), .sort = sort, .id = id
          )
        )
        expected <- list(expected_summary, expected_expansion)
        for (i in seq_along(queries)) {
          reference <- expected[[i]]
          if (!is.null(id)) {
            reference$sid <- 1:2
            reference <- reference[c("g", "sid", setdiff(names(reference),
                                                         c("g", "sid")))]
          }
          if (identical(sort, "first")) {
            reference <- reference[2:1, , drop = FALSE]
          }
          rownames(reference) <- NULL
          query <- queries[[i]]
          expect_equal(as.data.frame(dplyr::collect(query)), reference)
          materialized <- dplyr::compute(
            query, temporary = TRUE, analyze = FALSE
          )
          expect_equal(as.data.frame(dplyr::collect(materialized)), reference)
          for (n in 0:3) {
            prefix <- reference[seq_len(min(n, 2L)), , drop = FALSE]
            if (n == 0L && i == 1L) {
              # Ordinary aggregates have no declared empty type (ADR 0031).
              prefix$total <- logical()
            }
            expect_equal(as.data.frame(dplyr::collect(query, n = n)), prefix)
          }
        }
      }
    }
  }
})

test_that("SQLite limited input anchors preserve missing source types", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  missing <- list(NA_character_, NA_integer_, NA_real_)

  for (i in seq_along(missing)) {
    source <- dplyr::copy_to(
      con, data.frame(id = 1:2, g = rep(missing[[i]], 2L), v = c(2L, 9L)),
      paste0("missing_limit_", i)
    )
    for (n in 0:1) {
      limited <- head(
        dplyr::arrange(source, dplyr::desc(.data$v), .data$id), n
      )
      for (sort in c("none", "last")) {
        summary <- summarize_with_margins(
          limited, total = sum(.data$v, na.rm = TRUE),
          .grouping = rollup("g"), .margin_label = NULL,
          .id = "sid", .sort = sort
        )
        expansion <- expand_with_margins(
          limited, .grouping = rollup("g"), .margin_label = NULL,
          .id = "sid", .sort = sort
        )
        queries <- list(summary = summary, expansion = expansion)
        for (kind in names(queries)) {
          query <- queries[[kind]]
          expected_rows <- if (kind == "summary" && n == 0L) 1L else 2L * n
          for (result in list(
            dplyr::collect(query),
            dplyr::collect(dplyr::compute(
              query, temporary = TRUE, analyze = FALSE
            ))
          )) {
            expect_identical(nrow(result), expected_rows)
            expect_identical(result$g, rep(missing[[i]], expected_rows))
          }
        }
      }
    }
  }
})

test_that("SQLite limited summaries retain usable input window ordering", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b", "c"), v = c(1L, 2L, 9L)), "facts"
  )
  limited <- head(dplyr::arrange(source, dplyr::desc(.data$g)), 2L)
  for (input in list(limited, dplyr::collapse(limited))) {
    query <- summarize_with_margins(
      input, total = sum(.data$v, na.rm = TRUE),
      .grouping = grouping_set("g"), .sort = "none"
    )
    ranked <- dplyr::mutate(query, rank = dplyr::row_number())
    result <- dplyr::arrange(dplyr::collect(ranked), .data$rank)
    expect_identical(result$g, c("c", "b"))
    expect_equal(result$total, c(9, 2))
    expect_equal(result$rank, 1:2)
  }
})

ordered_limit_data <- function() {
  data.frame(
    id = 1:8, p = c(rep("P", 5), rep("Q", 3)),
    r = c("a", "a", "a", "b", "b", "a", "a", "b"),
    s = c("u", "u", "v", "u", "v", "u", "v", "u"),
    v = c(8L, 2L, 5L, 20L, 1L, 7L, 3L, 11L)
  )
}

ordered_limit_reference <- function(con) {
  DBI::dbGetQuery(con, paste(
    "WITH pre AS (SELECT * FROM facts ORDER BY v DESC, id LIMIT 3),",
    "a AS (SELECT p, r, s, 1 AS sid, COUNT(*) AS rows, SUM(v) AS total",
    "FROM pre GROUP BY p, r, s UNION ALL",
    "SELECT p, r, 'Total', 2, COUNT(*), SUM(v) FROM pre GROUP BY p, r",
    "UNION ALL SELECT p, 'Total', 'Total', 3, COUNT(*), SUM(v)",
    "FROM pre GROUP BY p)",
    "SELECT c.*, CASE WHEN c.sid = 3 THEN 1.0",
    "ELSE c.total * 1.0 / d.total END AS parent,",
    "c.total * 1.0 / t.total AS whole FROM a c LEFT JOIN a d",
    "ON c.p = d.p AND ((c.sid = 1 AND d.sid = 2 AND c.r = d.r)",
    "OR (c.sid = 2 AND d.sid = 3)) LEFT JOIN a t",
    "ON c.p = t.p AND t.sid = 3 ORDER BY c.p, c.sid, c.r, c.s"
  ))
}

ordered_limit_report <- function(input, duplicates = "drop") {
  total <- rlang::sym("total")
  summarize_with_margins(
    input, rows = dplyr::n(), total = sum(.data$v, na.rm = TRUE),
    parent = share_of_parent(!!total), whole = share_of_total(!!total),
    .by = "p", .grouping = rollup("r", "s"), .id = "sid",
    .check_share_source = FALSE, .duplicates = duplicates
  )
}

expect_ordered_limit_report <- function(query, expected) {
  result <- dplyr::arrange(
    dplyr::collect(query), .data$p, .data$sid, .data$r, .data$s
  )
  expect_identical(names(result), names(expected))
  expect_equal(as.data.frame(result[c("p", "r", "s")]),
               expected[c("p", "r", "s")])
  for (name in c("sid", "rows", "total", "parent", "whole")) {
    expect_equal(as.numeric(result[[name]]), as.numeric(expected[[name]]))
  }
}

test_that("SQLite ordered input LIMIT preserves fixed-key share denominators", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(con, ordered_limit_data(), "facts")
  limited <- head(dplyr::arrange(source, dplyr::desc(.data$v), .data$id), 3L)
  expected <- ordered_limit_reference(con)
  expect_equal(expected$total, c(8, 20, 8, 20, 28, 11, 11, 11))
  expect_equal(expected$whole, c(2 / 7, 5 / 7, 2 / 7, 5 / 7, 1, 1, 1, 1))

  for (input in list(limited, dplyr::collapse(limited))) {
    query <- ordered_limit_report(input)
    expect_ordered_limit_report(query, expected)
    expect_ordered_limit_report(
      dplyr::compute(query, temporary = TRUE, analyze = FALSE), expected
    )
  }
})

test_that("DuckDB adapters preserve ordered input LIMIT share denominators", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(con, ordered_limit_data(), "facts")
  limited <- head(dplyr::arrange(source, dplyr::desc(.data$v), .data$id), 3L)
  expected <- ordered_limit_reference(con)
  # Retaining occurrence identifiers selects the portable adapter.
  for (duplicates in c("drop", "keep")) {
    expect_ordered_limit_report(
      ordered_limit_report(limited, duplicates), expected
    )
  }
})
