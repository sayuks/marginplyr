#!/usr/bin/env Rscript

# Public acceptance for #786 against a disposable PostgreSQL server selected
# through standard PG environment variables. Install the working tree first.
library(marginplyr)

# Compare unordered results by the carried value or scalar summary, then set.
canonical_rows <- function(data, value) {
  data <- as.data.frame(data)
  keys <- intersect(c("set", value, "g", "h"), names(data))
  data <- data[do.call(order, data[keys]), , drop = FALSE]
  row.names(data) <- NULL
  data
}

# No database command is allowed while constructing these opted-out shares or
# expansions. Collection happens after the driver boundary is restored.
build_without_queries <- function(expr) {
  testthat::with_mocked_bindings(
    force(expr),
    dbSendQuery = function(...) stop("Unexpected construction-time query"),
    dbExecute = function(...) stop("Unexpected construction-time command"),
    .package = "DBI"
  )
}

verify_typed_missing <- function(con) {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  dimensions <- list(
    logical = c(FALSE, TRUE), integer = c(1L, 2L), double = c(1.25, 2.5),
    Date = as.Date(c("2026-01-01", "2026-02-01")),
    POSIXct = as.POSIXct(c("2026-01-01", "2026-02-01"), tz = "UTC"),
    character = c("x", "y")
  )
  # `amount` names the preceding summary in the contextual helper.
  share_requests <- list(
    none = list(),
    parent = rlang::exprs(share = share_of_parent(amount)),
    total = rlang::exprs(share = share_of_total(amount))
  )
  for (type in names(dimensions)) {
    data <- data.frame(
      g = dimensions[[type]][c(1L, 1L, 2L, NA_integer_)],
      h = c("a", "b", "a", "b"), value = c(1, 3, 2, 6)
    )
    source <- dplyr::copy_to(
      con, data, "typed_missing", temporary = TRUE, overwrite = TRUE
    )
    for (label in list(NULL, NA_character_)) {
      for (id in list(NULL, "set")) {
        for (sort in c("none", "last")) {
          query <- build_without_queries(expand_with_margins(
            source, .grouping = rollup(dplyr::all_of(c("g", "h"))),
            .margin_label = label, .id = id, .sort = sort
          ))
          testthat::expect_identical(last_sent_queries()$purpose, "result")
          expected <- expand_with_margins(
            data, .grouping = rollup(dplyr::all_of(c("g", "h"))),
            .margin_label = label, .id = id, .sort = sort
          )
          actual <- dplyr::collect(query)
          testthat::expect_identical(class(actual$g), class(data$g))
          if (sort == "last") {
            keys <- intersect(c("g", "h", "set"), names(actual))
            testthat::expect_equal(as.data.frame(actual[keys]),
                                   as.data.frame(expected[keys]), info = type)
          }
          testthat::expect_equal(canonical_rows(actual, "value"),
                                 canonical_rows(expected, "value"), info = type)
        }
      }
      for (duplicates in c("keep", "drop")) {
        for (kind in names(share_requests)) {
          shares <- share_requests[[kind]]
          summarize <- function(input) {
            summarize_with_margins(
              input, amount = sum(.data$value, na.rm = TRUE), !!!shares,
              .grouping = rollup(dplyr::all_of("g"), dplyr::all_of("g"),
                                 dplyr::all_of("h")),
              .duplicates = duplicates, .margin_label = label,
              .id = "set", .sort = "last", .check_share_source = FALSE
            )
          }
          query <- build_without_queries(summarize(source))
          testthat::expect_identical(last_sent_queries()$purpose, "result")
          sql <- as.character(dbplyr::sql_render(query))
          testthat::expect_identical(grepl("UNION ALL", sql, fixed = TRUE),
                                     duplicates == "keep")
          actual <- dplyr::collect(query)
          testthat::expect_identical(class(actual$g), class(data$g))
          testthat::expect_equal(as.data.frame(actual),
                                 as.data.frame(summarize(data)), info = type)
        }
      }
    }
    # Default character display must leave Parent keys in their original type.
    for (kind in names(share_requests)) {
      shares <- share_requests[[kind]]
      query <- build_without_queries(summarize_with_margins(
        source, amount = sum(.data$value, na.rm = TRUE), !!!shares,
        .grouping = rollup(dplyr::all_of("g"), dplyr::all_of("g"),
                           dplyr::all_of("h")),
        .duplicates = "keep", .id = "set", .check_share_source = FALSE
      ))
      testthat::expect_identical(last_sent_queries()$purpose, "result")
      actual <- canonical_rows(dplyr::collect(query), "amount")
      testthat::expect_type(actual$g, "character")
      testthat::expect_equal(actual$amount,
                             c(1, 2, 3, 6, 2, 4, 6, 2, 4, 6, 12))
      if (kind != "none") {
        expected <- if (kind == "parent") {
          c(1 / 4, 1, 3 / 4, 1, 1 / 6, 1 / 3, 1 / 2,
            1 / 6, 1 / 3, 1 / 2, 1)
        } else {
          c(1 / 12, 1 / 6, 1 / 4, 1 / 2, 1 / 6, 1 / 3, 1 / 2,
            1 / 6, 1 / 3, 1 / 2, 1)
        }
        testthat::expect_equal(actual$share, expected, info = type)
        testthat::expect_type(actual$share, "double")
      }
    }
  }

  # Derived dimensions have the query's type, not the physical table's type.
  source <- dplyr::copy_to(
    con, data.frame(g = c(1L, 2L)), "derived_missing", temporary = TRUE
  )
  source <- dplyr::mutate(source, g = .data$g + 0.5)
  query <- build_without_queries(summarize_with_margins(
    source, n = dplyr::n(), .margin_label = NULL, .id = "set",
    .grouping = grouping_sets(grouping_set(), grouping_set(dplyr::all_of("g"))),
    .duplicates = "keep"
  ))
  actual <- canonical_rows(dplyr::collect(query), "n")
  testthat::expect_equal(actual, data.frame(
    g = c(NA_real_, 1.5, 2.5), set = c(1L, 2L, 2L), n = c(2L, 1L, 1L)
  ))

  renamed <- dplyr::rename(source, .env = dplyr::all_of("g"))
  query <- build_without_queries(summarize_with_margins(
    renamed, n = dplyr::n(),
    .grouping = rollup(dplyr::all_of(".env")),
    .margin_label = NULL, .duplicates = "keep", .id = "set", .sort = "last"
  ))
  testthat::expect_equal(as.data.frame(dplyr::collect(query)), data.frame(
    .env = c(1.5, 2.5, NA_real_), set = c(1L, 1L, 2L), n = c(1L, 1L, 2L)
  ))

  empty <- dplyr::filter(source, FALSE)
  query <- build_without_queries(summarize_with_margins(
    empty, n = dplyr::n(), .grouping = rollup(dplyr::all_of("g")),
    .margin_label = NULL, .duplicates = "keep", .id = "set"
  ))
  testthat::expect_equal(as.data.frame(dplyr::collect(query)),
                         data.frame(g = NA_real_, set = 2L, n = 0L))
  query <- build_without_queries(expand_with_margins(
    empty, .grouping = rollup(dplyr::all_of("g")),
    .margin_label = NULL, .id = "set", .sort = "last"
  ))
  testthat::expect_equal(as.data.frame(dplyr::collect(query)),
                         data.frame(g = double(), set = integer()))
}

con <- DBI::dbConnect(RPostgres::Postgres())
tryCatch(verify_typed_missing(con), finally = DBI::dbDisconnect(con))
cat("PostgreSQL typed-missing labels and Parent share keys passed.\n")
