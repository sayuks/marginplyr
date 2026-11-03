#!/usr/bin/env Rscript

# Public acceptance for #1175. Select a disposable PostgreSQL server through
# standard PG environment variables and install the working tree first.
library(marginplyr)

# Check values and public names while the driver boundary proves construction
# issues no queries. Collection is the caller's explicit execution request.
verify_share_identifiers <- function(con) {
  input <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), x = c(1, 3)),
    "share_identifiers", temporary = TRUE
  )
  cases <- 0L
  stems <- c(
    short = "summary", ascii = strrep("s", 35L),
    utf8 = strrep("計", 12L), boundary = strrep("s", 62L),
    utf8_boundary = strrep("計", 20L)
  )
  for (stem in stems) {
    sources <- paste0(stem, c("a", "b"))
    ordinary <- stats::setNames(
      rlang::exprs(sum(.data$x, na.rm = TRUE), 2 * sum(.data$x, na.rm = TRUE)),
      sources
    )
    control <- dplyr::collect(dplyr::summarise(input, !!!ordinary))
    testthat::expect_identical(names(control), sources)
    testthat::expect_equal(unname(unlist(control)), c(4, 8))
    testthat::expect_true(all(vapply(control, is.double, logical(1))))

    # These valid public columns occupy the first twelve allocation candidates,
    # including the point at which a decimal suffix needs another byte.
    for (collision in c(FALSE, TRUE)) {
      reserved <- if (collision) {
        vapply(seq_len(12L), function(i) {
          paste0("..marginplyr_denominator_of_",
                 strrep("s", 35L - nchar(i)), i)
        }, character(1))
      } else {
        character()
      }
      extra <- stats::setNames(
        rep(rlang::exprs(7 * sum(.data$x, na.rm = TRUE)), length(reserved)),
        reserved
      )
      for (count in seq_len(2L)) {
        for (kind in c("parent", "total", "both")) {
          shares <- list()
          for (i in seq_len(count)) {
            source <- rlang::sym(sources[[i]])
            if (kind %in% c("parent", "both")) {
              shares[[paste0("parent", i)]] <- rlang::expr(
                share_of_parent(!!source)
              )
            }
            if (kind %in% c("total", "both")) {
              shares[[paste0("total", i)]] <- rlang::expr(
                share_of_total(!!source)
              )
            }
          }
          for (duplicates in c("error", "keep")) {
            query <- testthat::with_mocked_bindings(
              summarize_with_margins(
                input, !!!ordinary, !!!extra, !!!shares,
                .grouping = rollup(dplyr::all_of("g")),
                .id = "set", .margin_label = NULL,
                .duplicates = duplicates, .check_share_source = FALSE
              ),
              dbSendQuery = function(...) stop("Unexpected query"),
              dbExecute = function(...) stop("Unexpected command"),
              .package = "DBI"
            )
            sql <- as.character(dbplyr::sql_render(query))
            testthat::expect_identical(
              grepl("UNION ALL", sql, fixed = TRUE), duplicates == "keep"
            )
            value <- dplyr::collect(query)
            value <- value[order(value$set, value$g), ]
            testthat::expect_identical(
              names(value), c("g", "set", sources, reserved, names(shares))
            )
            testthat::expect_equal(value[[sources[[1L]]]], c(1, 3, 4))
            testthat::expect_equal(value[[sources[[2L]]]], c(2, 6, 8))
            testthat::expect_true(all(vapply(
              value[sources], is.double, logical(1)
            )))
            for (name in names(shares)) {
              testthat::expect_identical(value[[name]], c(0.25, 0.75, 1))
            }
            for (name in reserved) {
              testthat::expect_equal(value[[name]], c(7, 21, 28))
            }
            cases <- cases + 1L
          }
        }
      }
    }
  }
  cat("PostgreSQL share identifier cases passed:", cases, "\n")
}

con <- DBI::dbConnect(RPostgres::Postgres())
tryCatch(verify_share_identifiers(con), finally = DBI::dbDisconnect(con))
