check_sql_dot_dot_summaries <- function(con, portable = FALSE) {
  data <- data.frame(g = c("b", "a"), v = c(2, 3))
  source <- dplyr::copy_to(con, data, "dot_dot_source", overwrite = TRUE)
  for (name in c("..+0", "..+1", ".. 1", "..0", "..00", "..01", "..-1")) {
    dots <- stats::setNames(rlang::quos(sum(.data$v, na.rm = TRUE)), name)
    ordinary <- dplyr::summarize(source, !!!dots)
    expect_s3_class(dbplyr::sql_render(ordinary), "sql")
    expect_identical(names(dplyr::collect(ordinary)), name)
    expect_equal(dplyr::collect(ordinary)[[name]], 5)
    expect_equal(dplyr::collect(dplyr::compute(ordinary))[[name]], 5)
    identifier <- dplyr::mutate(source, !!name := 1L)
    expect_identical(dplyr::collect(identifier)[[name]], c(1L, 1L))
    for (role in c("output", "identifier")) {
      output <- if (role == "output") name else "amount"
      id <- if (role == "identifier") name else if (portable) "sid" else NULL
      summaries <- stats::setNames(
        rlang::quos(sum(.data$v, na.rm = TRUE)), output
      )
      for (plan in list(NULL, rollup(tidyselect::all_of("g")))) {
        for (sort in c("none", "first", "last")) {
          query <- summarize_with_margins(
            source, !!!summaries, .grouping = plan, .id = id, .sort = sort,
            .duplicates = if (portable) "keep" else "drop"
          )
          expect_s3_class(query, "tbl_lazy")
          sql <- dbplyr::sql_render(query)
          expect_s3_class(sql, "sql")
          if (!is.null(plan) && inherits(con, "duckdb_connection")) {
            expect_match(sql, if (portable) "UNION ALL" else "GROUPING SETS")
          }
          results <- list(dplyr::collect(query),
                          dplyr::collect(dplyr::compute(query)))
          for (result in results) {
            expect_identical(names(result), c(if (!is.null(plan)) "g", id,
                                              output))
            expect_equal(sort(result[[output]]), if (is.null(plan)) {
              5
            } else {
              c(2, 3, 5)
            })
            if (!is.null(id)) {
              expect_type(result[[id]], "integer")
              expect_identical(sort(result[[id]]), if (is.null(plan)) {
                1L
              } else {
                c(1L, 1L, 2L)
              })
            }
            if (!is.null(plan)) {
              rows <- match(c("a", "b", "Total"), result$g)
              expect_equal(result[[output]][rows], c(3, 2, 5))
            }
            if (!is.null(plan) && sort != "none") {
              expect_identical(result$g, if (sort == "first") {
                c("Total", "a", "b")
              } else {
                c("a", "b", "Total")
              })
              expect_equal(result[[output]], if (sort == "first") {
                c(5, 3, 2)
              } else {
                c(3, 2, 5)
              })
            }
          }
        }
      }
    }
  }
}

check_sql_dot_dot_empty <- function(con, portable = FALSE) {
  source <- dplyr::copy_to(
    con, data.frame(g = character(), v = double()),
    "dot_dot_empty", overwrite = TRUE
  )
  included_plan <- grouping_set(tidyselect::all_of("g"))
  plans <- list(NULL, included_plan, rollup(tidyselect::all_of("g")))
  for (name in c("..+0", "..+1", ".. 1", "..0", "..00", "..01", "..-1")) {
    for (role in c("output", "identifier")) {
      output <- if (role == "output") name else "amount"
      id <- if (role == "identifier") name else if (portable) "sid" else NULL
      for (kind in c("count", "constant")) {
        dots <- stats::setNames(if (kind == "count") {
          rlang::quos(dplyr::n())
        } else {
          rlang::quos(7L)
        }, output)
        for (plan in plans) {
          included <- identical(plan, included_plan)
          for (sort in c("none", "first", "last")) {
            query <- summarize_with_margins(
              source, !!!dots, .grouping = plan, .id = id, .sort = sort,
              .duplicates = if (portable) "keep" else "drop"
            )
            expect_s3_class(dbplyr::sql_render(query), "sql")
            for (result in list(dplyr::collect(query),
                                dplyr::collect(dplyr::compute(query)))) {
              expect_identical(names(result), c(if (!is.null(plan)) "g", id,
                                                output))
              expect_equal(nrow(result), if (included) 0L else 1L)
              if (included) {
                expect_length(result[[output]], 0L)
              } else {
                expect_equal(result[[output]], if (kind == "count") 0 else 7)
              }
              if (!is.null(id)) {
                expect_identical(result[[id]], if (included) {
                  integer()
                } else {
                  if (is.null(plan)) 1L else 2L
                })
              }
              if (!is.null(plan)) {
                expect_identical(result$g, if (included) {
                  character()
                } else {
                  "Total"
                })
              }
            }
          }
        }
      }
    }
  }
}

test_that("SQLite retains accepted dot-dot summary and identifier names", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  check_sql_dot_dot_summaries(con)
  check_sql_dot_dot_empty(con)
})

test_that("DuckDB retains accepted dot-dot names on both summary adapters", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  check_sql_dot_dot_summaries(con)
  check_sql_dot_dot_summaries(con, portable = TRUE)
  check_sql_dot_dot_empty(con)
  check_sql_dot_dot_empty(con, portable = TRUE)
})

check_sql_dot_dot_shares <- function(con, portable = FALSE) {
  source <- dplyr::copy_to(
    con, data.frame(g = c("b", "a"), v = c(2, 3)),
    "dot_dot_share_source", overwrite = TRUE
  )
  offset <- 1
  for (name in c("..+0", "..+1", ".. 1", "..0", "..-1")) {
    for (role in c("source", "total", "parent")) {
      amount <- if (role == "source") name else "amount"
      fraction <- if (role == "total") name else "fraction"
      parent <- if (role == "parent") name else "parent"
      # dbplyr supplies .env when translating this captured summary.
      dots <- stats::setNames(
        rlang::quos(
          sum(.data$v, na.rm = TRUE) +
            .env$offset # nolint: object_usage_linter.
        ), amount
      )
      dots[[fraction]] <- rlang::new_quosure(
        rlang::call2("share_of_total", rlang::sym(amount))
      )
      dots[[parent]] <- rlang::new_quosure(
        rlang::call2("share_of_parent", rlang::sym(amount))
      )
      for (empty in c(FALSE, TRUE)) {
        input <- if (empty) dplyr::filter(source, .data$v < 0) else source
        for (shape in c("scalar", "rollup", "fixed")) {
          if (role == "parent" && shape != "rollup") next
          plan <- if (shape == "rollup") {
            rollup(tidyselect::all_of("g"))
          } else {
            NULL
          }
          by <- if (shape == "fixed") "g" else character()
          summaries <- if (shape == "rollup") dots else dots[1:2]
          id <- if (portable) "sid" else NULL
          for (sort in c("none", "first", "last")) {
            query <- summarize_with_margins(
              input, !!!summaries, .by = tidyselect::all_of(by),
              .grouping = plan, .id = id, .sort = sort,
              .duplicates = if (portable) "keep" else "drop",
              .check_share_source = FALSE
            )
            expect_s3_class(query, "tbl_lazy")
            expect_s3_class(dbplyr::sql_render(query), "sql")
            for (result in list(dplyr::collect(query),
                                dplyr::collect(dplyr::compute(query)))) {
              info <- paste(name, role, empty, shape, sort)
              expect_identical(names(result), c(
                if (shape != "scalar") "g", id, amount, fraction,
                if (shape == "rollup") parent
              ), info = info)
              rows <- if (shape == "scalar") {
                1L
              } else if (empty) {
                if (shape == "fixed") integer() else match("Total", result$g)
              } else {
                match(c("a", "b", if (shape == "rollup") "Total"), result$g)
              }
              expected_amount <- if (shape == "scalar") {
                6
              } else {
                c(4, 3, if (shape == "rollup") 6)
              }
              expected_share <- if (empty) {
                if (shape == "fixed") double() else 1
              } else if (shape == "rollup") {
                c(4 / 6, 3 / 6, 1)
              } else {
                rep(1, length(rows))
              }
              expect_equal(nrow(result), length(rows), info = info)
              if (empty) {
                expect_true(all(is.na(result[[amount]])), info = info)
              } else {
                expect_equal(result[[amount]][rows], expected_amount,
                             info = info)
              }
              for (share in c(fraction, if (shape == "rollup") parent)) {
                expect_type(result[[share]], "double")
                expect_equal(result[[share]][rows], expected_share, info = info)
              }
              if (!is.null(id)) {
                expect_identical(result[[id]][rows], if (shape != "rollup") {
                  rep(1L, length(rows))
                } else if (empty) {
                  2L
                } else {
                  c(1L, 1L, 2L)
                })
              }
              if (shape == "rollup" && !empty && sort != "none") {
                expect_identical(result$g, if (sort == "first") {
                  c("Total", "a", "b")
                } else {
                  c("a", "b", "Total")
                })
              }
            }
          }
        }
      }
    }
  }
}

test_that("SQLite preserves signed dot-dot share sources and outputs", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  check_sql_dot_dot_shares(con)
})

test_that("DuckDB preserves signed dot-dot shares on both adapters", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  check_sql_dot_dot_shares(con)
  check_sql_dot_dot_shares(con, portable = TRUE)
})
