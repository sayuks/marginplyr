test_that("DuckDB preserves a .env Grouping dimension in a one-set summary", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  input <- dplyr::copy_to(
    con, data.frame(.env = "east", check.names = FALSE),
    "env_grouping_source", temporary = TRUE
  )

  query <- summarize_with_margins(
    input, rows = dplyr::n(),
    .grouping = grouping_set(tidyselect::all_of(".env")),
    .margin_label = NULL
  )
  expected <- data.frame(.env = "east", rows = 1, check.names = FALSE)

  expect_identical(as.data.frame(dplyr::collect(query)), expected)
  computed <- dplyr::compute(query, name = "env_grouping_one_result")
  expect_identical(as.data.frame(dplyr::collect(computed)), expected)
})

test_that("DuckDB preserves a .env fixed key in a one-set summary", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  input <- dplyr::copy_to(
    con, data.frame(.env = c("east", NA_character_), g = c("a", "b"),
                    check.names = FALSE),
    "env_fixed_one_source", temporary = TRUE
  )

  query <- summarize_with_margins(
    input, rows = dplyr::n(), .by = tidyselect::all_of(".env"),
    .grouping = grouping_set(g), .margin_label = NULL, .sort = "last"
  )
  expected <- data.frame(
    .env = c("east", NA_character_), g = c("a", "b"),
    rows = c(1, 1), check.names = FALSE
  )

  expect_identical(as.data.frame(dplyr::collect(query)), expected)
  computed <- dplyr::compute(query, name = "env_fixed_one_result")
  expect_identical(as.data.frame(dplyr::collect(computed)), expected)
})

test_that("SQL dialects render .env fixed and Grouping keys", {
  simulators <- available_simulators(c(
    "simulate_access", "simulate_dbi", "simulate_hana",
    "simulate_hive", "simulate_impala", "simulate_mariadb",
    "simulate_mssql", "simulate_mysql", "simulate_odbc",
    "simulate_oracle", "simulate_postgres", "simulate_redshift",
    "simulate_snowflake", "simulate_spark_sql", "simulate_sqlite",
    "simulate_teradata"
  ))
  source <- data.frame(
    .env = c("east", NA_character_), g = c("a", "b"),
    check.names = FALSE
  )

  for (simulator in simulators) {
    remote <- dbplyr::tbl_lazy(
      source, con = getExportedValue("dbplyr", simulator)()
    )
    for (role in c("dimension", "fixed")) {
      for (plan in c("one", "rollup")) {
        grouping <- if (identical(role, "dimension")) {
          if (identical(plan, "one")) {
            grouping_set(tidyselect::all_of(".env"))
          } else {
            rollup(tidyselect::all_of(".env"))
          }
        } else if (identical(plan, "one")) {
          grouping_set(g)
        } else {
          rollup(g)
        }
        for (portable in c(FALSE, TRUE)) {
          query <- if (identical(role, "dimension")) {
            summarize_with_margins(
              remote, rows = dplyr::n(), .grouping = grouping,
              .duplicates = if (portable) "keep" else "drop",
              .margin_label = NULL, .id = "sid", .sort = "last"
            )
          } else {
            summarize_with_margins(
              remote, rows = dplyr::n(),
              .by = tidyselect::all_of(".env"), .grouping = grouping,
              .duplicates = if (portable) "keep" else "drop",
              .margin_label = NULL, .id = "sid", .sort = "last"
            )
          }
          info <- paste(simulator, role, plan, portable)
          expect_true(inherits(query, "tbl_lazy"), info = info)
          expect_true(nzchar(as.character(dbplyr::sql_render(query))),
                      info = info)
        }
      }
    }
  }
})

test_that("DuckDB .env keys retain values, labels, identifiers, and order", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- data.frame(
    .env = c("east", NA_character_), g = c("a", "b"),
    check.names = FALSE
  )
  input <- dplyr::copy_to(con, source, "env_keys_matrix", temporary = TRUE)
  case <- 0L

  for (role in c("dimension", "fixed")) {
    for (label in list(NULL, "Total")) {
      for (sort in c("first", "last")) {
        for (portable in c(FALSE, TRUE)) {
          case <- case + 1L
          run_summary <- function(data) {
            if (identical(role, "dimension")) {
              summarize_with_margins(
                data, rows = dplyr::n(),
                .grouping = rollup(tidyselect::all_of(".env")),
                .margin_label = label, .id = "sid", .sort = sort,
                .duplicates = if (portable) "keep" else "drop"
              )
            } else {
              summarize_with_margins(
                data, rows = dplyr::n(),
                .by = tidyselect::all_of(".env"), .grouping = rollup(g),
                .margin_label = label, .id = "sid", .sort = sort,
                .duplicates = if (portable) "keep" else "drop"
              )
            }
          }
          query <- run_summary(input)
          margin <- if (is.null(label)) NA_character_ else label
          if (identical(role, "dimension")) {
            detail <- data.frame(
              .env = c("east", NA_character_), sid = 1L,
              rows = 1, check.names = FALSE
            )
            total <- data.frame(
              .env = margin, sid = 2L, rows = 2,
              check.names = FALSE
            )
            expected <- if (identical(sort, "first")) {
              rbind(total, detail)
            } else {
              rbind(detail, total)
            }
          } else {
            detail <- data.frame(
              .env = c("east", NA_character_), g = c("a", "b"),
              sid = 1L, rows = 1, check.names = FALSE
            )
            total <- data.frame(
              .env = c("east", NA_character_), g = margin,
              sid = 2L, rows = 1, check.names = FALSE
            )
            expected <- if (identical(sort, "first")) {
              vctrs::vec_rbind(total[1L, ], detail[1L, ],
                               total[2L, ], detail[2L, ])
            } else {
              vctrs::vec_rbind(detail[1L, ], total[1L, ],
                               detail[2L, ], total[2L, ])
            }
          }
          rownames(expected) <- NULL
          info <- paste(role, label, sort, portable)
          actual <- as.data.frame(dplyr::collect(query))
          expect_identical(actual, as.data.frame(expected), info = info)
          expect_equal(actual, as.data.frame(run_summary(source)), info = info)
          if (identical(sort, "last")) {
            computed <- dplyr::compute(
              query, name = paste0("env_keys_result_", case)
            )
            expect_identical(as.data.frame(dplyr::collect(computed)),
                             as.data.frame(expected), info = info)
          }
        }
      }
    }
  }
})

test_that("DuckDB .env keys leave the caller's lexical pronoun intact", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  input <- dplyr::copy_to(
    con, data.frame(.env = "east", check.names = FALSE),
    "env_key_lexical_source", temporary = TRUE
  )
  offset <- 4L
  query <- summarize_with_margins(
    input, rows = dplyr::n() + .env$offset,
    .grouping = grouping_set(tidyselect::all_of(".env"))
  )

  expect_identical(dplyr::collect(query)$rows, 5)
})
