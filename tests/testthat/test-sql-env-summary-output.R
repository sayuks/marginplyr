test_that("SQL dialects render ordinary .env summary outputs", {
  source <- data.frame(g = c("east", "west"), value = c(2L, 3L))
  simulators <- available_simulators(c(
    "simulate_access", "simulate_dbi", "simulate_hana",
    "simulate_hive", "simulate_impala", "simulate_mariadb",
    "simulate_mssql", "simulate_mysql", "simulate_odbc",
    "simulate_oracle", "simulate_postgres", "simulate_redshift",
    "simulate_snowflake", "simulate_spark_sql", "simulate_sqlite",
    "simulate_teradata"
  ))
  output <- ".env"

  for (simulator in simulators) {
    remote <- dbplyr::tbl_lazy(
      source, con = getExportedValue("dbplyr", simulator)()
    )
    for (sort in c("none", "first", "last")) {
      for (plan in c("one", "rollup")) {
        query <- if (identical(plan, "one")) {
          summarize_with_margins(
            remote, !!output := dplyr::n(),
            .grouping = grouping_set(g), .sort = sort
          )
        } else {
          summarize_with_margins(
            remote, !!output := dplyr::n(),
            .grouping = rollup(g), .sort = sort
          )
        }
        info <- paste(simulator, sort, plan)
        expect_identical(as.character(dplyr::tbl_vars(query)),
                         c("g", ".env"), info = info)
        expect_true(nzchar(as.character(dbplyr::sql_render(query))),
                    info = info)
      }
    }
  }
})

test_that("DuckDB preserves .env summary values and type across plans", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("east", "west"), value = c(2L, 3L)),
    "env_summary_matrix_source", temporary = TRUE
  )
  output <- ".env"

  for (plan in c("one", "rollup")) {
    for (sort in c("none", "first", "last")) {
      query <- if (identical(plan, "one")) {
        summarize_with_margins(
          source, !!output := dplyr::n(),
          .grouping = grouping_set(g), .sort = sort
        )
      } else {
        summarize_with_margins(
          source, !!output := dplyr::n(),
          .grouping = rollup(g), .sort = sort
        )
      }
      expected <- data.frame(
        g = c("east", "west"), .env = c(1, 1), check.names = FALSE
      )
      if (identical(plan, "rollup")) {
        expected <- rbind(
          expected,
          data.frame(g = "Total", .env = 2, check.names = FALSE)
        )
      }
      if (identical(sort, "first") && identical(plan, "rollup")) {
        expected <- expected[c(3L, 1L, 2L), , drop = FALSE]
        rownames(expected) <- NULL
      }
      info <- paste(plan, sort)
      materialized <- dplyr::compute(query, name = paste0(
        "env_summary_matrix_", plan, "_", sort
      ))
      results <- list(dplyr::collect(query), dplyr::collect(materialized))
      for (result in results) {
        actual <- as.data.frame(result)
        if (identical(sort, "none")) {
          actual <- actual[vctrs::vec_order(actual), , drop = FALSE]
          expected <- expected[vctrs::vec_order(expected), , drop = FALSE]
          rownames(actual) <- NULL
          rownames(expected) <- NULL
        }
        expect_identical(actual, expected, info = info)
        expect_identical(typeof(actual[[".env"]]), "double", info = info)
      }
    }
  }
})

test_that("SQL .env summary output keeps the caller's lexical pronoun", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = "east", value = 2L),
    "env_summary_lexical_source", temporary = TRUE
  )
  offset <- 4L
  output <- ".env"
  query <- summarize_with_margins(
    source, !!output := dplyr::n(),
    adjusted = sum(value) + .env$offset,
    .grouping = rollup(g), .sort = "last"
  )

  actual <- as.data.frame(dplyr::collect(query))
  expect_identical(actual[[".env"]], c(1, 1))
  expect_identical(actual$adjusted, c(6, 6))
})
