test_that("SQL dialects render .env Parent and Total share outputs", {
  source <- data.frame(
    g = c("east", "east", "west", "west"),
    h = c("one", "two", "one", "two"),
    value = c(2, 3, 5, 7)
  )
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
    for (kind in c("parent", "total")) {
      query <- if (identical(kind, "parent")) {
        summarize_with_margins(
          remote, amount = sum(value, na.rm = TRUE),
          !!output := share_of_parent(amount),
          .grouping = rollup(g), .check_share_source = FALSE
        )
      } else {
        summarize_with_margins(
          remote, amount = sum(value, na.rm = TRUE),
          !!output := share_of_total(amount),
          .grouping = rollup(g), .check_share_source = FALSE
        )
      }
      info <- paste(simulator, kind)
      expect_identical(as.character(dplyr::tbl_vars(query)),
                       c("g", "amount", ".env"), info = info)
      expect_true(nzchar(as.character(dbplyr::sql_render(query))),
                  info = info)
    }
  }
})

expect_sql_env_share_output <- function(backend) {
  old_options <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old_options), add = TRUE)
  if (identical(backend, "SQLite")) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  } else {
    con <- duckdb_test_connection()
  }
  on.exit({
    if (identical(backend, "DuckDB")) {
      DBI::dbDisconnect(con, shutdown = TRUE)
    } else {
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)
  source <- data.frame(
    g = c("east", "east", "west", "west"),
    h = c("one", "two", "one", "two"),
    value = c(2, 3, 5, 7)
  )
  remote <- dplyr::copy_to(
    con, source, paste0("env_share_output_", backend), temporary = TRUE
  )
  output <- ".env"
  offset <- 4

  for (kind in c("parent", "total")) {
    query <- if (identical(kind, "parent")) {
      summarize_with_margins(
        remote, amount = sum(value + .env$offset, na.rm = TRUE),
        !!output := share_of_parent(amount),
        .grouping = rollup(g, h), .sort = "last",
        .check_share_source = !identical(backend, "SQLite")
      )
    } else {
      summarize_with_margins(
        remote, amount = sum(value + .env$offset, na.rm = TRUE),
        !!output := share_of_total(amount),
        .grouping = rollup(g, h), .sort = "last",
        .check_share_source = !identical(backend, "SQLite")
      )
    }
    sent <- last_sent_queries()
    expect_identical(tail(sent$purpose, 1L), "result")
    if (identical(backend, "SQLite")) {
      expect_identical(sent$purpose, "result")
    } else {
      expect_identical(setdiff(
        sent$purpose, c("selection_proxy", "share_dialect",
                        "share_dialect_control", "result")
      ), character(), info = paste(sent$purpose, collapse = ", "))
    }

    destination <- paste0("env_share_output_", backend, "_", kind)
    computed <- dplyr::compute(query, name = destination)
    expected <- data.frame(
      g = c("east", "east", "east", "west", "west", "west", "Total"),
      h = c("one", "two", "Total", "one", "two", "Total", "Total"),
      amount = c(6, 7, 13, 9, 11, 20, 33),
      .env = if (identical(kind, "parent")) {
        c(6 / 13, 7 / 13, 13 / 33, 9 / 20, 11 / 20, 20 / 33, 1)
      } else {
        c(6 / 33, 7 / 33, 13 / 33, 9 / 33, 11 / 33, 20 / 33, 1)
      },
      check.names = FALSE
    )
    for (result in list(dplyr::collect(query), dplyr::collect(computed))) {
      actual <- as.data.frame(result)
      expect_identical(names(actual), names(expected))
      expect_identical(actual$g, expected$g)
      expect_identical(actual$h, expected$h)
      expect_equal(actual$amount, expected$amount)
      expect_equal(actual[[".env"]], expected[[".env"]])
      expect_identical(typeof(actual[[".env"]]), "double")
    }
    expect_identical(DBI::dbListFields(con, destination), names(expected))
  }
}

test_that("SQLite preserves .env share outputs and lexical pronouns", {
  skip_if_suggest_absent("RSQLite", "DBI")
  expect_sql_env_share_output("SQLite")
})

test_that("DuckDB preserves .env share outputs and lexical pronouns", {
  skip_if_suggest_absent("duckdb", "DBI")
  expect_sql_env_share_output("DuckDB")
})
