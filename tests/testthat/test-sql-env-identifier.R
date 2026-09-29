test_that("SQL dialects render .env Grouping set identifiers", {
  simulators <- available_simulators(c(
    "simulate_access", "simulate_dbi", "simulate_hana",
    "simulate_hive", "simulate_impala", "simulate_mariadb",
    "simulate_mssql", "simulate_mysql", "simulate_odbc",
    "simulate_oracle", "simulate_postgres", "simulate_redshift",
    "simulate_snowflake", "simulate_spark_sql", "simulate_sqlite",
    "simulate_teradata"
  ))

  for (simulator in simulators) {
    remote <- dbplyr::tbl_lazy(
      data.frame(g = c("east", "west"), value = c(2L, 3L)),
      con = getExportedValue("dbplyr", simulator)()
    )
    for (plan in c("one", "rollup")) {
      grouping <- if (identical(plan, "one")) grouping_set(g) else rollup(g)
      for (portable in c(FALSE, TRUE)) {
        summary <- summarize_with_margins(
          remote, rows = dplyr::n(), .grouping = grouping,
          .duplicates = if (portable) "keep" else "drop",
          .id = ".env", .sort = "last"
        )
        info <- paste(simulator, plan, portable)
        expect_identical(as.character(dplyr::tbl_vars(summary)),
                         c("g", ".env", "rows"), info = info)
        expect_true(nzchar(as.character(dbplyr::sql_render(summary))),
                    info = info)
      }
      expansion <- expand_with_margins(
        remote, .grouping = grouping, .id = ".env", .sort = "last"
      )
      info <- paste(simulator, plan, "expansion")
      expect_identical(as.character(dplyr::tbl_vars(expansion)),
                       c("g", ".env", "value"), info = info)
      expect_true(nzchar(as.character(dbplyr::sql_render(expansion))),
                  info = info)
    }
  }
})

check_sql_env_identifier <- function(con, source_name) {
  source <- dplyr::copy_to(
    con, data.frame(g = c("east", "west"), value = c(2L, 3L)),
    source_name,
    temporary = TRUE
  )
  case <- 0L

  for (plan in c("one", "rollup")) {
    grouping <- if (identical(plan, "one")) grouping_set(g) else rollup(g)
    for (kind in c("summary", "expansion")) {
      for (portable in if (identical(kind, "summary")) c(FALSE, TRUE) else
           FALSE) {
        case <- case + 1L
        query <- if (identical(kind, "summary")) {
          summarize_with_margins(
            source, rows = dplyr::n(), .grouping = grouping,
            .duplicates = if (portable) "keep" else "drop",
            .id = ".env", .sort = "last"
          )
        } else {
          expand_with_margins(
            source, .grouping = grouping, .id = ".env", .sort = "last"
          )
        }
        info <- paste(source_name, plan, kind, portable)
        purposes <- last_sent_queries()$purpose
        expect_identical(tail(purposes, 1L), "result", info = info)
        expect_true(all(purposes %in% c("selection_proxy", "result")),
                    info = info)
        expect_s3_class(query, "tbl_lazy")
        expected <- if (identical(kind, "summary")) {
          data.frame(g = c("east", "west"), .env = c(1L, 1L),
                     rows = c(1L, 1L), check.names = FALSE)
        } else {
          data.frame(g = c("east", "west"), .env = c(1L, 1L),
                     value = c(2L, 3L), check.names = FALSE)
        }
        if (identical(plan, "rollup")) {
          margin_rows <- if (identical(kind, "summary")) {
            data.frame(g = "Total", .env = 2L, rows = 2L,
                       check.names = FALSE)
          } else {
            data.frame(g = c("Total", "Total"), .env = c(2L, 2L),
                       value = c(2L, 3L), check.names = FALSE)
          }
          expected <- rbind(expected, margin_rows)
        }
        computed <- dplyr::compute(
          query, name = paste0(source_name, "_result_", case)
        )
        for (result in list(dplyr::collect(query), dplyr::collect(computed))) {
          actual <- as.data.frame(result)
          expect_identical(names(actual), names(expected), info = info)
          expect_identical(actual$g, expected$g, info = info)
          expect_identical(actual[[".env"]], expected[[".env"]], info = info)
          expect_identical(typeof(actual[[".env"]]), "integer", info = info)
          if (identical(kind, "summary")) {
            expect_equal(actual$rows, expected$rows, info = info)
          } else {
            expect_identical(actual$value, expected$value, info = info)
          }
        }
      }
    }
  }
}

test_that("DuckDB keeps .env identifiers through collection and compute", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  withr::local_options(marginplyr.audit_sql = TRUE)
  check_sql_env_identifier(con, "env_id_duckdb")
})

test_that("SQLite keeps .env identifiers through collection and compute", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  withr::local_options(marginplyr.audit_sql = TRUE)
  check_sql_env_identifier(con, "env_id_sqlite")
})
