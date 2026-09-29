test_that("SQL dialects render an ordinary .env expansion payload", {
  source <- data.frame(
    g = c("east", "west"), .env = c("red", "blue"),
    value = c(2L, 3L), check.names = FALSE
  )
  simulators <- available_simulators(c(
    "simulate_access", "simulate_dbi", "simulate_hana",
    "simulate_hive", "simulate_impala", "simulate_mariadb",
    "simulate_mssql", "simulate_mysql", "simulate_odbc",
    "simulate_oracle", "simulate_postgres", "simulate_redshift",
    "simulate_snowflake", "simulate_spark_sql", "simulate_sqlite",
    "simulate_teradata"
  ))

  for (simulator in simulators) {
    con <- getExportedValue("dbplyr", simulator)()
    remote <- dbplyr::tbl_lazy(source, con = con)
    query <- expand_with_margins(
      remote, .grouping = rollup(g), .id = "sid", .sort = "last"
    )

    expect_true(inherits(query, "tbl_lazy"), info = simulator)
    expect_identical(as.character(dplyr::tbl_vars(query)),
                     c("g", "sid", ".env", "value"), info = simulator)
    expect_match(dbplyr::sql_render(query), "UNION ALL", fixed = TRUE,
                 info = simulator)

    one_set <- expand_with_margins(
      remote, .grouping = grouping_set(g)
    )
    expect_true(nzchar(as.character(dbplyr::sql_render(one_set))),
                info = simulator)

    control <- dbplyr::tbl_lazy(
      data.frame(g = source$g, payload = source[[".env"]]), con = con
    )
    ordinary <- expand_with_margins(
      control, .grouping = rollup(g), .id = "sid", .sort = "last"
    )
    expect_match(dbplyr::sql_render(ordinary), "UNION ALL", fixed = TRUE,
                 info = simulator)
  }
})

test_that("DuckDB materializes an expanded .env payload", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  input <- dplyr::copy_to(
    con,
    data.frame(g = c("east", "west"), .env = c("red", "blue"),
               check.names = FALSE),
    "env_expand_source", temporary = TRUE
  )

  query <- expand_with_margins(
    input, .grouping = rollup(g), .id = "sid", .sort = "last"
  )
  expected <- data.frame(
    g = c("east", "west", "Total", "Total"),
    sid = c(1L, 1L, 2L, 2L),
    .env = c("red", "blue", "red", "blue"), check.names = FALSE
  )

  expect_identical(as.data.frame(dplyr::collect(query)), expected)
  computed <- dplyr::compute(query, name = "env_expand_result")
  expect_identical(as.data.frame(dplyr::collect(computed)), expected)

  one_set <- expand_with_margins(input, .grouping = grouping_set(g))
  one_set_expected <- data.frame(
    g = c("east", "west"), .env = c("red", "blue"),
    check.names = FALSE
  )
  expect_identical(as.data.frame(dplyr::collect(one_set)), one_set_expected)
  expect_identical(
    as.data.frame(dplyr::collect(dplyr::compute(
      one_set, name = "env_expand_one_set"
    ))),
    one_set_expected
  )
  expect_identical(
    as.data.frame(dplyr::collect(dplyr::compute(one_set))),
    one_set_expected
  )
  expect_error(
    dplyr::compute(one_set, temporary = FALSE),
    "`name` must be provided when `temporary = FALSE`",
    class = "marginplyr_error"
  )
  expect_identical(
    as.data.frame(dplyr::collect(dplyr::compute(
      one_set, name = "env_expand_persistent", temporary = FALSE,
      analyze = FALSE
    ))),
    one_set_expected
  )
})

test_that("SQLite materializes expanded .env payloads", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  input <- dplyr::copy_to(
    con,
    data.frame(g = c("east", "west"), .env = c("red", "blue"),
               check.names = FALSE),
    "env_expand_materialize_source", temporary = TRUE
  )
  query <- expand_with_margins(
    input, .grouping = rollup(g), .id = "sid", .sort = "last"
  )
  expected <- data.frame(
    g = c("east", "west", "Total", "Total"),
    sid = c(1L, 1L, 2L, 2L),
    .env = c("red", "blue", "red", "blue"), check.names = FALSE
  )

  expect_identical(as.data.frame(dplyr::collect(query)), expected)
  computed <- dplyr::compute(query, name = "env_expand_materialized")
  expect_identical(as.data.frame(dplyr::collect(computed)), expected)

  one_set <- expand_with_margins(input, .grouping = grouping_set(g))
  one_set_expected <- data.frame(
    g = c("east", "west"), .env = c("red", "blue"),
    check.names = FALSE
  )
  expect_identical(as.data.frame(dplyr::collect(one_set)), one_set_expected)
  expect_identical(
    as.data.frame(dplyr::collect(dplyr::compute(
      one_set, name = "env_expand_materialized_one_set"
    ))),
    one_set_expected
  )
})

check_sql_env_expansion_cases <- function(con, source_name) {
  input <- dplyr::copy_to(
    con,
    data.frame(g = c("east", "west"), .env = c("red", "blue"),
               value = c(2L, 3L), check.names = FALSE),
    source_name, temporary = TRUE
  )

  for (label in list(NULL, "Total")) {
    margin <- if (is.null(label)) NA_character_ else label
    for (sort in c("none", "first", "last")) {
      for (with_id in c(FALSE, TRUE)) {
        id <- if (with_id) "sid" else NULL
        query <- expand_with_margins(
          input, .grouping = rollup(tidyselect::all_of("g")),
          .margin_label = label,
          .id = id, .sort = sort
        )
        expected <- data.frame(
          g = c("east", "west", margin, margin),
          sid = c(1L, 1L, 2L, 2L),
          .env = c("red", "blue", "red", "blue"),
          value = c(2L, 3L, 2L, 3L), check.names = FALSE
        )
        if (!with_id) {
          expected$sid <- NULL
        }
        actual <- as.data.frame(dplyr::collect(query))
        expect_identical(names(actual), names(expected))
        actual_sorted <- actual[vctrs::vec_order(actual), , drop = FALSE]
        expected_sorted <- expected[vctrs::vec_order(expected), , drop = FALSE]
        rownames(actual_sorted) <- NULL
        rownames(expected_sorted) <- NULL
        expect_identical(actual_sorted, expected_sorted)
        if (!identical(sort, "none")) {
          ordered_g <- if (identical(sort, "first")) {
            c(margin, margin, "east", "west")
          } else {
            c("east", "west", margin, margin)
          }
          expect_identical(actual$g, ordered_g)
        }
      }
    }
  }
}

test_that("SQLite expands .env payloads across labels and orders", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  check_sql_env_expansion_cases(con, "env_expansion_sqlite_cases")
})

test_that("DuckDB expands .env payloads across labels and orders", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  check_sql_env_expansion_cases(con, "duckdb_env_expansion_cases")
})
