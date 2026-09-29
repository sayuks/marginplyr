test_that("SQL dialects render .env as a Parent and Total share source", {
  source <- data.frame(g = c("east", "west"), value = c(2, 3))
  simulators <- available_simulators(c(
    "simulate_access", "simulate_dbi", "simulate_hana",
    "simulate_hive", "simulate_impala", "simulate_mariadb",
    "simulate_mssql", "simulate_mysql", "simulate_odbc",
    "simulate_oracle", "simulate_postgres", "simulate_redshift",
    "simulate_snowflake", "simulate_spark_sql", "simulate_sqlite",
    "simulate_teradata"
  ))
  source_name <- ".env"

  for (simulator in simulators) {
    remote <- dbplyr::tbl_lazy(
      source, con = getExportedValue("dbplyr", simulator)()
    )
    query <- summarize_with_margins(
      remote,
      !!source_name := sum(value),
      parent = share_of_parent(.env),
      total = share_of_total(.env),
      .grouping = rollup(g), .check_share_source = FALSE
    )
    expect_true(nzchar(as.character(dbplyr::sql_render(query))),
                info = simulator)
  }
})

expect_sql_env_share_values <- function(backend) {
  old_options <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old_options), add = TRUE)
  source <- data.frame(
    partition = c("A", "A", "B", "B"),
    g = c("east", "west", "east", "west"),
    h = c("one", "two", "one", "two"),
    value = c(2, 3, 5, 7)
  )
  source_name <- ".env"
  # Grouping specifications capture source-column names for later selection.
  # nolint start: object_usage_linter.
  plans <- list(
    rollup = rollup(g),
    composite = rollup(g, h),
    repeated = rollup(g, g),
    cube = cube(g),
    explicit = grouping_sets(grouping_set(g), grouping_set())
  )
  # nolint end
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
  remote <- dplyr::copy_to(
    con, source, paste0("env_share_source_", backend), temporary = TRUE
  )
  for (plan_name in names(plans)) {
    grouping <- plans[[plan_name]]
    duplicates <- if (identical(plan_name, "repeated")) "keep" else "drop"
    # Summary expressions resolve columns and the staged `.env` source in
    # dplyr's data mask; these names are not lexical bindings here.
    # nolint start: object_usage_linter.
    if (plan_name %in% c("rollup", "composite", "repeated")) {
      local <- summarize_with_margins(
        source, !!source_name := sum(value),
        parent = share_of_parent(.env), total = share_of_total(.env),
        .by = partition, .grouping = grouping, .id = "set", .sort = "last",
        .duplicates = duplicates
      )
      query <- summarize_with_margins(
        remote, !!source_name := sum(value),
        parent = share_of_parent(.env), total = share_of_total(.env),
        .by = partition, .grouping = grouping, .id = "set", .sort = "last",
        .duplicates = duplicates,
        .check_share_source = !identical(backend, "SQLite")
      )
    } else {
      local <- summarize_with_margins(
        source, !!source_name := sum(value),
        total = share_of_total(.env),
        .by = partition, .grouping = grouping, .id = "set", .sort = "last"
      )
      query <- summarize_with_margins(
        remote, !!source_name := sum(value),
        total = share_of_total(.env),
        .by = partition, .grouping = grouping, .id = "set", .sort = "last",
        .check_share_source = !identical(backend, "SQLite")
      )
    }
    # nolint end
    sent <- last_sent_queries()
    expect_identical(tail(sent$purpose, 1L), "result")
    if (identical(backend, "SQLite")) {
      expect_identical(sent$purpose, "result")
    }
    expected <- as.data.frame(local)
    for (actual in list(
      dplyr::collect(query),
      dplyr::collect(dplyr::compute(
        query, name = paste0("env_share_", backend, "_", plan_name)
      ))
    )) {
      expect_equal(as.data.frame(actual), expected,
                   info = paste(backend, plan_name))
    }
  }
}

test_that("SQLite .env shares match local values and materialize", {
  skip_if_suggest_absent("RSQLite", "DBI")
  expect_sql_env_share_values("SQLite")
})

test_that("DuckDB .env shares match local values and materialize", {
  skip_if_suggest_absent("duckdb", "DBI")
  expect_sql_env_share_values("DuckDB")
})

test_that("SQL .env source keeps lexical pronouns and source validation", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(
    con, data.frame(g = c("east", "west"), value = c(2, 3)),
    "env_share_lexical_source", temporary = TRUE
  )
  source_name <- ".env"
  offset <- 4
  query <- summarize_with_margins(
    remote, !!source_name := sum(value) + .env$offset,
    total = share_of_total(.env), .grouping = rollup(g), .sort = "last"
  )
  actual <- dplyr::collect(query)
  expect_equal(actual[[".env"]], c(6, 7, 9))
  expect_equal(actual$total, c(6 / 9, 7 / 9, 1))

  invalid <- summarize_with_margins(
    remote, !!source_name := min(as.character(value)),
    total = share_of_total(.env), .grouping = rollup(g)
  )
  error <- expect_error(dplyr::collect(invalid))
  expect_match(conditionMessage(error), ".env", fixed = TRUE)
})
