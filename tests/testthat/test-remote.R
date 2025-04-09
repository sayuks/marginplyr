dbs <- list(
  access     = dbplyr::simulate_access(),
  hana       = dbplyr::simulate_hana(),
  hive       = dbplyr::simulate_hive(),
  impala     = dbplyr::simulate_impala(),
  mssql      = dbplyr::simulate_mssql(),
  mysql      = dbplyr::simulate_mysql(),
  mariadb    = dbplyr::simulate_mariadb(),
  odbc       = dbplyr::simulate_odbc(),
  oracle     = dbplyr::simulate_oracle(),
  postgres   = dbplyr::simulate_postgres(),
  redshift   = dbplyr::simulate_redshift(),
  snowflake  = dbplyr::simulate_snowflake(),
  spark_sql  = dbplyr::simulate_spark_sql(),
  sqlite     = dbplyr::simulate_sqlite(),
  teradata   = dbplyr::simulate_teradata()
)

df <- dplyr::tribble(
  ~year, ~region, ~store, ~product, ~sales,
  2020, "East", "E1", "X", 200,
  2020, "East", "E2", "Y", 500,
  2020, "West", "W1", "Z", 900,
  2020, "West", "W2", "X", 400,
  2021, "East", "E1", "Y", 600,
  2021, "West", "W1", "Z", 800
)

test_that("summarize_with_margins works across all remote backends", {
  for (db_name in names(dbs)) {
    con <- dbs[[db_name]]

    remote_tbl <- dbplyr::tbl_lazy(df, con = con)

    result <- summarize_with_margins(
      remote_tbl,
      total_sales = sum(sales, na.rm = TRUE),
      .rollup = c(region, store),
      .cube = product,
      .by = year
    )

    sql <- dbplyr::sql_render(result)

    expect_true(
      inherits(sql, "sql"),
      info = paste0("DB: ", db_name, " - SQL object not returned")
    )

    expect_match(
      sql,
      "GROUP BY|ROLLUP|GROUPING SETS",
      ignore.case = TRUE,
      info = paste0("DB: ", db_name, " - GROUP/ROLLUP/SET missing")
    )

    expect_match(
      sql,
      "(all)",
      ignore.case = TRUE,
      info = paste0("DB: ", db_name, " - \"(all)\" missing")
    )
  }
})

test_that("union_all_with_margins works across all remote backends", {
  for (db_name in names(dbs)) {
    con <- dbs[[db_name]]

    remote_tbl <- dbplyr::tbl_lazy(df, con = con)

    result <- union_all_with_margins(
      remote_tbl,
      .rollup = c(region, store),
      .cube = product,
      .by = year
    )

    sql <- dbplyr::sql_render(result)

    expect_true(
      inherits(sql, "sql"),
      info = paste0("DB: ", db_name, " - SQL object not returned")
    )

    expect_match(
      sql,
      "UNION ALL",
      ignore.case = TRUE,
      info = paste0("DB: ", db_name, " - UNION ALL missing")
    )

    expect_match(
      sql,
      "(all)",
      ignore.case = TRUE,
      info = paste0("DB: ", db_name, " - \"(all)\" missing")
    )
  }
})
