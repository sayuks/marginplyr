test_that("summarize_with_margins works across all remote backends", {
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

  df <- tibble::tibble(
    region = c("East", "East", "West", "West"),
    store = c("A", "B", "C", "C"),
    sales = c(100, 200, 300, 400)
  )

  for (db_name in names(dbs)) {
    con <- dbs[[db_name]]

    remote_tbl <- dbplyr::tbl_lazy(df, con = con)

    result <- summarize_with_margins(
      remote_tbl,
      total_sales = sum(sales, na.rm = TRUE),
      .margins = c(region, store)
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
  }
})
