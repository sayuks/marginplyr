test_that("dtplyr and Arrow use the normalized grouping contract", {
  data <- data.frame(
    a = c("x", "x", "y"),
    b = c("u", "v", "u"),
    value = 1:3
  )

  skip_if_not_installed("dtplyr")
  expect_no_message(
    dt_result <- summarize_with_margins(
      dtplyr::lazy_dt(data),
      n = dplyr::n(),
      gid = grouping_id(a, b),
      .grouping = rollup(a, b)
    ) |>
      dplyr::collect()
  )
  expect_setequal(unique(dt_result$gid), c(0L, 1L, 3L))
  expect_true(any(dt_result$a == "Total"))

  dt_rowwise <- nest_by_with_margins(
    dtplyr::lazy_dt(data),
    .grouping = rollup(a, b)
  )
  expect_s3_class(dt_rowwise, "rowwise_df")
  expect_equal(names(dt_rowwise), c("a", "b", "data"))

  skip_if_not_installed("arrow")
  arrow_result <- summarize_with_margins(
    arrow::Table$create(data),
    n = dplyr::n(),
    gid = grouping_id(a, b),
    .grouping = rollup(a, b)
  ) |>
    dplyr::collect()
  expect_setequal(unique(arrow_result$gid), c(0L, 1L, 3L))
  expect_true(any(arrow_result$a == "Total"))

  factor_result <- summarize_with_margins(
    arrow::Table$create(data.frame(a = factor(c("x", "y")))),
    n = dplyr::n(),
    .grouping = rollup(a)
  ) |>
    dplyr::collect()
  expect_type(factor_result$a, "character")
  expect_true("Total" %in% factor_result$a)
})

test_that("union adapters reserve user columns that look internal", {
  data <- data.frame(
    group = c("x", "x", "y"),
    value = 1:3,
    check.names = FALSE
  )
  data[["..marginplyr_key_1"]] <- 10:12
  expected <- data.frame(
    group = c("Total", "x", "y"),
    total = c(6L, 3L, 3L)
  )

  local <- summarize_with_margins(
    data,
    total = sum(value),
    .grouping = rollup(group)
  )
  expect_equal(dplyr::arrange(local, group), expected)

  skip_if_not_installed("dtplyr")
  dt_result <- summarize_with_margins(
    dtplyr::lazy_dt(data),
    total = sum(value),
    .grouping = rollup(group)
  ) |>
    dplyr::collect() |>
    dplyr::arrange(group)
  expect_equal(as.data.frame(dt_result), expected)

  skip_if_not_installed("arrow")
  arrow_result <- summarize_with_margins(
    arrow::Table$create(data),
    total = sum(value),
    .grouping = rollup(group)
  ) |>
    dplyr::collect() |>
    dplyr::arrange(group)
  expect_equal(as.data.frame(arrow_result), expected)
})

test_that("union adapters reserve generated summary names", {
  data <- data.frame(
    group = c("x", "x", "y"),
    value = 1:3,
    check.names = FALSE
  )
  data[["..marginplyr_key_1"]] <- 10:12
  data[["..marginplyr_key_1_"]] <- 20:22
  expected <- data.frame(
    group = c("Total", "x", "y"),
    check.names = FALSE
  )
  expected[["..marginplyr_key_1__"]] <- c(6L, 3L, 3L)

  local <- summarize_with_margins(
    data,
    dplyr::across(
      value,
      sum,
      .names = "..marginplyr_key_1__"
    ),
    .grouping = rollup(group)
  )
  expect_equal(dplyr::arrange(local, group), expected)

  skip_if_not_installed("dtplyr")
  dt_result <- summarize_with_margins(
    dtplyr::lazy_dt(data),
    dplyr::across(
      value,
      sum,
      .names = "..marginplyr_key_1__"
    ),
    .grouping = rollup(group)
  ) |>
    dplyr::collect() |>
    dplyr::arrange(group)
  expect_equal(as.data.frame(dt_result), expected)

  skip_if_not_installed("arrow")
  arrow_result <- summarize_with_margins(
    arrow::Table$create(data),
    dplyr::across(
      value,
      sum,
      .names = "..marginplyr_key_1__"
    ),
    .grouping = rollup(group)
  ) |>
    dplyr::collect() |>
    dplyr::arrange(group)
  expect_equal(as.data.frame(arrow_result), expected)
})

test_that("native adapters reserve generated summary names", {
  data <- data.frame(
    group = c("x", "x", "y"),
    value = 1:3,
    check.names = FALSE
  )
  data[["..marginplyr_grouping_1_"]] <- 10:12

  postgres <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_postgres())
  query <- summarize_with_margins(
    postgres,
    dplyr::across(
      value,
      sum,
      .names = "..marginplyr_grouping_1"
    ),
    .grouping = rollup(group)
  )
  sql <- dbplyr::sql_render(query)
  expect_match(sql, "\"..marginplyr_grouping_1\"", fixed = TRUE)
  expect_match(sql, "\"..marginplyr_grouping_1__\"", fixed = TRUE)

  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(
    con,
    data,
    "internal_summary_names",
    overwrite = TRUE,
    temporary = TRUE
  )
  result <- summarize_with_margins(
    remote,
    dplyr::across(
      value,
      sum,
      .names = "..marginplyr_grouping_1"
    ),
    .grouping = rollup(group)
  ) |>
    dplyr::collect() |>
    dplyr::arrange(group)
  expect_equal(
    result[["..marginplyr_grouping_1"]],
    c(6, 3, 3)
  )
})

test_that("column-wise summaries share one lazy-backend selection", {
  data <- data.frame(
    group = c("b", "a", "b"),
    value = c(1, 2, 3)
  )

  skip_if_not_installed("dtplyr")
  dt_result <- summarize_with_margins(
    dtplyr::lazy_dt(data),
    dplyr::across(
      dplyr::everything(),
      dplyr::n_distinct,
      .names = "n_{.col}"
    ),
    .grouping = rollup(group),
    .sort = FALSE
  ) |>
    dplyr::collect()
  expect_equal(names(dt_result), c("group", "n_value"))
  expect_setequal(dt_result$n_value, c(2L, 1L, 3L))

  skip_if_not_installed("arrow")
  arrow_result <- summarize_with_margins(
    arrow::Table$create(data),
    dplyr::across(
      dplyr::everything(),
      dplyr::n_distinct,
      .names = "n_{.col}"
    ),
    .grouping = rollup(group),
    .sort = FALSE
  ) |>
    dplyr::collect()
  expect_equal(names(arrow_result), c("group", "n_value"))
  expect_setequal(arrow_result$n_value, c(2L, 1L, 3L))

  skip_if_not_installed("dbplyr")
  sqlite <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_sqlite())
  sqlite_query <- summarize_with_margins(
    sqlite,
    dplyr::across(value, mean, .names = "mean_{.col}"),
    .grouping = rollup(group)
  )
  expect_match(
    dbplyr::sql_render(sqlite_query),
    "UNION ALL",
    fixed = TRUE
  )
  expect_false(grepl(
    "mean_group",
    dbplyr::sql_render(sqlite_query),
    fixed = TRUE
  ))

  postgres <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_postgres())
  expect_error(
    summarize_with_margins(
      postgres,
      dplyr::across(value, mean, .names = "group"),
      .grouping = rollup(group)
    ),
    "cannot overwrite grouping column.*`group`"
  )
})

test_that("dtplyr nesting retains original keys and empty rowwise behavior", {
  skip_if_not_installed("dtplyr")
  data <- data.frame(
    group = c("a", "a", "b"),
    item = c("x", "y", "z"),
    value = 1:3
  )

  kept_lazy <- nest_with_margins(
    dtplyr::lazy_dt(data),
    .grouping = rollup(group),
    .keep = TRUE
  )
  expect_s3_class(kept_lazy, "dtplyr_step")
  kept_nested <- dplyr::collect(kept_lazy)
  total_nested <- kept_nested[kept_nested$group == "Total", ]
  expect_equal(total_nested$data[[1]]$group, c("a", "a", "b"))

  kept <- nest_by_with_margins(
    dtplyr::lazy_dt(data),
    .grouping = rollup(group),
    .keep = TRUE
  )
  total <- kept[kept$group == "Total", ]
  expect_equal(total$data[[1]]$group, c("a", "a", "b"))

  empty <- nest_by_with_margins(dtplyr::lazy_dt(data[0, ]))
  expect_s3_class(empty, "rowwise_df")
  expect_equal(nrow(empty), 1L)
  expect_equal(names(empty$data[[1]]), names(data))
})

test_that("grouped lazy inputs use their groups as fixed keys", {
  data <- data.frame(
    year = c(2025L, 2025L, 2026L, 2026L),
    region = c("East", "West", "East", "West"),
    value = c(1, 10, 100, 1000)
  )

  skip_if_not_installed("dtplyr")
  grouped_dt <- dtplyr::lazy_dt(data) |>
    dplyr::group_by(year)

  dt_summary <- summarize_with_margins(
    grouped_dt,
    value = sum(value),
    .grouping = rollup(region),
    .sort = FALSE
  )
  expect_equal(dplyr::group_vars(dt_summary), character())
  expect_setequal(
    dplyr::collect(dt_summary)$value,
    c(1, 10, 11, 100, 1000, 1100)
  )

  dt_union <- expand_with_margins(
    grouped_dt,
    .grouping = rollup(region),
    .sort = FALSE
  )
  expect_equal(dplyr::group_vars(dt_union), character())

  dt_nest <- nest_with_margins(
    grouped_dt,
    .grouping = rollup(region),
    .sort = FALSE
  )
  expect_equal(dplyr::group_vars(dt_nest), character())
  expect_equal(names(dplyr::collect(dt_nest)), c("year", "region", "data"))

  dt_nest_by <- nest_by_with_margins(
    grouped_dt,
    .grouping = rollup(region),
    .sort = FALSE
  )
  expect_s3_class(dt_nest_by, "rowwise_df")
  expect_equal(dplyr::group_vars(dt_nest_by), c("year", "region"))

  skip_if_not_installed("arrow")
  grouped_arrow <- arrow::Table$create(data) |>
    dplyr::group_by(year)
  arrow_summary <- summarize_with_margins(
    grouped_arrow,
    value = sum(value),
    .grouping = rollup(region),
    .sort = FALSE
  )
  expect_equal(dplyr::group_vars(arrow_summary), character())
  expect_setequal(
    dplyr::collect(arrow_summary)$value,
    c(1, 10, 11, 100, 1000, 1100)
  )

  skip_if_not_installed("dbplyr")
  grouped_sql <- dbplyr::tbl_lazy(
    data,
    con = dbplyr::simulate_postgres()
  ) |>
    dplyr::group_by(year)
  sql_summary <- summarize_with_margins(
    grouped_sql,
    value = sum(value),
    .grouping = rollup(region),
    .sort = FALSE
  )
  expect_equal(dplyr::group_vars(sql_summary), character())
  expect_match(
    dbplyr::sql_render(sql_summary),
    'GROUPING SETS (("year", "region"), ("year"))',
    fixed = TRUE
  )
  expect_error(
    summarize_with_margins(
      grouped_sql,
      value = sum(value),
      .by = year,
      .grouping = rollup(region)
    ),
    "Can't supply `.by`"
  )
})

test_that("DuckDB executes grouped lazy input as fixed keys", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(
    year = c(2025L, 2025L, 2026L, 2026L),
    region = c("East", "West", "East", "West"),
    value = c(1, 10, 100, 1000)
  )
  dplyr::copy_to(
    con,
    data,
    "grouped_lazy_data",
    overwrite = TRUE,
    temporary = TRUE
  )
  grouped <- dplyr::tbl(con, "grouped_lazy_data") |>
    dplyr::group_by(year)

  query <- summarize_with_margins(
    grouped,
    value = sum(value),
    .grouping = rollup(region),
    .sort = TRUE
  )
  expect_equal(dplyr::group_vars(query), character())

  result <- dplyr::collect(query)
  expect_equal(result$year, c(2025L, 2025L, 2025L, 2026L, 2026L, 2026L))
  expect_equal(
    result$region,
    c("East", "Total", "West", "East", "Total", "West")
  )
  expect_equal(result$value, c(1, 11, 10, 100, 1100, 1000))
})

test_that("PostgreSQL renders native SQL under strict translation", {
  skip_if_not_installed("dbplyr")
  data <- data.frame(a = "x", b = "u", value = 1)
  remote <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_postgres())

  old <- options(dplyr.strict_sql = TRUE)
  on.exit(options(old), add = TRUE)
  expect_no_message(
    query <- summarize_with_margins(
      remote,
      n = dplyr::n(),
      ga = marginplyr::grouping_bit(a),
      gid = grouping_id(a, b),
      .grouping = grouping_sets(
        grouping_set(a, b),
        grouping_set()
      )
    )
  )
  sql <- dbplyr::sql_render(query)

  expect_match(sql, "GROUP BY GROUPING SETS", fixed = TRUE)
  expect_match(sql, "GROUPING(\"a\")", fixed = TRUE)
  expect_match(sql, "GROUPING(\"b\")", fixed = TRUE)
  expect_false(grepl("UNION ALL", sql, fixed = TRUE))
})

test_that("unconfirmed SQL dialects use UNION ALL", {
  skip_if_not_installed("dbplyr")
  data <- data.frame(a = "x", b = "u", value = 1)

  for (con in list(dbplyr::simulate_mysql(), dbplyr::simulate_sqlite())) {
    remote <- dbplyr::tbl_lazy(data, con = con)
    query <- summarize_with_margins(
      remote,
      n = dplyr::n(),
      gid = grouping_id(a, b),
      .grouping = rollup(a, b)
    )
    sql <- dbplyr::sql_render(query)
    expect_match(sql, "UNION ALL", fixed = TRUE)
    expect_false(grepl("GROUPING SETS", sql, fixed = TRUE))
  }
})

test_that("documented fallback dialects render portable UNION ALL SQL", {
  skip_if_not_installed("dbplyr")
  data <- data.frame(a = "x", b = "u", value = 1)
  simulators <- c(
    "simulate_access",
    "simulate_dbi",
    "simulate_hana",
    "simulate_hive",
    "simulate_impala",
    "simulate_mariadb",
    "simulate_mssql",
    "simulate_mysql",
    "simulate_odbc",
    "simulate_oracle",
    "simulate_redshift",
    "simulate_snowflake",
    "simulate_spark_sql",
    "simulate_sqlite",
    "simulate_teradata"
  )

  for (simulator in simulators) {
    con <- getExportedValue("dbplyr", simulator)()
    remote <- dbplyr::tbl_lazy(data, con = con)
    query <- summarize_with_margins(
      remote,
      n = dplyr::n(),
      gid = grouping_id(a, b),
      .grouping = rollup(a, b)
    )
    sql <- dbplyr::sql_render(query)
    expect_match(sql, "UNION ALL", fixed = TRUE, info = simulator)
    expect_false(
      grepl("GROUPING SETS", sql, fixed = TRUE),
      info = simulator
    )
  }
})

test_that("PostgreSQL duplicate keep falls back conservatively", {
  skip_if_not_installed("dbplyr")
  remote <- dbplyr::tbl_lazy(
    data.frame(a = "x"),
    con = dbplyr::simulate_postgres()
  )
  query <- summarize_with_margins(
    remote,
    n = dplyr::n(),
    .grouping = grouping_sets(grouping_set(a), grouping_set(a)),
    .duplicates = "keep"
  )
  expect_match(dbplyr::sql_render(query), "UNION ALL", fixed = TRUE)
})

test_that("DuckDB executes native grouping sets with unambiguous bits", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(
    a = c("x", NA_character_, "Total"),
    b = c("u", "u", "v"),
    value = 1:3
  )
  dplyr::copy_to(con, data, "grouping_data", overwrite = TRUE, temporary = TRUE)
  remote <- dplyr::tbl(con, "grouping_data")

  result <- summarize_with_margins(
    remote,
    n = dplyr::n(),
    ga = grouping_bit(a),
    gid = grouping_id(a, b),
    .grouping = rollup(a, b),
    .check_margin_label = FALSE
  ) |>
    dplyr::collect()

  expect_true(any(is.na(result$a) & result$gid == 0))
  expect_true(any(is.na(result$a) & result$gid == 1))
  expect_true(any(result$a == "Total" & result$ga == 0))
  expect_true(any(result$a == "Total" & result$ga == 1))
  expect_setequal(unique(result$gid), c(0, 1, 3))
})

test_that("DuckDB keeps input types available to summary expressions", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(a = c(1L, 1L, 2L), value = 1:3)
  dplyr::copy_to(
    con,
    data,
    "numeric_groups",
    overwrite = TRUE,
    temporary = TRUE
  )

  result <- summarize_with_margins(
    dplyr::tbl(con, "numeric_groups"),
    sum_a = sum(a),
    .grouping = rollup(a)
  ) |>
    dplyr::collect()

  expect_equal(result$sum_a[result$a == "Total"], 4)
})

test_that("DuckDB native and UNION adapters agree", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(a = c("x", "x", "y"), b = c("u", "v", "u"), value = 1:3)
  dplyr::copy_to(con, data, "adapter_data", overwrite = TRUE, temporary = TRUE)
  remote <- dplyr::tbl(con, "adapter_data")
  spec <- grouping_spec(rollup(a), cube(b))

  native <- summarize_with_margins(
    remote,
    total = sum(value),
    gid = grouping_id(a, b),
    .grouping = spec
  ) |>
    dplyr::collect() |>
    dplyr::arrange(a, b, gid)

  plan <- compile_grouping_spec(spec, names(data))
  dots <- rlang::quos(total = sum(value), gid = grouping_id(a, b))
  fallback <- summarize_margin_union(
    remote,
    dots = dots,
    plan = plan,
    .margin_label = "Total",
    column_info = margin_column_info(remote, plan$dimensions),
    reserved_names = unique(c(names(data), names(dots)))
  ) |>
    dplyr::collect() |>
    dplyr::arrange(a, b, gid)

  expect_equal(native, fallback)
})

test_that("DuckDB duplicate keep and downstream verbs remain lazy", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dplyr::copy_to(
    con,
    data.frame(a = c("x", "y"), value = 1:2),
    "duplicate_data",
    overwrite = TRUE,
    temporary = TRUE
  )
  remote <- dplyr::tbl(con, "duplicate_data")

  duplicated <- summarize_with_margins(
    remote,
    n = dplyr::n(),
    .grouping = grouping_sets(grouping_set(a), grouping_set(a)),
    .duplicates = "keep"
  )
  expect_match(dbplyr::sql_render(duplicated), "GROUPING SETS", fixed = TRUE)
  expect_equal(nrow(dplyr::collect(duplicated)), 4L)

  downstream <- duplicated |>
    dplyr::select(a, n) |>
    dplyr::rename(group = a) |>
    dplyr::filter(n > 0) |>
    dplyr::mutate(n_plus_one = n + 1)
  expect_s3_class(downstream, "tbl_lazy")
  expect_equal(nrow(dplyr::collect(downstream)), 4L)
})

test_that("DuckDB safely quotes factor identifiers and labels", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(check.names = FALSE, "odd name" = factor(c("A", "B")))
  dplyr::copy_to(con, data, "factor_data", overwrite = TRUE, temporary = TRUE)

  result <- summarize_with_margins(
    dplyr::tbl(con, "factor_data"),
    n = dplyr::n(),
    .grouping = rollup(`odd name`),
    .margin_label = "O'Total"
  ) |>
    dplyr::collect()

  expect_true(is.factor(result[["odd name"]]))
  expect_true("O'Total" %in% levels(result[["odd name"]]))
})
