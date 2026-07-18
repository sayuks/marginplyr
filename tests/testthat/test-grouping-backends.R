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
      ga = grouping(a),
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
    ga = grouping(a),
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
    column_info = margin_column_info(remote, plan$dimensions)
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
