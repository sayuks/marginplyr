test_that("dtplyr batches Parent shares with missing-safe parent matching", {
  skip_if_not_installed("dtplyr")
  data <- data.frame(
    fixed = c(NA_character_, NA_character_, "a", "a"),
    group = c(NA_character_, "x", NA_character_, "x"),
    revenue = c(1, 3, 2, 2),
    units = c(1L, 3L, 0L, 0L)
  )

  summarize <- function(source) {
    summarize_with_margins(
      source,
      level = grouping_id(group),
      dplyr::across(c(revenue, units), sum),
      dplyr::across(
        c(revenue, units),
        share_of_parent,
        .names = "{.col}_share"
      ),
      .by = fixed,
      .grouping = rollup(group),
      .margin_label = NULL
    ) |>
      dplyr::arrange(fixed, level, group)
  }

  expected <- summarize(data)
  query <- summarize(dtplyr::lazy_dt(data))

  expect_s3_class(query, "dtplyr_step")
  expect_equal(
    as.data.frame(dplyr::collect(query)),
    as.data.frame(expected)
  )
})

test_that("Arrow batches Parent shares while preserving lazy execution", {
  skip_if_not_installed("arrow")
  data <- data.frame(
    fixed = c(NA_character_, NA_character_, "a", "a"),
    group = c(NA_character_, "x", NA_character_, "x"),
    revenue = c(1, 3, 2, 2),
    units = c(1L, 3L, 0L, 0L)
  )

  summarize <- function(source) {
    summarize_with_margins(
      source,
      level = grouping_id(group),
      dplyr::across(c(revenue, units), sum),
      dplyr::across(
        c(revenue, units),
        share_of_parent,
        .names = "{.col}_share"
      ),
      .by = fixed,
      .grouping = rollup(group),
      .margin_label = NULL
    ) |>
      dplyr::arrange(fixed, level, group)
  }

  expected <- summarize(data)
  query <- summarize(arrow::Table$create(data))

  expect_s3_class(query, "arrow_dplyr_query")
  expect_equal(
    as.data.frame(dplyr::collect(query)),
    as.data.frame(expected)
  )
})

parent_sql_count <- function(sql, pattern) {
  lengths(gregexpr(pattern, sql, fixed = TRUE))
}

test_that("PostgreSQL renders one staged Parent-share mapping for all measures", {
  data <- data.frame(
    region = "East",
    store = "A",
    revenue = 10,
    units = 1L
  )
  remote <- dbplyr::tbl_lazy(data, con = dbplyr::simulate_postgres())

  one <- summarize_with_margins(
    remote,
    revenue = sum(revenue),
    revenue_share = share_of_parent(revenue),
    .grouping = rollup(region, store),
    .margin_label = NULL
  )
  many <- summarize_with_margins(
    remote,
    revenue = sum(revenue),
    units = sum(units),
    revenue_share = share_of_parent(revenue),
    units_share = share_of_parent(units),
    .grouping = rollup(region, store),
    .margin_label = NULL
  )
  one_sql <- dbplyr::sql_render(one)
  many_sql <- dbplyr::sql_render(many)

  expect_match(many_sql, "GROUP BY GROUPING SETS", fixed = TRUE)
  expect_identical(parent_sql_count(many_sql, "LEFT JOIN"), 1L)
  expect_identical(
    parent_sql_count(many_sql, "GROUP BY GROUPING SETS"),
    parent_sql_count(one_sql, "GROUP BY GROUPING SETS")
  )
  expect_match(many_sql, "IS NULL AND", fixed = TRUE)
  expect_match(many_sql, "CAST(", fixed = TRUE)
})

test_that("fallback simulators render portable staged Parent-share SQL", {
  data <- data.frame(
    fixed = NA_character_,
    group = NA_character_,
    value = 1
  )
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
    remote <- dbplyr::tbl_lazy(
      data,
      con = getExportedValue("dbplyr", simulator)()
    )
    query <- summarize_with_margins(
      remote,
      total = sum(value),
      share = share_of_parent(total),
      .by = fixed,
      .grouping = rollup(group),
      .margin_label = NULL
    )
    sql <- dbplyr::sql_render(query)

    expect_match(sql, "UNION ALL", fixed = TRUE, info = simulator)
    expect_identical(
      parent_sql_count(sql, "LEFT JOIN"),
      1L,
      info = simulator
    )
    expect_match(sql, "IS NULL AND", fixed = TRUE, info = simulator)
    expect_match(sql, "(CAST|CDBL)\\(", info = simulator)
    expect_false(
      grepl("GROUPING SETS", sql, fixed = TRUE),
      info = simulator
    )
  }
})

test_that("DuckDB Parent shares agree across native, portable, and local paths", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(
    fixed = c(NA_character_, NA_character_, "a", "a"),
    group = c(NA_character_, "x", NA_character_, "x"),
    item = c("i", "j", "i", "j"),
    revenue = c(1, 3, 2, 2),
    units = c(1L, 3L, 0L, 0L)
  )
  remote <- dplyr::copy_to(
    con,
    data,
    "parent_share_data",
    overwrite = TRUE,
    temporary = TRUE
  )

  summarize <- function(source, duplicates) {
    summarize_with_margins(
      source,
      level = grouping_id(group, item),
      revenue = sum(revenue),
      units = sum(units),
      revenue_share = share_of_parent(revenue),
      dplyr::across(
        units,
        share_of_parent,
        .names = "{.col}_share"
      ),
      .by = fixed,
      .grouping = rollup(group, item),
      .duplicates = duplicates,
      .id = "set",
      .margin_label = NULL
    )
  }

  expected <- summarize(data, "drop") |>
    dplyr::arrange(fixed, set, group, item)
  native <- summarize(remote, "drop")
  portable <- summarize(remote, "keep")

  expect_s3_class(native, "tbl_lazy")
  expect_match(dbplyr::sql_render(native), "GROUPING SETS", fixed = TRUE)
  expect_match(dbplyr::sql_render(portable), "UNION ALL", fixed = TRUE)
  expect_equal(
    as.data.frame(
      dplyr::collect(native) |>
        dplyr::arrange(fixed, set, group, item)
    ),
    as.data.frame(expected)
  )
  expect_equal(
    as.data.frame(
      dplyr::collect(portable) |>
        dplyr::arrange(fixed, set, group, item)
    ),
    as.data.frame(expected)
  )

  composed <- native |>
    dplyr::select(fixed, group, item, set, revenue, revenue_share) |>
    dplyr::filter(set <= 3L) |>
    dplyr::mutate(revenue_percent = 100 * revenue_share) |>
    dplyr::arrange(fixed, set, group, item)
  expect_s3_class(composed, "tbl_lazy")
  expect_output(dplyr::show_query(composed), "SELECT")
  expect_equal(
    as.data.frame(dplyr::collect(composed)),
    as.data.frame(
      expected |>
        dplyr::select(fixed, group, item, set, revenue, revenue_share) |>
        dplyr::filter(set <= 3L) |>
        dplyr::mutate(revenue_percent = 100 * revenue_share) |>
        dplyr::arrange(fixed, set, group, item)
    )
  )
})

test_that("lazy Parent shares preserve empty-input root and partition behavior", {
  empty <- data.frame(group = character(), value = double())
  sources <- list()

  if (rlang::is_installed("dtplyr")) {
    sources$dtplyr <- dtplyr::lazy_dt(empty)
  }
  if (rlang::is_installed("arrow")) {
    sources$arrow <- arrow::Table$create(empty)
  }
  if (rlang::is_installed("duckdb") && rlang::is_installed("DBI")) {
    con <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    sources$duckdb <- dplyr::copy_to(
      con,
      empty,
      "empty_parent_share_data",
      overwrite = TRUE,
      temporary = TRUE
    )
  }
  skip_if(length(sources) == 0L, "No supported lazy backend is installed")

  for (backend in names(sources)) {
    source <- sources[[backend]]
    root <- summarize_with_margins(
      source,
      total = sum(value),
      share = share_of_parent(total),
      .grouping = rollup(group),
      .margin_label = NULL
    ) |>
      dplyr::collect()
    partitioned <- summarize_with_margins(
      source,
      total = sum(value),
      share = share_of_parent(total),
      .by = group,
      .grouping = rollup(value),
      .margin_label = NULL
    ) |>
      dplyr::collect()

    expect_identical(root$share, 1, info = backend)
    expect_identical(nrow(partitioned), 0L, info = backend)
    expect_type(partitioned$share, "double")
  }
})

test_that("lazy Parent shares skip duplicate grouping-set occurrences", {
  data <- data.frame(group = c("x", "y"), value = c(1, 3))
  summarize <- function(source) {
    summarize_with_margins(
      source,
      total = sum(value),
      share = share_of_parent(total),
      .grouping = rollup(group, group),
      .duplicates = "keep",
      .id = "set",
      .margin_label = NULL
    ) |>
      dplyr::arrange(set, group)
  }
  expected <- summarize(data)
  sources <- list()
  if (rlang::is_installed("dtplyr")) {
    sources$dtplyr <- dtplyr::lazy_dt(data)
  }
  if (rlang::is_installed("arrow")) {
    sources$arrow <- arrow::Table$create(data)
  }
  if (rlang::is_installed("duckdb") && rlang::is_installed("DBI")) {
    con <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    sources$duckdb <- dplyr::copy_to(
      con,
      data,
      "duplicate_parent_share_data",
      overwrite = TRUE,
      temporary = TRUE
    )
  }
  skip_if(length(sources) == 0L, "No supported lazy backend is installed")

  for (backend in names(sources)) {
    expect_equal(
      as.data.frame(dplyr::collect(summarize(sources[[backend]]))),
      as.data.frame(expected),
      info = backend
    )
  }
})

test_that("lazy Parent-share staging avoids adversarial user-name collisions", {
  group_name <- "..marginplyr_parent_key_1"
  value_name <- "..marginplyr_parent_value_1"
  summary_name <- "..marginplyr_parent_value_1_"
  share_name <- "..marginplyr_parent_set_1_"
  id_name <- "..marginplyr_parent_match_1_"
  data <- data.frame(
    c(group_name, "x"),
    c(1, 3),
    10:11,
    20:21,
    check.names = FALSE
  )
  names(data) <- c(
    group_name,
    value_name,
    "..marginplyr_parent_set_1",
    "..marginplyr_parent_match_1"
  )

  summarize <- function(source) {
    rlang::inject(summarize_with_margins(
      source,
      !!summary_name := sum(.data[[value_name]]),
      !!share_name := share_of_parent(!!rlang::sym(summary_name)),
      .grouping = rollup(dplyr::all_of(group_name)),
      .margin_label = group_name,
      .check_margin_label = FALSE,
      .id = id_name
    )) |>
      dplyr::arrange(.data[[id_name]], .data[[group_name]])
  }

  expected <- summarize(data)
  simulated <- summarize(dbplyr::tbl_lazy(
    data,
    con = dbplyr::simulate_sqlite()
  ))
  expect_identical(
    as.character(dplyr::tbl_vars(simulated)),
    names(expected)
  )
  expect_no_error(dbplyr::sql_render(simulated))

  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(
    con,
    data,
    "adversarial_parent_share_names",
    overwrite = TRUE,
    temporary = TRUE
  )
  expect_equal(
    as.data.frame(dplyr::collect(summarize(remote))),
    as.data.frame(expected)
  )
})
