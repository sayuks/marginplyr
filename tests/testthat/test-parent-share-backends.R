test_that("Parent shares preserve columns, grouping, and laziness", {
  data <- data.frame(
    fixed = c("a", "a", "b"),
    group = c("x", "y", "x"),
    value = c(1, 3, 6)
  )
  summarize <- function(source) {
    summarize_with_margins(
      source,
      total = sum(value),
      share = share_of_parent(total),
      rows = dplyr::n(),
      .by = fixed,
      .grouping = rollup(group),
      .id = "set",
      .margin_label = NULL
    )
  }
  expected_names <- c("fixed", "group", "set", "total", "share", "rows")

  local <- summarize(data) |>
    dplyr::arrange(fixed, set, group)
  expect_identical(names(local), expected_names)
  expect_identical(dplyr::group_vars(local), character())

  sql <- summarize(dbplyr::tbl_lazy(
    data,
    con = dbplyr::simulate_sqlite()
  ))
  expect_s3_class(sql, "tbl_lazy")
  expect_identical(as.character(dplyr::tbl_vars(sql)), expected_names)
  expect_identical(dplyr::group_vars(sql), character())
  expect_no_error(dbplyr::sql_render(sql))

  if (rlang::is_installed("dtplyr")) {
    lazy <- summarize(dtplyr::lazy_dt(data))
    expect_s3_class(lazy, "dtplyr_step")
    expect_identical(as.character(dplyr::tbl_vars(lazy)), expected_names)
    expect_identical(dplyr::group_vars(lazy), character())
    expect_equal(
      as.data.frame(
        dplyr::collect(lazy) |>
          dplyr::arrange(fixed, set, group)
      ),
      as.data.frame(local)
    )
  }
})

test_that("dtplyr and Arrow batch Parent shares with missing-safe matching", {
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
  cases <- list()
  if (rlang::is_installed("dtplyr")) {
    cases$dtplyr <- list(
      source = dtplyr::lazy_dt(data),
      class = "dtplyr_step"
    )
  }
  if (rlang::is_installed("arrow")) {
    cases$arrow <- list(
      source = arrow::Table$create(data),
      class = "arrow_dplyr_query"
    )
  }
  skip_if(length(cases) == 0L, "Neither dtplyr nor Arrow is installed")

  for (backend in names(cases)) {
    query <- summarize(cases[[backend]]$source)
    expect_s3_class(query, cases[[backend]]$class)
    expect_equal(
      as.data.frame(dplyr::collect(query)),
      as.data.frame(expected),
      info = backend
    )
  }
})

parent_sql_count <- function(sql, pattern) {
  lengths(gregexpr(pattern, sql, fixed = TRUE))
}

parent_lazy_probe_capture <- new.env(parent = emptyenv())

parent_lazy_probe_collect <- function(con, sql, ...) {
  parent_lazy_probe_capture$n <- parent_lazy_probe_capture$n + 1L
  stop("Parent-share planning must not execute a schema probe.", call. = FALSE)
}

test_that("PostgreSQL renders one staged Parent-share mapping for all measures", { # nolint: line_length_linter
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

test_that("general dbplyr leaves incompatible summary types to execution", {
  registerS3method(
    "db_collect",
    "parent_lazy_probe_connection",
    parent_lazy_probe_collect,
    envir = asNamespace("dbplyr")
  )
  con <- dbplyr::simulate_dbi()
  class(con) <- c("parent_lazy_probe_connection", class(con))
  remote <- dbplyr::tbl_lazy(
    data.frame(group = "x", label = "value"),
    con = con
  )
  parent_lazy_probe_capture$n <- 0L

  query <- summarize_with_margins(
    remote,
    label = min(label),
    share = share_of_parent(label),
    .grouping = rollup(group),
    .margin_label = NULL
  )
  expect_s3_class(query, "tbl_lazy")
  expect_identical(parent_lazy_probe_capture$n, 0L)

  sql <- dbplyr::sql_render(query)
  expect_identical(parent_lazy_probe_capture$n, 0L)
  expect_match(sql, "CAST(", fixed = TRUE)
  expect_match(sql, "LEFT JOIN", fixed = TRUE)
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

test_that("DuckDB Parent shares agree across native, portable, and local paths", { # nolint: line_length_linter
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(
    fixed = c(NA_character_, NA_character_, "a", "a"),
    group = c(NA_character_, "x", NA_character_, "x"),
    item = c("i", "j", "i", "j"),
    revenue = c(1, 3, 2, 2),
    units = c(1L, 3L, 0L, 0L),
    missing_source = c(1, 3, 2, 2)
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
      missing_parent = dplyr::if_else(
        dplyr::n() > 1L,
        NA_real_,
        sum(missing_source)
      ),
      revenue_share = share_of_parent(revenue),
      missing_parent_share = share_of_parent(missing_parent),
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
  expect_true(all(is.na(
    expected$missing_parent_share[expected$level == 1L]
  )))
  expect_true(all(
    expected$missing_parent_share[expected$level == 3L] == 1
  ))
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

test_that("lazy Parent shares preserve empty-input root and partition behavior", { # nolint: line_length_linter
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
  summarize <- function(source, include_id) {
    id_name <- if (include_id) "set" else NULL
    result <- summarize_with_margins(
      source,
      total = sum(value),
      share = share_of_parent(total),
      .grouping = rollup(group, group),
      .duplicates = "keep",
      .id = id_name,
      .margin_label = NULL
    )
    if (include_id) {
      return(dplyr::arrange(result, set, group))
    }
    result |>
      dplyr::arrange(group, total, share)
  }
  expected <- summarize(data, include_id = TRUE)
  expected_without_id <- summarize(data, include_id = FALSE)
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
      as.data.frame(dplyr::collect(summarize(
        sources[[backend]],
        include_id = TRUE
      ))),
      as.data.frame(expected),
      info = backend
    )
    without_id <- summarize(sources[[backend]], include_id = FALSE)
    if (identical(backend, "duckdb")) {
      expect_match(
        dbplyr::sql_render(without_id),
        "UNION ALL",
        fixed = TRUE
      )
    }
    expect_equal(
      as.data.frame(dplyr::collect(without_id)),
      as.data.frame(expected_without_id),
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
