margin_check_capture <- new.env(parent = emptyenv())

margin_check_collect <- function(con, sql, ...) {
  margin_check_capture$sql <- as.character(sql)
  check_names <- attr(con, "check_names", exact = TRUE)
  result <- as.data.frame(rep(list(FALSE), length(check_names)))
  names(result) <- check_names
  result
}

bad_query_sql_build <- function(op, con, ...) {
  dbplyr::sql("SELECT 1")
}

backend_dialect_error <- function(con) {
  rlang::abort(
    "Backend dialect classification failed.",
    class = "marginplyr_test_backend_error",
    provenance = "backend dialect"
  )
}

test_that("backend classification preserves backend conditions", {
  registerS3method(
    "sql_dialect",
    "marginplyr_backend_error_connection",
    backend_dialect_error,
    envir = asNamespace("dbplyr")
  )
  source <- dbplyr::tbl_lazy(
    data.frame(group = "x"),
    con = dbplyr::simulate_sqlite()
  )
  class(source$con) <- c(
    "marginplyr_backend_error_connection",
    class(source$con)
  )

  error <- expect_error(
    inspect_grouping(source, .grouping = rollup(group)),
    "Backend dialect classification failed"
  )

  expect_s3_class(error, "marginplyr_test_backend_error")
  expect_identical(error$provenance, "backend dialect")
  expect_false(inherits(error, "marginplyr_error"))
})

selection_proxy_capture <- new.env(parent = emptyenv())

proxy_counter_head <- function(x, ...) {
  result <- NextMethod()
  class(result) <- unique(c("margin_selection_proxy_counter", class(result)))
  result
}

proxy_counter_collect <- function(x, ...) {
  selection_proxy_capture$n <- selection_proxy_capture$n + 1L
  NextMethod()
}

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

test_that("Arrow schema metadata supports predicates and computed queries", {
  skip_if_not_installed("arrow")

  data <- data.frame(
    group = c("x", "x", "y"),
    value = 1:3
  )
  table <- arrow::Table$create(data)
  sources <- list(
    table,
    arrow::InMemoryDataset$create(table),
    dplyr::mutate(table, doubled = value * 2)
  )

  for (source in sources) {
    result <- summarize_with_margins(
      source,
      total = sum(value),
      .grouping = rollup(where(is.character)),
      .margin_label = NULL
    ) |>
      dplyr::collect()

    expect_equal(names(result), c("group", "total"))
    expect_setequal(result$total, c(3L, 3L, 6L))
    expect_true(any(is.na(result$group)))
  }

  factor_result <- summarize_with_margins(
    arrow::Table$create(
      data.frame(group = factor(c("x", "y")), value = 1:2)
    ),
    total = sum(value),
    .grouping = rollup(group),
    .margin_label = NULL
  ) |>
    dplyr::collect()
  expect_setequal(as.character(factor_result$group), c("x", "y", NA))

  numeric_result <- summarize_with_margins(
    table,
    n = dplyr::n(),
    .grouping = rollup(where(is.numeric)),
    .margin_label = NULL
  ) |>
    dplyr::collect()
  expect_identical(names(numeric_result), c("value", "n"))
  expect_true(anyNA(numeric_result$value))
})

test_that("Arrow metadata preserves ordered dictionaries without collecting", {
  skip_if_not_installed("arrow")
  registerS3method(
    "head",
    "margin_selection_proxy_counter",
    proxy_counter_head,
    envir = asNamespace("utils")
  )
  registerS3method(
    "collect",
    "margin_selection_proxy_counter",
    proxy_counter_collect,
    envir = asNamespace("dplyr")
  )

  ordered_group <- factor(
    c("b", "a", "b"),
    levels = c("a", "b"),
    ordered = TRUE
  )
  source <- arrow::Table$create(
    data.frame(group = ordered_group, value = 1:3)
  ) |>
    dplyr::mutate(doubled = value * 2)
  class(source) <- c("margin_selection_proxy_counter", class(source))
  selection_proxy_capture$n <- 0L

  query <- summarize_with_margins(
    source,
    total = sum(value),
    .grouping = rollup(where(is.factor)),
    .margin_label = NULL
  )

  expect_identical(selection_proxy_capture$n, 0L)
  result <- dplyr::collect(query)
  expect_s3_class(result$group, "ordered")
  expect_identical(levels(result$group), c("a", "b"))
  expect_true(anyNA(result$group))
  expect_setequal(result$total, c(2L, 4L, 6L))
})

test_that("dtplyr constructs one typed selection proxy for predicates", {
  skip_if_not_installed("dtplyr")
  registerS3method(
    "head",
    "margin_selection_proxy_counter",
    proxy_counter_head,
    envir = asNamespace("utils")
  )
  registerS3method(
    "collect",
    "margin_selection_proxy_counter",
    proxy_counter_collect,
    envir = asNamespace("dplyr")
  )

  source <- dtplyr::lazy_dt(data.frame(
    group = c("x", "y"),
    code = c(1L, 2L),
    value = c(10, 20)
  ))
  class(source) <- c("margin_selection_proxy_counter", class(source))
  selection_proxy_capture$n <- 0L

  numeric_query <- summarize_with_margins(
    source,
    n = dplyr::n(),
    .grouping = rollup(where(is.numeric)),
    .margin_label = NULL
  )

  expect_identical(selection_proxy_capture$n, 1L)
  expect_identical(
    names(dplyr::collect(numeric_query)),
    c("code", "value", "n")
  )

  selection_proxy_capture$n <- 0L
  character_query <- summarize_with_margins(
    source,
    n = dplyr::n(),
    .grouping = rollup(where(is.character)),
    .margin_label = NULL
  )
  expect_identical(selection_proxy_capture$n, 1L)
  expect_identical(
    names(dplyr::collect(character_query)),
    c("group", "n")
  )
})

test_that("DuckDB constructs one typed selection proxy for predicates", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  registerS3method(
    "head",
    "margin_selection_proxy_counter",
    proxy_counter_head,
    envir = asNamespace("utils")
  )
  registerS3method(
    "collect",
    "margin_selection_proxy_counter",
    proxy_counter_collect,
    envir = asNamespace("dplyr")
  )

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(
    con,
    data.frame(
      group = c("x", "y"),
      code = c(1L, 2L),
      value = c(10, 20)
    ),
    "selection_proxy_data",
    overwrite = TRUE,
    temporary = TRUE
  )
  class(source) <- c("margin_selection_proxy_counter", class(source))
  selection_proxy_capture$n <- 0L

  numeric_query <- summarize_with_margins(
    source,
    n = dplyr::n(),
    .grouping = rollup(where(is.numeric)),
    .margin_label = NULL
  )

  expect_identical(selection_proxy_capture$n, 1L)
  expect_identical(
    names(dplyr::collect(numeric_query)),
    c("code", "value", "n")
  )

  selection_proxy_capture$n <- 0L
  character_query <- summarize_with_margins(
    source,
    n = dplyr::n(),
    .grouping = rollup(where(is.character)),
    .margin_label = NULL
  )
  expect_identical(selection_proxy_capture$n, 1L)
  expect_identical(
    names(dplyr::collect(character_query)),
    c("group", "n")
  )
})

test_that("public Arrow table classes are supported", {
  skip_if_not_installed("arrow")

  data <- data.frame(group = c("x", "y"), value = 1:2)
  table <- arrow::Table$create(data)
  sources <- list(
    table,
    arrow::RecordBatch$create(data),
    arrow::InMemoryDataset$create(table)
  )

  for (source in sources) {
    result <- summarize_with_margins(
      source,
      n = dplyr::n(),
      .grouping = rollup(group)
    ) |>
      dplyr::collect()
    expect_setequal(result$n, c(1L, 1L, 2L))
  }

  reader <- arrow::RecordBatchReader$create(
    arrow::RecordBatch$create(data)
  )
  expect_error(
    summarize_with_margins(
      reader,
      n = dplyr::n(),
      .grouping = rollup(group)
    ),
    "RecordBatchReader"
  )
})

test_that("lazy backends check margin labels across all dimensions", {
  data <- data.frame(
    first = c("Total", "x"),
    second = c("y", "Total"),
    value = 1:2
  )

  skip_if_not_installed("dtplyr")
  expect_error(
    summarize_with_margins(
      dtplyr::lazy_dt(data),
      n = dplyr::n(),
      .grouping = rollup(first, second),
      .check_margin_label = TRUE
    ),
    "grouping columns `first`, `second`"
  )

  skip_if_not_installed("arrow")
  expect_error(
    summarize_with_margins(
      arrow::Table$create(data),
      n = dplyr::n(),
      .grouping = rollup(first, second),
      .check_margin_label = TRUE
    ),
    "grouping columns `first`, `second`"
  )

  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(
    con,
    data,
    "margin_label_checks",
    overwrite = TRUE,
    temporary = TRUE
  )
  expect_error(
    summarize_with_margins(
      remote,
      n = dplyr::n(),
      .grouping = rollup(first, second),
      .check_margin_label = TRUE
    ),
    "grouping columns `first`, `second`"
  )
})

test_that("lazy margin label checks aggregate portable numeric values", {
  registerS3method(
    "db_collect",
    "margin_check_connection",
    margin_check_collect,
    envir = asNamespace("dbplyr")
  )
  con <- dbplyr::simulate_oracle()
  class(con) <- c(
    "Oracle",
    "TestConnection",
    "margin_check_connection",
    "DBIConnection"
  )
  attr(con, "check_names") <- c("first", "second")
  remote <- dbplyr::tbl_lazy(
    data.frame(first = "x", second = "y", value = 1),
    con = con
  )
  class(remote) <- c("tbl_Oracle", "tbl_sql", "tbl_lazy", "tbl")

  expect_no_error(
    summarize_with_margins(
      remote,
      n = dplyr::n(),
      .grouping = rollup(first, second),
      .check_margin_label = TRUE
    )
  )

  expect_match(margin_check_capture$sql, "CASE WHEN", fixed = TRUE)
  expect_match(margin_check_capture$sql, "THEN 1", fixed = TRUE)
  expect_false(grepl(
    "MAX(\"first\" = 'Total')",
    margin_check_capture$sql,
    fixed = TRUE
  ))
})

test_that("documented SQL dialects use portable margin label checks", {
  registerS3method(
    "db_collect",
    "margin_check_connection",
    margin_check_collect,
    envir = asNamespace("dbplyr")
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
    "simulate_postgres",
    "simulate_redshift",
    "simulate_snowflake",
    "simulate_spark_sql",
    "simulate_sqlite",
    "simulate_teradata"
  )

  for (simulator in simulators) {
    con <- getExportedValue("dbplyr", simulator)()
    con_classes <- class(con)
    class(con) <- append(
      con_classes,
      "margin_check_connection",
      after = 1L
    )
    attr(con, "check_names") <- "group"
    remote <- dbplyr::tbl_lazy(
      data.frame(group = "x", value = 1),
      con = con
    )
    remote_classes <- class(remote)
    if (!"tbl_sql" %in% remote_classes) {
      class(remote) <- append(remote_classes, "tbl_sql", after = 1L)
    }

    margin_check_capture$sql <- NULL
    summarize_with_margins(
      remote,
      n = dplyr::n(),
      .grouping = rollup(group),
      .check_margin_label = TRUE
    )

    expect_match(
      margin_check_capture$sql,
      "SUM(CASE WHEN",
      fixed = TRUE,
      info = simulator
    )
    expect_match(
      margin_check_capture$sql,
      "THEN 1 ELSE 0 END)",
      fixed = TRUE,
      info = simulator
    )
  }
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

test_that("union adapters reserve dynamically injected summary names", {
  data <- data.frame(
    group = c("x", "x", "y"),
    value = 1:3
  )
  summary_name <- "..marginplyr_key_1"

  result <- summarize_with_margins(
    data,
    tibble::tibble(!!summary_name := sum(value)),
    .grouping = rollup(group)
  )

  expect_equal(
    dplyr::arrange(result, group),
    data.frame(
      group = c("Total", "x", "y"),
      check.names = FALSE,
      "..marginplyr_key_1" = c(6L, 3L, 3L)
    )
  )
})

test_that("union adapters diagnose opaque summary name collisions", {
  data <- data.frame(
    group = c("x", "x", "y"),
    value = 1:3
  )
  opaque_summary <- function(x) {
    stats::setNames(
      data.frame(sum(x)),
      "..marginplyr_key_1"
    )
  }

  error <- expect_error(
    summarize_with_margins(
      data,
      opaque_summary(value),
      .grouping = rollup(group)
    ),
    "summary output names conflict with internal grouping columns"
  )

  expect_s3_class(error, "marginplyr_error")
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "summarize_with_margins"
  )
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
    .grouping = rollup(group)
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
    .grouping = rollup(group)
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

test_that("union backends preserve margin values without implicit ordering", {
  data <- data.frame(
    group = c("b", "a", "b"),
    value = 1:3
  )
  expected <- c("Total", "a", "b")

  local <- summarize_with_margins(
    data,
    total = sum(value),
    .grouping = rollup(group)
  )
  expect_setequal(local$group, expected)

  skip_if_not_installed("dtplyr")
  dt_result <- summarize_with_margins(
    dtplyr::lazy_dt(data),
    total = sum(value),
    .grouping = rollup(group)
  ) |>
    dplyr::collect()
  expect_setequal(dt_result$group, expected)

  skip_if_not_installed("arrow")
  arrow_result <- summarize_with_margins(
    arrow::Table$create(data),
    total = sum(value),
    .grouping = rollup(group)
  ) |>
    dplyr::collect()
  expect_setequal(arrow_result$group, expected)
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
    .grouping = rollup(region)
  )
  expect_equal(dplyr::group_vars(dt_summary), character())
  expect_setequal(
    dplyr::collect(dt_summary)$value,
    c(1, 10, 11, 100, 1000, 1100)
  )

  dt_union <- expand_with_margins(
    grouped_dt,
    .grouping = rollup(region)
  )
  expect_equal(dplyr::group_vars(dt_union), character())

  dt_nest <- nest_with_margins(
    grouped_dt,
    .grouping = rollup(region)
  )
  expect_equal(dplyr::group_vars(dt_nest), character())
  expect_equal(names(dplyr::collect(dt_nest)), c("year", "region", "data"))

  dt_nest_by <- nest_by_with_margins(
    grouped_dt,
    .grouping = rollup(region)
  )
  expect_s3_class(dt_nest_by, "rowwise_df")
  expect_equal(dplyr::group_vars(dt_nest_by), c("year", "region"))

  skip_if_not_installed("arrow")
  grouped_arrow <- arrow::Table$create(data) |>
    dplyr::group_by(year)
  arrow_summary <- summarize_with_margins(
    grouped_arrow,
    value = sum(value),
    .grouping = rollup(region)
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
    .grouping = rollup(region)
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
    .grouping = rollup(region)
  )
  expect_equal(dplyr::group_vars(query), character())

  result <- dplyr::collect(query)
  result <- dplyr::arrange(result, year, region)
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

test_that("native SQL omits display flags when labels are disabled", {
  remote <- dbplyr::tbl_lazy(
    data.frame(a = "x", value = 1),
    con = dbplyr::simulate_postgres()
  )
  query <- summarize_with_margins(
    remote,
    n = dplyr::n(),
    bit = grouping_bit(a),
    id = grouping_id(a),
    .grouping = rollup(a),
    .margin_label = NULL
  )
  sql <- dbplyr::sql_render(query)

  expect_match(sql, "GROUPING(\"a\")", fixed = TRUE)
  expect_false(grepl("..marginplyr_grouping_", sql, fixed = TRUE))
  expect_identical(
    as.character(dplyr::tbl_vars(query)),
    c("a", "n", "bit", "id")
  )
  expect_identical(dplyr::group_vars(query), character())
})

test_that("native SQL reports incompatible dbplyr query representations", {
  registerS3method(
    "sql_build",
    "lazy_marginplyr_bad_query",
    bad_query_sql_build,
    envir = asNamespace("dbplyr")
  )
  bad_query <- structure(
    list(),
    class = c("lazy_marginplyr_bad_query", "lazy_query")
  )
  grouping_query <- dbplyr::lazy_query(
    "grouping_sets",
    x = bad_query,
    grouping_sets = list(character()),
    group_vars = character()
  )

  error <- expect_error(
    dbplyr::sql_build(
      grouping_query,
      con = dbplyr::simulate_postgres()
    ),
    "dbplyr query representation has changed"
  )

  # Not a Package condition: no call rewrite avoids it. See ADR 0015.
  expect_false(inherits(error, "marginplyr_error"))
})

test_that("native grouping sets remain a subquery after downstream verbs", {
  remote <- dbplyr::tbl_lazy(
    data.frame(a = "x", b = "u", value = 1),
    con = dbplyr::simulate_postgres()
  )
  query <- summarize_with_margins(
    remote,
    n = dplyr::n(),
    gid = grouping_id(a, b),
    .grouping = rollup(a, b),
    .margin_label = NULL
  )
  downstream <- list(
    select = dplyr::select(query, a, n, gid),
    mutate = dplyr::mutate(query, n_plus_one = n + 1),
    filter = dplyr::filter(query, n > 0),
    arrange = dplyr::arrange(query, a),
    summarize = dplyr::summarize(query, total = sum(n, na.rm = TRUE))
  )

  for (verb in names(downstream)) {
    sql <- dbplyr::sql_render(downstream[[verb]])
    expect_match(sql, "FROM (", fixed = TRUE, info = verb)
    expect_match(sql, "GROUP BY GROUPING SETS", fixed = TRUE, info = verb)
  }
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
    margin_labels = resolve_margin_labels(
      "Total",
      dimensions = plan$dimensions
    ),
    column_info = margin_column_info(
      grouping_selection_proxy(remote),
      plan$dimensions,
      backend = grouping_backend(remote)
    ),
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
    dplyr::mutate(n_plus_one = n + 1) |>
    dplyr::arrange(group)
  expect_s3_class(downstream, "tbl_lazy")
  expect_equal(nrow(dplyr::collect(downstream)), 4L)

  resummarized <- duplicated |>
    dplyr::summarise(total = sum(n)) |>
    dplyr::collect()
  expect_equal(resummarized$total, 4)
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
