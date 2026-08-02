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

  if (sqlite_simulation_available()) {
    sql <- summarize(dbplyr::tbl_lazy(
      data,
      con = dbplyr::simulate_sqlite()
    ))
    expect_s3_class(sql, "tbl_lazy")
    expect_identical(as.character(dplyr::tbl_vars(sql)), expected_names)
    expect_identical(dplyr::group_vars(sql), character())
    expect_no_error(dbplyr::sql_render(sql))
  }

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

test_that("dtplyr validates Parent-share source types during collection", {
  skip_if_not_installed("dtplyr")
  data <- data.frame(
    group = c("x", "y"),
    value = 1:2
  )

  query <- summarize_with_margins(
    dtplyr::lazy_dt(data),
    flag = any(value > 0),
    flag_share = share_of_parent(flag),
    .grouping = rollup(group),
    .margin_label = NULL
  )

  expect_s3_class(query, "dtplyr_step")
  error <- expect_error(
    dplyr::collect(query),
    "plain integer or double scalar"
  )
  expect_s3_class(error, "marginplyr_error")
  expect_identical(error$parent_output, "flag_share")
  expect_identical(error$source_summary, "flag")
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "summarize_with_margins"
  )
})

test_that("dtplyr rejects every ineligible Parent-share source type", {
  skip_if_not_installed("dtplyr")
  data <- data.frame(
    group = c("x", "y"),
    value = 1:2,
    stamp = as.POSIXct("2020-01-01", tz = "UTC") + 1:2
  )
  expressions <- list(
    logical = rlang::expr(any(value > 0)),
    date_time = rlang::expr(min(stamp)),
    duration = rlang::expr(difftime(max(stamp), min(stamp), units = "secs")),
    character = rlang::expr(paste(value, collapse = "")),
    factor = rlang::expr(factor("level")),
    list = rlang::expr(list(sum(value)))
  )

  for (semantic_type in names(expressions)) {
    query <- rlang::inject(summarize_with_margins(
      dtplyr::lazy_dt(data),
      source = !!expressions[[semantic_type]],
      share = share_of_parent(source),
      .grouping = rollup(group),
      .margin_label = NULL
    ))

    expect_s3_class(query, "dtplyr_step")
    error <- expect_error(
      dplyr::collect(query),
      "plain integer or double scalar",
      info = semantic_type
    )
    expect_s3_class(error, "marginplyr_error")
    expect_identical(error$parent_output, "share", info = semantic_type)
    expect_identical(error$source_summary, "source", info = semantic_type)
    expect_identical(
      rlang::call_name(conditionCall(error)),
      "summarize_with_margins",
      info = semantic_type
    )
  }
})

test_that("dtplyr rejects non-scalar Parent-share sources on collection", {
  skip_if_not_installed("dtplyr")
  data <- data.frame(
    group = c("x", "x", "y"),
    value = 1:3
  )
  expressions <- list(
    zero = rlang::expr(numeric()),
    multiple = rlang::expr(range(value))
  )

  for (cardinality in names(expressions)) {
    query <- rlang::inject(summarize_with_margins(
      dtplyr::lazy_dt(data),
      source = !!expressions[[cardinality]],
      share = share_of_parent(source),
      .grouping = rollup(group),
      .margin_label = NULL
    ))

    expect_s3_class(query, "dtplyr_step")
    error <- expect_error(
      dplyr::collect(query),
      "exactly one value per grouping row",
      info = cardinality
    )
    expect_s3_class(
      error,
      "marginplyr_parent_cardinality_error"
    )
    expect_s3_class(error, "marginplyr_error")
    expect_identical(error$parent_output, "share", info = cardinality)
    expect_identical(error$source_summary, "source", info = cardinality)
    expect_identical(
      rlang::call_name(conditionCall(error)),
      "summarize_with_margins",
      info = cardinality
    )
  }
})

test_that("dtplyr integer and double Parent shares match local results", {
  skip_if_not_installed("dtplyr")
  data <- data.frame(
    group = c("x", "x", "y"),
    integer_value = 1:3,
    double_value = c(0.5, 1.5, 4)
  )
  summarize <- function(source) {
    summarize_with_margins(
      source,
      integer_total = sum(integer_value),
      double_mean = mean(double_value),
      integer_share = share_of_parent(integer_total),
      double_share = share_of_parent(double_mean),
      .grouping = rollup(group),
      .margin_label = NULL
    ) |>
      dplyr::arrange(group)
  }

  expected <- summarize(data)
  query <- summarize(dtplyr::lazy_dt(data))
  expect_s3_class(query, "dtplyr_step")
  result <- dplyr::collect(query)

  expect_equal(as.data.frame(result), as.data.frame(expected))
  expect_type(result$integer_total, "integer")
  expect_type(result$double_mean, "double")
  expect_type(result$integer_share, "double")
  expect_type(result$double_share, "double")
})

test_that("dtplyr validates each referenced source expanded by across", {
  skip_if_not_installed("dtplyr")
  data <- data.frame(
    group = c("x", "x", "y"),
    value = 1:3
  )

  query <- summarize_with_margins(
    dtplyr::lazy_dt(data),
    dplyr::across(
      value,
      list(total = sum, flag = ~any(.x > 0))
    ),
    total_share = share_of_parent(value_total),
    flag_share = share_of_parent(value_flag),
    .grouping = rollup(group),
    .margin_label = NULL
  )

  expect_s3_class(query, "dtplyr_step")
  error <- expect_error(
    dplyr::collect(query),
    "plain integer or double scalar"
  )
  expect_s3_class(error, "marginplyr_error")
  expect_identical(error$parent_output, "flag_share")
  expect_identical(error$source_summary, "value_flag")
})

test_that("dtplyr validates constant summaries expanded by across", {
  skip_if_not_installed("dtplyr")
  data <- data.frame(
    group = c("x", "y"),
    value = 1:2
  )
  expressions <- list(
    factor = rlang::expr(~factor("level")),
    list = rlang::expr(~list(1)),
    multiple = rlang::expr(~c(1, 2))
  )

  for (kind in names(expressions)) {
    query <- rlang::inject(summarize_with_margins(
      dtplyr::lazy_dt(data),
      dplyr::across(value, !!expressions[[kind]], .names = "source"),
      share = share_of_parent(source),
      .grouping = rollup(group),
      .margin_label = NULL
    ))

    expect_s3_class(query, "dtplyr_step")
    error <- expect_error(dplyr::collect(query))
    expect_s3_class(error, "marginplyr_error")
    expect_identical(error$parent_output, "share", info = kind)
    expect_identical(error$source_summary, "source", info = kind)
    if (identical(kind, "multiple")) {
      expect_s3_class(error, "marginplyr_parent_cardinality_error")
    } else {
      expect_match(conditionMessage(error), "plain integer or double scalar")
    }
  }
})

test_that("dtplyr Parent validation preserves ordinary across arguments", {
  skip_if_not_installed("dtplyr")
  data <- data.frame(
    group = c("x", "x", "y"),
    value = c(1, NA, 3)
  )
  summarize <- function(source) {
    summarize_with_margins(
      source,
      dplyr::across(value, sum, na.rm = TRUE, .names = "total"),
      share = share_of_parent(total),
      .grouping = rollup(group),
      .margin_label = NULL
    ) |>
      dplyr::arrange(group)
  }

  expected <- suppressWarnings(summarize(data))
  query <- summarize(dtplyr::lazy_dt(data))
  expect_s3_class(query, "dtplyr_step")
  expect_equal(
    as.data.frame(dplyr::collect(query)),
    as.data.frame(expected)
  )
})

test_that("dtplyr preserves across arguments for unreferenced functions", {
  skip_if_not_installed("dtplyr")
  data <- data.frame(
    group = c("x", "x", "y"),
    value = c(1, NA, 3)
  )
  summarize <- function(source) {
    summarize_with_margins(
      source,
      dplyr::across(
        value,
        list(total = sum, average = mean),
        na.rm = TRUE
      ),
      share = share_of_parent(value_total),
      .grouping = rollup(group),
      .margin_label = NULL
    ) |>
      dplyr::arrange(group)
  }

  expected <- suppressWarnings(summarize(data))
  query <- summarize(dtplyr::lazy_dt(data))
  expect_s3_class(query, "dtplyr_step")
  expect_equal(
    as.data.frame(dplyr::collect(query)),
    as.data.frame(expected)
  )
})

test_that("Arrow rejects Parent shares before constructing a query", {
  skip_if_not_installed("arrow")
  source <- arrow::Table$create(data.frame(
    group = c("x", "y"),
    value = 1:2
  ))
  calls <- new.env(parent = emptyenv())
  calls$schema <- 0L
  calls$summarize <- 0L
  calls$collect <- 0L
  infer_schema <- getFromNamespace("infer_schema", "arrow")
  testthat::local_mocked_bindings(
    infer_schema = function(x) {
      calls$schema <- calls$schema + 1L
      infer_schema(x)
    },
    do_arrow_summarize = function(...) {
      calls$summarize <- calls$summarize + 1L
      stop("Arrow summary query was constructed.", call. = FALSE)
    },
    collect.arrow_dplyr_query = function(...) {
      calls$collect <- calls$collect + 1L
      stop("Arrow query was collected.", call. = FALSE)
    },
    collect.ArrowTabular = function(...) {
      calls$collect <- calls$collect + 1L
      stop("Arrow data was collected.", call. = FALSE)
    },
    .package = "arrow"
  )

  error <- expect_error(
    summarize_with_margins(
      source,
      total = sum(value),
      share = share_of_parent(total),
      .grouping = rollup(group),
      .margin_label = NULL
    ),
    "Arrow.*Parent share"
  )

  expect_s3_class(error, "marginplyr_error")
  expect_match(
    conditionMessage(error),
    "Other Arrow Margin operations remain supported",
    fixed = TRUE
  )
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "summarize_with_margins"
  )
  expect_identical(calls$schema, 1L)
  expect_identical(calls$summarize, 0L)
  expect_identical(calls$collect, 0L)
  expect_snapshot(conditionMessage(error))
})

test_that("Arrow ordinary Margin summaries remain lazy and available", {
  skip_if_not_installed("arrow")
  query <- summarize_with_margins(
    arrow::Table$create(data.frame(
      group = c("x", "x", "y"),
      value = 1:3
    )),
    total = sum(value),
    .grouping = rollup(group),
    .margin_label = NULL
  )

  expect_s3_class(query, "arrow_dplyr_query")
  result <- dplyr::collect(query)
  expect_identical(names(result), c("group", "total"))
  expect_setequal(result$total, c(3L, 3L, 6L))
  expect_true(anyNA(result$group))
})

test_that("Arrow Parent-share planning errors precede backend rejection", {
  skip_if_not_installed("arrow")
  source <- arrow::Table$create(data.frame(
    group = c("x", "y"),
    value = 1:2
  ))
  cases <- list(
    grammar = list(
      call = rlang::expr(summarize_with_margins(
        source,
        total = sum(value),
        share = share_of_parent(sum(value)),
        .grouping = rollup(group),
        .margin_label = NULL
      )),
      pattern = "requires exactly one bare name"
    ),
    source_name = list(
      call = rlang::expr(summarize_with_margins(
        source,
        share = share_of_parent(total),
        total = sum(value),
        .grouping = rollup(group),
        .margin_label = NULL
      )),
      pattern = "forward reference"
    ),
    dependency = list(
      call = rlang::expr(summarize_with_margins(
        source,
        gross = sum(value),
        net = gross,
        share = share_of_parent(net),
        .grouping = rollup(group),
        .margin_label = NULL
      )),
      pattern = "depends on earlier summary alias"
    ),
    naming = list(
      call = rlang::expr(summarize_with_margins(
        source,
        total = sum(value),
        share = share_of_parent(total),
        .grouping = rollup(group),
        .margin_label = NULL,
        .id = "share"
      )),
      pattern = "output name.*conflicts"
    ),
    grouping_plan = list(
      call = rlang::expr(summarize_with_margins(
        source,
        total = sum(value),
        share = share_of_parent(total),
        .grouping = grouping_sets(group),
        .margin_label = NULL
      )),
      pattern = "requires.*one pure.*rollup"
    )
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    error <- expect_error(
      rlang::eval_tidy(case$call),
      case$pattern,
      info = case_name
    )
    expect_false(
      grepl("Arrow backends do not support", conditionMessage(error)),
      info = case_name
    )
  }
})

test_that("dtplyr batches Parent shares with missing-safe matching", {
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

parent_sql_count <- function(sql, pattern) {
  lengths(gregexpr(pattern, sql, fixed = TRUE))
}

test_that("dtplyr batches validated summaries and parent mapping", {
  skip_if_not_installed("dtplyr")
  data <- data.frame(
    group = c("x", "y"),
    revenue = c(1, 3),
    units = 1:2
  )
  one <- summarize_with_margins(
    dtplyr::lazy_dt(data),
    revenue = sum(revenue),
    revenue_share = share_of_parent(revenue),
    .grouping = rollup(group),
    .margin_label = NULL
  )
  many <- summarize_with_margins(
    dtplyr::lazy_dt(data),
    revenue = sum(revenue),
    units = sum(units),
    revenue_share = share_of_parent(revenue),
    units_share = share_of_parent(units),
    .grouping = rollup(group),
    .margin_label = NULL
  )
  one_call <- paste(capture.output(dplyr::show_query(one)), collapse = "\n")
  many_call <- paste(capture.output(dplyr::show_query(many)), collapse = "\n")

  expect_identical(
    parent_sql_count(
      many_call,
      "check_parent_scalar(sum(revenue)"
    ),
    parent_sql_count(
      one_call,
      "check_parent_scalar(sum(revenue)"
    )
  )
  expect_identical(parent_sql_count(many_call, "allow.cartesian = TRUE"), 1L)
})

parent_lazy_probe_capture <- new.env(parent = emptyenv())

parent_lazy_probe_collect <- function(con, sql, ...) {
  parent_lazy_probe_capture$collection <-
    parent_lazy_probe_capture$collection + 1L
  stop("Parent-share planning must not execute a schema probe.", call. = FALSE)
}

parent_lazy_probe_fields <- function(con, sql, ...) {
  parent_lazy_probe_capture$result_type <-
    parent_lazy_probe_capture$result_type + 1L
  stop("Parent-share planning must not query result fields.", call. = FALSE)
}

parent_lazy_probe_rows <- function(con, sql, ...) {
  parent_lazy_probe_capture$cardinality <-
    parent_lazy_probe_capture$cardinality + 1L
  stop("Parent-share planning must not query result rows.", call. = FALSE)
}

new_parent_lazy_probe <- function(data) {
  methods <- list(
    db_collect = parent_lazy_probe_collect,
    sql_query_fields = parent_lazy_probe_fields,
    sql_query_rows = parent_lazy_probe_rows
  )
  for (generic in names(methods)) {
    registerS3method(
      generic,
      "parent_lazy_probe_connection",
      methods[[generic]],
      envir = asNamespace("dbplyr")
    )
  }
  con <- dbplyr::simulate_dbi()
  class(con) <- c("parent_lazy_probe_connection", class(con))
  parent_lazy_probe_capture$result_type <- 0L
  parent_lazy_probe_capture$cardinality <- 0L
  parent_lazy_probe_capture$collection <- 0L
  dbplyr::tbl_lazy(data, con = con)
}

parent_lazy_probe_counts <- function() {
  c(
    result_type = parent_lazy_probe_capture$result_type,
    cardinality = parent_lazy_probe_capture$cardinality,
    collection = parent_lazy_probe_capture$collection
  )
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
  remote <- new_parent_lazy_probe(
    data.frame(group = "x", label = "value")
  )

  query <- summarize_with_margins(
    remote,
    label = min(label),
    share = share_of_parent(label),
    .grouping = rollup(group),
    .margin_label = NULL
  )
  expect_s3_class(query, "tbl_lazy")
  expect_identical(parent_lazy_probe_counts(), c(
    result_type = 0L,
    cardinality = 0L,
    collection = 0L
  ))

  sql <- dbplyr::sql_render(query)
  expect_identical(parent_lazy_probe_counts(), c(
    result_type = 0L,
    cardinality = 0L,
    collection = 0L
  ))
  expect_match(sql, "CAST(", fixed = TRUE)
  expect_match(sql, "LEFT JOIN", fixed = TRUE)
})

test_that("general dbplyr reports static Parent-share errors without probing", {
  remote <- new_parent_lazy_probe(
    data.frame(group = "x", value = 1)
  )
  cases <- list(
    syntax = rlang::expr(summarize_with_margins(
      remote,
      total = sum(value),
      share = share_of_parent(sum(value)),
      .grouping = rollup(group)
    )),
    source_name = rlang::expr(summarize_with_margins(
      remote,
      share = share_of_parent(total),
      total = sum(value),
      .grouping = rollup(group)
    )),
    dependency = rlang::expr(summarize_with_margins(
      remote,
      gross = sum(value),
      net = gross,
      share = share_of_parent(net),
      .grouping = rollup(group)
    )),
    output_name = rlang::expr(summarize_with_margins(
      remote,
      total = sum(value),
      share = share_of_parent(total),
      .grouping = rollup(group),
      .id = "share"
    )),
    grouping_plan = rlang::expr(summarize_with_margins(
      remote,
      total = sum(value),
      share = share_of_parent(total),
      .grouping = cube(group)
    ))
  )

  for (case_name in names(cases)) {
    error <- expect_error(
      rlang::eval_tidy(cases[[case_name]]),
      info = case_name
    )
    expect_true(inherits(error, "marginplyr_error"), info = case_name)
    expect_identical(
      parent_lazy_probe_counts(),
      c(result_type = 0L, cardinality = 0L, collection = 0L),
      info = case_name
    )
  }
})

test_that("fallback simulators render portable staged Parent-share SQL", {
  data <- data.frame(
    fixed = NA_character_,
    group = NA_character_,
    value = 1
  )
  simulators <- available_simulators(c(
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
  ))

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

test_that("RSQLite executes portable Parent shares end to end", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(
    fixed = c(NA_character_, NA_character_, "a", "a"),
    group = c(NA_character_, "x", NA_character_, "x"),
    revenue = c(1, 3, 2, 2),
    units = c(1L, 3L, 0L, 0L),
    unclamped = c(-1, 2, -2, 3)
  )
  remote <- dplyr::copy_to(
    con,
    data,
    "parent_share_sqlite_data",
    overwrite = TRUE,
    temporary = TRUE
  )

  summarize <- function(source) {
    summarize_with_margins(
      source,
      level = grouping_id(group),
      revenue_total = sum(revenue),
      units_total = sum(units),
      unclamped_total = sum(unclamped),
      missing_parent = dplyr::if_else(
        dplyr::n() > 1L,
        NA_real_,
        sum(revenue)
      ),
      revenue_share = share_of_parent(revenue_total),
      units_share = share_of_parent(units_total),
      unclamped_share = share_of_parent(unclamped_total),
      missing_parent_share = share_of_parent(missing_parent),
      .by = fixed,
      .grouping = rollup(group),
      .id = "set",
      .margin_label = "Margin"
    )
  }
  arrange_result <- function(result) {
    dplyr::arrange(
      result,
      is.na(fixed),
      fixed,
      set,
      is.na(group),
      group
    )
  }

  expected <- arrange_result(summarize(data))
  query <- summarize(remote)
  sql <- dbplyr::sql_render(query)

  expect_s3_class(query, "tbl_lazy")
  expect_match(sql, "UNION ALL", fixed = TRUE)
  expect_match(sql, "IS NULL AND", fixed = TRUE)
  expect_match(sql, "CAST(", fixed = TRUE)
  expect_false(grepl("GROUPING SETS", sql, fixed = TRUE))

  result <- arrange_result(dplyr::collect(query))
  expect_equal(as.data.frame(result), as.data.frame(expected))
  expect_identical(result$set, expected$set)
  expect_type(result$revenue_share, "double")
  expect_type(result$units_share, "double")
  expect_type(result$unclamped_share, "double")
  expect_type(result$missing_parent_share, "double")
  expect_true(all(result$revenue_share[result$level == 1L] == 1))
  expect_true(all(is.na(
    result$units_share[result$fixed == "a" & result$level == 0L]
  )))
  expect_true(all(is.na(
    result$missing_parent_share[result$level == 0L]
  )))
  expect_identical(
    sort(result$unclamped_share[result$level == 0L]),
    c(-2, -1, 2, 3)
  )
})

test_that("RSQLite Parent shares preserve runtime backend conditions", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- dplyr::copy_to(
    con,
    data.frame(group = c("x", "y"), value = 1:2),
    "parent_share_runtime_error_data",
    overwrite = TRUE,
    temporary = TRUE
  )

  baseline <- expect_error(
    remote |>
      dplyr::summarize(bad = no_such_function(value)) |>
      dplyr::collect()
  )
  query <- summarize_with_margins(
    remote,
    bad = no_such_function(value),
    share = share_of_parent(bad),
    .grouping = rollup(group),
    .margin_label = NULL
  )

  expect_s3_class(query, "tbl_lazy")
  error <- expect_error(dplyr::collect(query))
  expect_identical(class(error), class(baseline))
  expect_identical(class(error$parent), class(baseline$parent))
  expect_false(inherits(error, "marginplyr_error"))
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
    missing_source = c(1, 3, 2, 2),
    unclamped = c(-1, 2, -2, 3)
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
      unclamped = sum(unclamped),
      missing_parent = dplyr::if_else(
        dplyr::n() > 1L,
        NA_real_,
        sum(missing_source)
      ),
      revenue_share = share_of_parent(revenue),
      unclamped_share = share_of_parent(unclamped),
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
  expect_identical(
    sort(expected$unclamped_share[expected$level == 1L]),
    c(-2, -1, 2, 3)
  )
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
  if (sqlite_simulation_available()) {
    simulated <- summarize(dbplyr::tbl_lazy(
      data,
      con = dbplyr::simulate_sqlite()
    ))
    expect_identical(
      as.character(dplyr::tbl_vars(simulated)),
      names(expected)
    )
    expect_no_error(dbplyr::sql_render(simulated))
  }

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
