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

  if (backend_available("dtplyr")) {
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
  skip_if_backend_absent("dtplyr")
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
  expect_identical(error$share_output, "flag_share")
  expect_identical(error$source_summary, "flag")
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "summarize_with_margins"
  )
})

test_that("dtplyr rejects every ineligible Parent-share source type", {
  skip_if_backend_absent("dtplyr")
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
    expect_identical(error$share_output, "share", info = semantic_type)
    expect_identical(error$source_summary, "source", info = semantic_type)
    expect_identical(
      rlang::call_name(conditionCall(error)),
      "summarize_with_margins",
      info = semantic_type
    )
  }
})

test_that("dtplyr rejects non-scalar Parent-share sources on collection", {
  skip_if_backend_absent("dtplyr")
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
      "marginplyr_share_cardinality_error"
    )
    expect_s3_class(error, "marginplyr_error")
    expect_identical(error$share_output, "share", info = cardinality)
    expect_identical(error$source_summary, "source", info = cardinality)
    expect_identical(
      rlang::call_name(conditionCall(error)),
      "summarize_with_margins",
      info = cardinality
    )
  }
})

test_that("dtplyr integer and double Parent shares match local results", {
  skip_if_backend_absent("dtplyr")
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
  skip_if_backend_absent("dtplyr")
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
  expect_identical(error$share_output, "flag_share")
  expect_identical(error$source_summary, "value_flag")
})

test_that("dtplyr validates constant summaries expanded by across", {
  skip_if_backend_absent("dtplyr")
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
    expect_identical(error$share_output, "share", info = kind)
    expect_identical(error$source_summary, "source", info = kind)
    if (identical(kind, "multiple")) {
      expect_s3_class(error, "marginplyr_share_cardinality_error")
    } else {
      expect_match(conditionMessage(error), "plain integer or double scalar")
    }
  }
})

test_that("dtplyr Parent validation preserves ordinary across arguments", {
  skip_if_backend_absent("dtplyr")
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
  skip_if_backend_absent("dtplyr")
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

test_that("dtplyr preserves an omitted across selection", {
  skip_if_backend_absent("dtplyr")
  # dtplyr takes the share `across()` apart and rebuilds it a second time, to
  # wrap each function in the validation the lazy backend needs, so an argument
  # the caller omitted has one more rebuild to survive (#174). Compared against
  # the local result, which needs no optional backend and cannot agree with a
  # wrong rebuild by making the same mistake.
  data <- data.frame(
    group = c("x", "x", "y"),
    value = c(1, 3, 6),
    other = c(2, 4, 8)
  )
  summarize <- function(source) {
    summarize_with_margins(
      source,
      total = sum(value),
      rest = sum(other),
      dplyr::across(, share_of_parent, .names = "{.col}_share"),
      .grouping = rollup(group),
      .margin_label = NULL
    ) |>
      dplyr::arrange(group)
  }

  expected <- summarize(data)
  query <- summarize(dtplyr::lazy_dt(data))
  expect_s3_class(query, "dtplyr_step")
  expect_equal(
    as.data.frame(dplyr::collect(query)),
    as.data.frame(expected)
  )
  # The omission selected both eligible summaries, as `everything()` does.
  expect_named(
    expected,
    c("group", "total", "rest", "total_share", "rest_share")
  )
})

test_that("Arrow rejects Parent shares before constructing a query", {
  skip_if_backend_absent("arrow")
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
  skip_if_backend_absent("arrow")
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
  skip_if_backend_absent("arrow")
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
  skip_if_backend_absent("dtplyr")
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
  skip_if_backend_absent("dtplyr")
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
      "check_share_scalar(sum(revenue)"
    ),
    parent_sql_count(
      one_call,
      "check_share_scalar(sum(revenue)"
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

test_that("PostgreSQL renders one staged Parent-share join for all measures", {
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

# This connection cannot collect at all, so it stands for a backend the type
# sample learns nothing from: the read fails, the source goes unsampled, and
# the staged query is returned for the caller to execute. The counters are
# what hold the rest of the contract — nothing asks this connection for the
# staged query's fields, its row count, or its results.
test_that("general dbplyr leaves a source it cannot sample to execution", {
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
  skip_if_backend_absent("RSQLite", "DBI")
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
  skip_if_backend_absent("RSQLite", "DBI")
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

# The reproduction from #106. A weakly typed dialect used to answer it with an
# all-missing share column plus the grand total's own-denominator `1`, because
# the eligible-type rule was reached only from the local adapter. Comparing the
# condition against the local one is what makes "the same rejection" checkable
# without a second backend in this test.
test_that("RSQLite rejects an ineligible share source like the local backend", {
  skip_if_backend_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(
    region = c("E", "E", "W"),
    store = c("s1", "s2", "s3"),
    revenue = c(1, 3, 2)
  )
  remote <- dplyr::copy_to(
    con,
    data,
    "share_source_type_sqlite_data",
    overwrite = TRUE,
    temporary = TRUE
  )
  summarize <- function(source) {
    summarize_with_margins(
      source,
      lab = max(region),
      p = share_of_parent(lab),
      .grouping = rollup(region, store)
    )
  }

  local_error <- expect_error(summarize(data), "plain integer or double scalar")
  remote_error <- expect_error(
    summarize(remote),
    "plain integer or double scalar"
  )

  expect_s3_class(remote_error, "marginplyr_error")
  expect_identical(
    conditionMessage(remote_error),
    conditionMessage(local_error)
  )
  expect_identical(remote_error$share_output, "p")
  expect_identical(remote_error$source_summary, "lab")
  expect_identical(
    rlang::call_name(conditionCall(remote_error)),
    "summarize_with_margins"
  )
  # The internal denominator and match columns are marginplyr's own names for
  # temporaries the caller never wrote and cannot act on.
  expect_false(grepl(
    "..marginplyr",
    conditionMessage(remote_error),
    fixed = TRUE
  ))
})

test_that("RSQLite rejects an ineligible source beside a Margin level", {
  skip_if_backend_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- dplyr::copy_to(
    con,
    data.frame(
      region = c("E", "E", "W"),
      store = c("s1", "s2", "s3"),
      revenue = c(1, 3, 2)
    ),
    "share_source_level_sqlite_data",
    overwrite = TRUE,
    temporary = TRUE
  )

  # `grouping_bit()` and `grouping_id()` are marginplyr's own summary-context
  # helpers, so the backend has no such functions and the read that types the
  # source summaries would fail on them as a whole. A call that identifies its
  # Margin levels must not lose the rule for its measures.
  error <- expect_error(
    summarize_with_margins(
      remote,
      level = grouping_id(region, store),
      bit = grouping_bit(store),
      lab = max(region),
      p = share_of_parent(lab),
      .grouping = rollup(region, store)
    ),
    "plain integer or double scalar"
  )
  expect_s3_class(error, "marginplyr_error")
  expect_identical(error$source_summary, "lab")
})

test_that("RSQLite types a share source past an unrelated failing summary", {
  skip_if_backend_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- dplyr::copy_to(
    con,
    data.frame(
      region = c("E", "E", "W"),
      store = c("s1", "s2", "s3"),
      revenue = c(1, 3, 2)
    ),
    "share_source_scope_sqlite_data",
    overwrite = TRUE,
    temporary = TRUE
  )

  # A summary no share reads must not decide whether the rule runs. Reading it
  # too would fail the whole read on this expression the backend refuses, and
  # the source it was never asked about would go unchecked with it.
  error <- expect_error(
    summarize_with_margins(
      remote,
      unrelated = no_such_aggregate(revenue),
      lab = max(region),
      p = share_of_parent(lab),
      .grouping = rollup(region, store)
    ),
    "plain integer or double scalar"
  )
  expect_s3_class(error, "marginplyr_error")
  expect_identical(error$source_summary, "lab")
})

test_that("RSQLite keeps eligible share sources working after the probe", {
  skip_if_backend_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(
    group = c("x", "x", "y"),
    revenue = c(1, 3, 2),
    units = c(1L, 3L, 0L),
    # The first input row is missing, so a weakly typed dialect answers the
    # type probe with a value carrying no type at all. That is not evidence of
    # an ineligible source, and rejecting on it would break a working call.
    sparse = c(NA_real_, 4, 5)
  )
  remote <- dplyr::copy_to(
    con,
    data,
    "share_source_probe_sqlite_data",
    overwrite = TRUE,
    temporary = TRUE
  )
  summarize <- function(source) {
    summarize_with_margins(
      source,
      revenue_total = sum(revenue),
      units_total = sum(units),
      sparse_total = sum(sparse, na.rm = TRUE),
      revenue_share = share_of_parent(revenue_total),
      units_share = share_of_total(units_total),
      sparse_share = share_of_parent(sparse_total),
      .grouping = rollup(group),
      .margin_label = NULL
    ) |>
      dplyr::arrange(is.na(group), group)
  }

  expected <- summarize(data)
  query <- summarize(remote)

  expect_s3_class(query, "tbl_lazy")
  expect_equal(
    as.data.frame(dplyr::collect(query)),
    as.data.frame(expected)
  )
})

test_that("a lazy backend that answers nothing keeps its share source", {
  skip_if_no_sqlite_simulation()
  # A simulated connection executes no query, so the type probe learns nothing
  # and the call must stay lazy rather than reject a source it cannot read.
  remote <- dbplyr::tbl_lazy(
    data.frame(group = c("x", "y"), value = 1:2),
    con = dbplyr::simulate_sqlite()
  )

  query <- summarize_with_margins(
    remote,
    total = sum(value),
    share = share_of_parent(total),
    .grouping = rollup(group),
    .margin_label = NULL
  )

  expect_s3_class(query, "tbl_lazy")
  expect_match(dbplyr::sql_render(query), "UNION ALL", fixed = TRUE)
})

test_that("DuckDB rejects an ineligible share source like the local backend", {
  skip_if_backend_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(
    region = c("E", "E", "W"),
    store = c("s1", "s2", "s3"),
    revenue = c(1, 3, 2)
  )
  remote <- dplyr::copy_to(
    con,
    data,
    "share_source_type_duckdb_data",
    overwrite = TRUE,
    temporary = TRUE
  )
  summarize <- function(source) {
    summarize_with_margins(
      source,
      lab = max(region),
      p = share_of_total(lab),
      .grouping = rollup(region, store)
    )
  }

  local_error <- expect_error(summarize(data), "plain integer or double scalar")
  remote_error <- expect_error(
    summarize(remote),
    "plain integer or double scalar"
  )

  expect_s3_class(remote_error, "marginplyr_error")
  expect_identical(
    conditionMessage(remote_error),
    conditionMessage(local_error)
  )
  expect_identical(remote_error$share_output, "p")
  expect_identical(remote_error$source_summary, "lab")
  # The backend used to raise its own error here, naming the internal
  # denominator column the join reserved.
  expect_false(grepl(
    "..marginplyr",
    conditionMessage(remote_error),
    fixed = TRUE
  ))
})

test_that("DuckDB Parent shares agree across native, portable, local paths", {
  skip_if_backend_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
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

empty_share_data <- function() {
  data.frame(group = character(), value = double())
}

# The identities an empty input still owes, whichever backend produced it. Given
# collected results rather than a source, so the summarize calls stay inside
# their own `test_that()` block where `codetools` does not follow them and the
# bare dimension names need no `all_of()` or `# nolint`.
expect_empty_share_identities <- function(root, partitioned) {
  expect_identical(root$share, 1)
  expect_identical(root$whole, 1)
  expect_identical(nrow(partitioned), 0L)
  expect_type(partitioned$share, "double")
  # A Total share joins its denominator on the fixed keys, so the empty
  # partitioned case is the one where that join has nothing on either side.
  expect_type(partitioned$whole, "double")
}

test_that("dtplyr shares preserve empty-input grand total and partitions", {
  skip_if_backend_absent("dtplyr")
  source <- dtplyr::lazy_dt(empty_share_data())

  root <- summarize_with_margins(
    source,
    total = sum(value),
    share = share_of_parent(total),
    whole = share_of_total(total),
    .grouping = rollup(group),
    .margin_label = NULL
  ) |>
    dplyr::collect()
  partitioned <- summarize_with_margins(
    source,
    total = sum(value),
    share = share_of_parent(total),
    whole = share_of_total(total),
    .by = group,
    .grouping = rollup(value),
    .margin_label = NULL
  ) |>
    dplyr::collect()

  expect_empty_share_identities(root, partitioned)
})

test_that("DuckDB shares preserve empty-input grand total and partitions", {
  skip_if_backend_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(
    con,
    empty_share_data(),
    "empty_share_data",
    overwrite = TRUE,
    temporary = TRUE
  )

  root <- summarize_with_margins(
    source,
    total = sum(value),
    share = share_of_parent(total),
    whole = share_of_total(total),
    .grouping = rollup(group),
    .margin_label = NULL
  ) |>
    dplyr::collect()
  partitioned <- summarize_with_margins(
    source,
    total = sum(value),
    share = share_of_parent(total),
    whole = share_of_total(total),
    .by = group,
    .grouping = rollup(value),
    .margin_label = NULL
  ) |>
    dplyr::collect()

  expect_empty_share_identities(root, partitioned)
})

duplicate_share_data <- function() {
  data.frame(group = c("x", "y"), value = c(1, 3))
}

test_that("dtplyr Parent shares skip duplicate grouping-set occurrences", {
  skip_if_backend_absent("dtplyr")
  data <- duplicate_share_data()
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
  source <- dtplyr::lazy_dt(data)

  # Against the local result for the same specification, so the backend cannot
  # pass by being self-consistently wrong.
  expect_equal(
    as.data.frame(dplyr::collect(summarize(source, include_id = TRUE))),
    as.data.frame(summarize(data, include_id = TRUE))
  )
  expect_equal(
    as.data.frame(dplyr::collect(summarize(source, include_id = FALSE))),
    as.data.frame(summarize(data, include_id = FALSE))
  )
})

test_that("DuckDB Parent shares skip duplicate grouping-set occurrences", {
  skip_if_backend_absent("duckdb", "DBI")
  data <- duplicate_share_data()
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
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(
    con,
    data,
    "duplicate_parent_share_data",
    overwrite = TRUE,
    temporary = TRUE
  )

  expect_equal(
    as.data.frame(dplyr::collect(summarize(source, include_id = TRUE))),
    as.data.frame(summarize(data, include_id = TRUE))
  )
  without_id <- summarize(source, include_id = FALSE)
  # Duplicate occurrences cost DuckDB its native plan, which is the half a
  # collected result cannot show.
  expect_match(dbplyr::sql_render(without_id), "UNION ALL", fixed = TRUE)
  expect_equal(
    as.data.frame(dplyr::collect(without_id)),
    as.data.frame(summarize(data, include_id = FALSE))
  )
})

test_that("lazy Parent-share staging avoids adversarial user-name collisions", {
  group_name <- "..marginplyr_parent_key_1"
  value_name <- "..marginplyr_share_value_1"
  summary_name <- "..marginplyr_share_value_1_"
  share_name <- "..marginplyr_set_id_1_"
  id_name <- "..marginplyr_share_match_1_"
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
    "..marginplyr_set_id_1",
    "..marginplyr_share_match_1"
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

  skip_if_backend_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
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

test_that("RSQLite executes portable Total shares end to end", {
  skip_if_backend_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(
    fixed = c(NA_character_, NA_character_, "a", "a"),
    group = c(NA_character_, "x", NA_character_, "x"),
    item = c("i", "j", "i", "j"),
    revenue = c(1, 3, 2, 2),
    units = c(1L, 3L, 0L, 0L),
    unclamped = c(-1, 2, -2, 3)
  )
  remote <- dplyr::copy_to(
    con,
    data,
    "total_share_sqlite_data",
    overwrite = TRUE,
    temporary = TRUE
  )

  summarize <- function(source) {
    summarize_with_margins(
      source,
      level = grouping_id(group, item),
      revenue_total = sum(revenue),
      units_total = sum(units),
      unclamped_total = sum(unclamped),
      missing_total = dplyr::if_else(
        dplyr::n() > 1L,
        NA_real_,
        sum(revenue)
      ),
      revenue_share = share_of_total(revenue_total),
      units_share = share_of_total(units_total),
      unclamped_share = share_of_total(unclamped_total),
      missing_share = share_of_total(missing_total),
      .by = fixed,
      .grouping = rollup(group, item),
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
      group,
      is.na(item),
      item
    )
  }

  expected <- arrange_result(summarize(data))
  query <- summarize(remote)
  sql <- dbplyr::sql_render(query)

  expect_s3_class(query, "tbl_lazy")
  expect_match(sql, "UNION ALL", fixed = TRUE)
  # The fixed key is matched with missing-safe identity, and every measure
  # shares the one denominator join rather than adding a join each.
  expect_match(sql, "IS NULL AND", fixed = TRUE)
  expect_identical(
    lengths(regmatches(sql, gregexpr("LEFT JOIN", sql, fixed = TRUE))),
    1L
  )
  expect_false(grepl("GROUPING SETS", sql, fixed = TRUE))

  result <- arrange_result(dplyr::collect(query))
  expect_equal(as.data.frame(result), as.data.frame(expected))
  expect_type(result$revenue_share, "double")
  expect_true(all(result$revenue_share[result$level == 3L] == 1))
  expect_equal(
    result$revenue_share[result$level == 0L],
    c(0.5, 0.5, 0.75, 0.25)
  )
  # Missing and zero denominators are missing on the database too.
  expect_true(all(is.na(
    result$units_share[result$fixed == "a" & result$level != 3L]
  )))
  expect_true(all(is.na(result$missing_share[result$level != 3L])))
  expect_identical(
    sort(result$unclamped_share[result$level == 0L]),
    c(-2, -1, 2, 3)
  )
})

test_that("DuckDB Total shares agree across native, portable, local paths", {
  skip_if_backend_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
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
    "total_share_data",
    overwrite = TRUE,
    temporary = TRUE
  )

  summarize <- function(source, duplicates) {
    summarize_with_margins(
      source,
      level = grouping_id(group, item),
      revenue = sum(revenue),
      units = sum(units),
      revenue_share = share_of_total(revenue),
      dplyr::across(
        units,
        share_of_total,
        .names = "{.col}_share"
      ),
      .by = fixed,
      .grouping = cube(group, item),
      .duplicates = duplicates,
      .id = "set",
      .margin_label = NULL
    )
  }

  expected <- summarize(data, "drop") |>
    dplyr::arrange(fixed, set, group, item)
  # A cube is the plan a Parent share cannot accept, and every cell of it
  # divides by the one row of the Grand total set in its own fixed partition.
  expect_identical(sum(expected$revenue_share == 1), 2L)
  expect_equal(
    expected$revenue_share[expected$level == 0L],
    c(0.5, 0.5, 0.75, 0.25)
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
    dplyr::mutate(revenue_percent = 100 * revenue_share) |>
    dplyr::arrange(fixed, set, group, item)
  expect_s3_class(composed, "tbl_lazy")
  expect_output(dplyr::show_query(composed), "SELECT")
  expect_equal(
    as.data.frame(dplyr::collect(composed)),
    as.data.frame(
      expected |>
        dplyr::select(fixed, group, item, set, revenue, revenue_share) |>
        dplyr::mutate(revenue_percent = 100 * revenue_share) |>
        dplyr::arrange(fixed, set, group, item)
    )
  )
})

test_that("dtplyr Total shares match local results", {
  skip_if_backend_absent("dtplyr")
  data <- data.frame(
    fixed = c("p", "p", "q", "q"),
    group = c("x", "y", "x", "y"),
    item = c("i", "j", "i", "j"),
    revenue = c(1, 3, 2, 2),
    units = c(1L, 3L, 2L, 2L)
  )
  summarize <- function(source, grouping) {
    summarize_with_margins(
      source,
      level = grouping_id(group, item),
      revenue = sum(revenue),
      units = sum(units),
      revenue_share = share_of_total(revenue),
      units_share = share_of_total(units),
      .by = fixed,
      .grouping = grouping,
      .margin_label = NULL
    )
  }
  arrange_result <- function(result) {
    dplyr::arrange(result, fixed, level, group, item)
  }

  for (grouping in list(rollup(group, item), cube(group, item))) {
    expected <- arrange_result(summarize(data, grouping))
    step <- summarize(dtplyr::lazy_dt(data), grouping)

    expect_s3_class(step, "dtplyr_step")
    result <- arrange_result(dplyr::collect(step))
    expect_equal(as.data.frame(result), as.data.frame(expected))
    expect_type(result$revenue_share, "double")
    expect_true(all(result$revenue_share[result$level == 3L] == 1))
    expect_equal(
      result$revenue_share[result$level == 0L],
      c(0.25, 0.75, 0.5, 0.5)
    )
  }

  # Without fixed keys the denominator is one row and the join key standing in
  # for a partition is internal: it must not reach the result.
  unpartitioned <- dplyr::collect(summarize_with_margins(
    dtplyr::lazy_dt(data),
    revenue = sum(revenue),
    revenue_share = share_of_total(revenue),
    .grouping = rollup(group),
    .margin_label = NULL
  ))
  expect_identical(names(unpartitioned), c("group", "revenue", "revenue_share"))
  expect_equal(sum(unpartitioned$revenue_share), 2)
})

test_that("dtplyr rejects ineligible Total-share sources on collection", {
  skip_if_backend_absent("dtplyr")
  step <- dtplyr::lazy_dt(data.frame(group = c("x", "y"), value = 1:2))

  query <- summarize_with_margins(
    step,
    total = as.Date("2026-01-01"),
    whole = share_of_total(total),
    .grouping = rollup(group),
    .margin_label = NULL
  )
  expect_s3_class(query, "dtplyr_step")

  error <- expect_error(
    dplyr::collect(query),
    "plain integer or double scalar"
  )
  expect_s3_class(error, "marginplyr_error")
  # The execution-time diagnostic names the helper the caller wrote, the
  # output, the source, and the original public call.
  expect_match(conditionMessage(error), "Total share `whole`")
  expect_match(conditionMessage(error), "source summary `total`")
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "summarize_with_margins"
  )

  across_error <- expect_error(
    dplyr::collect(summarize_with_margins(
      step,
      dplyr::across(value, function(x) c(min(x), max(x))),
      dplyr::across(value, share_of_total, .names = "{.col}_share"),
      .grouping = rollup(group),
      .margin_label = NULL
    )),
    "exactly one value per grouping row"
  )
  expect_s3_class(across_error, "marginplyr_share_cardinality_error")
  expect_match(conditionMessage(across_error), "Total share `value_share`")
})

test_that("Arrow rejects Total shares before constructing a query", {
  skip_if_backend_absent("arrow")
  source <- arrow::Table$create(data.frame(
    group = c("x", "y"),
    value = 1:2
  ))
  calls <- new.env(parent = emptyenv())
  calls$summarize <- 0L
  calls$collect <- 0L
  testthat::local_mocked_bindings(
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
      whole = share_of_total(total),
      .grouping = cube(group),
      .margin_label = NULL
    ),
    "Arrow.*Total share"
  )
  expect_s3_class(error, "marginplyr_error")
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "summarize_with_margins"
  )
  expect_snapshot(conditionMessage(error))

  # A call using both helpers is refused once, naming both.
  both <- expect_error(
    summarize_with_margins(
      source,
      total = sum(value),
      parent = share_of_parent(total),
      whole = share_of_total(total),
      .grouping = rollup(group),
      .margin_label = NULL
    ),
    "Arrow"
  )
  expect_snapshot(conditionMessage(both))

  expect_identical(calls$summarize, 0L)
  expect_identical(calls$collect, 0L)
})
