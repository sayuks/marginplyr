# `.check_share_source = FALSE` appears throughout this file wherever a share
# is requested of a connection that executes nothing -- every `simulate_*()`
# one, and every live SQLite one. Neither can establish that a share source is
# eligible: a simulator answers no query at all, and SQLite's dialect converts
# a value of another type to a number rather than refusing it, so it would
# accept any source whatever. What each of those tests is about is the SQL the
# staged share produces, or the values it produces from sources the test
# itself defines, so opting out of the establishing rule is what leaves them
# testing that. The rule itself is covered where it belongs, in the two tests
# named for it below.
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
      .margin_label = NULL,
      .check_share_source = FALSE
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

  if (suggest_available("dtplyr")) {
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
  skip_if_suggest_absent("dtplyr")
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
  skip_if_suggest_absent("dtplyr")
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
  skip_if_suggest_absent("dtplyr")
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
  skip_if_suggest_absent("dtplyr")
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

# The branch that builds no ratio is not conditioned on backend kind, so
# data.table translates the same expression the SQL kinds send (#446). What is
# at risk here is the value: the refusal below is the local eligible-type
# check, which dtplyr reaches before any share is built.
test_that("dtplyr Total shares needing no join match local results", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(
    group = c("x", "x", "y"),
    integer_value = 1:3,
    double_value = c(0.5, 1.5, 4),
    missing_value = rep(NA_real_, 3L),
    zero_value = c(0, 0, 0),
    character_value = c("1", "2", "3")
  )
  # No `.grouping`, so the only occurrence is the Grand total set and every
  # share is its own denominator.
  summarize <- function(source) {
    summarize_with_margins(
      source,
      integer_total = sum(integer_value),
      double_total = sum(double_value),
      missing_total = sum(missing_value),
      zero_total = sum(zero_value),
      integer_share = share_of_total(integer_total),
      double_share = share_of_total(double_total),
      missing_share = share_of_total(missing_total),
      zero_share = share_of_total(zero_total)
    )
  }

  expected <- summarize(data)
  query <- summarize(dtplyr::lazy_dt(data))
  expect_s3_class(query, "dtplyr_step")
  result <- dplyr::collect(query)
  expect_equal(as.data.frame(result), as.data.frame(expected))
  for (share in c("integer", "double", "missing", "zero")) {
    expect_identical(result[[paste0(share, "_share")]], 1, info = share)
  }

  expect_error(
    dplyr::collect(summarize_with_margins(
      dtplyr::lazy_dt(data),
      total = max(character_value),
      whole = share_of_total(total)
    )),
    "plain integer or double scalar"
  )
})

test_that("dtplyr validates each referenced source expanded by across", {
  skip_if_suggest_absent("dtplyr")
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
  skip_if_suggest_absent("dtplyr")
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
  skip_if_suggest_absent("dtplyr")
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
  skip_if_suggest_absent("dtplyr")
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
  skip_if_suggest_absent("dtplyr")
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
  skip_if_suggest_absent("arrow")
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
  skip_if_suggest_absent("arrow")
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
  skip_if_suggest_absent("arrow")
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
  skip_if_suggest_absent("dtplyr")
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

# How many times a rendered query contains a pattern, and zero where it
# contains none. `gregexpr()` answers `-1` for a pattern it does not find
# and `lengths()` counts that as one element, so the count goes through
# `regmatches()`, which drops what did not match (#362).
#
# A caller comparing one of these counts with another asserts that the count is
# positive as well: an equality on its own is satisfied by two absences. One
# comparing against a positive literal already has.
share_query_count <- function(sql, pattern) {
  lengths(regmatches(sql, gregexpr(pattern, sql, fixed = TRUE)))
}

test_that("a pattern is counted once per occurrence, and none where absent", {
  # Held here because no caller can observe it: an absence counted as one
  # occurrence reads exactly like one occurrence.
  expect_identical(share_query_count("SELECT one FROM two", "absent"), 0L)
  expect_identical(share_query_count("SELECT one FROM two", "one"), 1L)
  expect_identical(share_query_count("one and one", "one"), 2L)
})

test_that("dtplyr batches validated summaries and parent mapping", {
  skip_if_suggest_absent("dtplyr")
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

  # A dtplyr share stages `check_dtplyr_share_source()`. `check_share_scalar()`
  # is the local backend's wrapper and appears in no dtplyr rendering.
  staged_check <- "check_dtplyr_share_source(sum(revenue)"
  staged_checks <- share_query_count(many_call, staged_check)
  expect_gt(staged_checks, 0L)
  expect_identical(staged_checks, share_query_count(one_call, staged_check))
  expect_identical(share_query_count(many_call, "allow.cartesian = TRUE"), 1L)
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
    .margin_label = NULL,
    .check_share_source = FALSE
  )
  many <- summarize_with_margins(
    remote,
    revenue = sum(revenue),
    units = sum(units),
    revenue_share = share_of_parent(revenue),
    units_share = share_of_parent(units),
    .grouping = rollup(region, store),
    .margin_label = NULL,
    .check_share_source = FALSE
  )
  one_sql <- dbplyr::sql_render(one)
  many_sql <- dbplyr::sql_render(many)

  expect_match(many_sql, "GROUP BY GROUPING SETS", fixed = TRUE)
  expect_identical(share_query_count(many_sql, "LEFT JOIN"), 1L)
  expect_identical(
    share_query_count(many_sql, "GROUP BY GROUPING SETS"),
    share_query_count(one_sql, "GROUP BY GROUPING SETS")
  )
  expect_match(many_sql, "IS NULL AND", fixed = TRUE)
  expect_match(many_sql, "CAST(", fixed = TRUE)
})

# The counters are the contract: requesting a share reads none of the caller's
# data, so nothing asks this connection for the staged query's fields, its row
# count, or its results — whichever way `.check_share_source` is set, and
# whether the call is answered with a query or with a refusal. The ineligible
# source is what shows the second half: the share is not calculated from a
# value marginplyr checked, it is left to the database, and this connection is
# never asked to type it.
test_that("general dbplyr builds a share without reading the caller's data", {
  remote <- new_parent_lazy_probe(
    data.frame(group = "x", label = "value")
  )
  summarize <- function(...) {
    summarize_with_margins(
      remote,
      label = min(label),
      share = share_of_parent(label),
      .grouping = rollup(group),
      .margin_label = NULL,
      ...
    )
  }
  unread <- c(result_type = 0L, cardinality = 0L, collection = 0L)

  query <- summarize(.check_share_source = FALSE)
  expect_s3_class(query, "tbl_lazy")
  expect_identical(parent_lazy_probe_counts(), unread)

  sql <- dbplyr::sql_render(query)
  expect_identical(parent_lazy_probe_counts(), unread)
  expect_match(sql, "CAST(", fixed = TRUE)
  expect_match(sql, "LEFT JOIN", fixed = TRUE)

  # A connection that answers nothing cannot say whether its dialect converts,
  # so the default refuses the share rather than calculating one nothing
  # stands behind — and it refuses without reading the caller's data either.
  refusal <- expect_error(summarize(), class = "marginplyr_error")
  expect_match(conditionMessage(refusal), "could not be asked", fixed = TRUE)
  expect_identical(parent_lazy_probe_counts(), unread)
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
      .margin_label = NULL,
      .check_share_source = FALSE
    )
    sql <- dbplyr::sql_render(query)

    expect_match(sql, "UNION ALL", fixed = TRUE, info = simulator)
    expect_identical(
      share_query_count(sql, "LEFT JOIN"),
      1L,
      info = simulator
    )
    expect_match(sql, "IS NULL AND", fixed = TRUE, info = simulator)
    # A simulator has no database behind it, so neither of these executes and
    # each stands for a property a dialect that does execute is held to: the
    # cast for the integer source share `RSQLite executes portable Parent
    # shares end to end` compares against a local result, and the `* 1` for the
    # refusal `DuckDB refuses a character share source whatever it holds`
    # asserts (#429). The character class is what keeps the second from passing
    # on a `* 1.0`, which `DuckDB shares a source at its declared type's
    # maximum` is the executed gate against.
    expect_match(sql, "(CAST|CDBL)\\(", info = simulator)
    expect_match(sql, "\\* 1[^.0-9]", info = simulator)
    expect_false(
      grepl("GROUPING SETS", sql, fixed = TRUE),
      info = simulator
    )
  }
})

test_that("RSQLite executes portable Parent shares end to end", {
  skip_if_suggest_absent("RSQLite", "DBI")
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
      .margin_label = "Margin",
      .check_share_source = FALSE
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
  skip_if_suggest_absent("RSQLite", "DBI")
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
    .margin_label = NULL,
    .check_share_source = FALSE
  )

  expect_s3_class(query, "tbl_lazy")
  error <- expect_error(dplyr::collect(query))
  expect_identical(class(error), class(baseline))
  expect_identical(class(error$parent), class(baseline$parent))
  expect_false(inherits(error, "marginplyr_error"))
})

# The reproduction from #195, on the dialect #106 was filed about. SQLite
# answers `sum(<text column>)` with a genuine `0` rather than refusing it, so
# no reading of the result distinguishes an ineligible source from an eligible
# one, and the share it used to produce — an all-missing column beside the
# grand total's own-denominator `1` — reads as 100%. What is asserted here is
# that the eligible source is refused too: eligibility is a question about the
# dialect, and this dialect answers it the same way whatever the call
# summarizes.
test_that("RSQLite refuses a share its dialect cannot establish", {
  skip_if_suggest_absent("RSQLite", "DBI")
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
  summarize <- function(source, ...) {
    summarize_with_margins(
      source,
      lab = max(region),
      p = share_of_parent(lab),
      .grouping = rollup(region, store),
      ...
    )
  }
  eligible <- function(source, ...) {
    summarize_with_margins(
      source,
      total = sum(revenue),
      p = share_of_total(total),
      .grouping = rollup(region, store),
      ...
    )
  }

  refusal <- expect_error(summarize(remote), class = "marginplyr_error")
  expect_snapshot(conditionMessage(refusal))
  expect_identical(
    rlang::call_name(conditionCall(refusal)),
    "summarize_with_margins"
  )
  expect_error(eligible(remote), class = "marginplyr_error")

  # Forced, the ineligible source is calculated from what the dialect converted
  # it to, which is the answer #195 reported and is now the caller's own
  # request. The eligible one agrees with the local result.
  forced <- dplyr::collect(summarize(remote, .check_share_source = FALSE))
  expect_true(all(is.na(forced$p[forced$lab != "W"]) | forced$p == 1))
  expect_equal(
    as.data.frame(dplyr::collect(
      eligible(remote, .check_share_source = FALSE) |>
        dplyr::arrange(region, store)
    )),
    as.data.frame(dplyr::arrange(
      eligible(data, .check_share_source = FALSE),
      region,
      store
    ))
  )
  # The local backend holds its summaries' own types, so the rule applies there
  # whatever this argument says, and it is the eligible-type diagnostic rather
  # than this refusal that a local caller gets.
  local_error <- expect_error(
    summarize(data, .check_share_source = FALSE),
    "plain integer or double scalar"
  )
  expect_identical(local_error$source_summary, "lab")
})

# This test's predecessor guarded a probe query that no longer exists. That
# probe collected the caller's own summaries, so `grouping_id()` and
# `grouping_bit()` -- marginplyr's spellings, which no backend has functions
# for -- failed it as a whole and the rule was lost for the measures written
# beside them. The probe is gone, but the guarantee it protected is not: a call
# that identifies its Margin levels must still reach the rule for its measures,
# and must still calculate them once the caller takes that rule on themselves.
test_that("RSQLite keeps the share rule beside Margin level helpers", {
  skip_if_suggest_absent("RSQLite", "DBI")
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
    "share_source_level_sqlite_data",
    overwrite = TRUE,
    temporary = TRUE
  )
  summarize <- function(source, ...) {
    summarize_with_margins(
      source,
      level = grouping_id(region, store),
      bit = grouping_bit(store),
      total = sum(revenue),
      p = share_of_parent(total),
      .grouping = rollup(region, store),
      ...
    )
  }

  refusal <- expect_error(summarize(remote), class = "marginplyr_error")
  expect_match(conditionMessage(refusal), "cannot establish")

  # Taken on by the caller, the helpers and the share are calculated together,
  # and against the local result rather than against another backend, so this
  # cannot pass by being self-consistently wrong.
  expect_equal(
    as.data.frame(dplyr::collect(dplyr::arrange(
      summarize(remote, .check_share_source = FALSE),
      region,
      store
    ))),
    as.data.frame(dplyr::arrange(
      summarize(data, .check_share_source = FALSE),
      region,
      store
    ))
  )
})

test_that("RSQLite calculates eligible shares a caller has established", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(
    group = c("x", "x", "y"),
    revenue = c(1, 3, 2),
    units = c(1L, 3L, 0L),
    # A source whose first values are missing is still a source: what the
    # dialect converts is the type of a value, and a missing one carries none.
    sparse = c(NA_real_, 4, 5)
  )
  remote <- dplyr::copy_to(
    con,
    data,
    "share_source_forced_sqlite_data",
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
      .margin_label = NULL,
      .check_share_source = FALSE
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

test_that("a lazy backend that answers nothing refuses to establish a share", {
  skip_if_no_sqlite_simulation()
  # A simulated connection executes no query, so it can say nothing about what
  # its dialect does with an ineligible summary. Reading its failure as the
  # refusal a live database of that dialect would make is what the default
  # declines to do; asked to, it stays lazy and renders as any other.
  remote <- dbplyr::tbl_lazy(
    data.frame(group = c("x", "y"), value = 1:2),
    con = dbplyr::simulate_sqlite()
  )
  summarize <- function(...) {
    summarize_with_margins(
      remote,
      total = sum(value),
      share = share_of_parent(total),
      .grouping = rollup(group),
      .margin_label = NULL,
      ...
    )
  }

  refusal <- expect_error(summarize(), class = "marginplyr_error")
  expect_snapshot(conditionMessage(refusal))

  query <- summarize(.check_share_source = FALSE)
  expect_s3_class(query, "tbl_lazy")
  expect_match(dbplyr::sql_render(query), "UNION ALL", fixed = TRUE)
})

# Only the three tests below need either helper from
# `helper-share-dialect-verdicts.R`: the verdict tests after them call
# `probe_share_dialect()` directly or assert the cache as they found it, and
# the live-backend tests further down write to it through the verb without
# reading what it holds.

# What the dialect does with an ineligible summary is a property of the
# dialect, so it is asked once and reused — which is only observable across
# calls, and only from an empty cache. The second half is why the question is
# asked of the connection before it is put to it: a connection that executes
# nothing answers for itself and not for its dialect, so nothing it does is
# recorded where a live connection carrying that dialect would find it.
test_that("a dialect is asked whether it converts at most once", {
  remote <- dbplyr::tbl_lazy(
    data.frame(group = c("x", "y"), value = c(1, 3)),
    con = dbplyr::simulate_postgres()
  )
  backend <- grouping_backend(remote)
  saved <- as.list(share_dialect_verdicts, all.names = TRUE)
  on.exit(restore_share_dialect_verdicts(saved), add = TRUE)
  probes <- 0L
  local_mocked_bindings(
    probe_share_dialect = function(con) {
      probes <<- probes + 1L
      "refuses"
    }
  )

  empty_share_dialect_verdicts()
  local_mocked_bindings(share_dialect_can_be_asked = function(con) TRUE)
  expect_identical(share_dialect_verdict(remote, backend = backend), "refuses")
  expect_identical(share_dialect_verdict(remote, backend = backend), "refuses")
  expect_identical(probes, 1L)
  expect_identical(
    ls(share_dialect_verdicts, all.names = TRUE),
    paste(class(backend$dialect), collapse = "\n")
  )
})

test_that("a connection that answers nothing records nothing for its dialect", {
  remote <- dbplyr::tbl_lazy(
    data.frame(group = c("x", "y"), value = c(1, 3)),
    con = dbplyr::simulate_postgres()
  )
  backend <- grouping_backend(remote)
  saved <- as.list(share_dialect_verdicts, all.names = TRUE)
  on.exit(restore_share_dialect_verdicts(saved), add = TRUE)
  empty_share_dialect_verdicts()

  expect_identical(share_dialect_verdict(remote, backend = backend), "unknown")
  expect_identical(ls(share_dialect_verdicts, all.names = TRUE), character())
})

# `"refuses"` and `"converts"` are facts about the dialect, which is what makes
# reusing them sound. `"unknown"` is a fact about one attempt: a dropped
# socket, a permissions blip, or a warehouse that was resuming produces it on a
# connection whose dialect would answer perfectly well. Recording it would
# refuse shares on that dialect for the rest of the session, on every later
# connection carrying it, with `.check_share_source = FALSE` -- opting out of
# the rule entirely -- the only way back. So the question is asked again, and
# the answer it then gives is the one that is recorded and reused.
#
# The third request is what keeps this from being satisfied by not caching at
# all: the mock has no third answer, so a verdict that is asked again after
# answering fails here rather than passing.
test_that("a question that went unanswered is asked again", {
  remote <- dbplyr::tbl_lazy(
    data.frame(group = c("x", "y"), value = c(1, 3)),
    con = dbplyr::simulate_postgres()
  )
  backend <- grouping_backend(remote)
  key <- paste(class(backend$dialect), collapse = "\n")
  saved <- as.list(share_dialect_verdicts, all.names = TRUE)
  on.exit(restore_share_dialect_verdicts(saved), add = TRUE)
  answers <- c("unknown", "refuses")
  probes <- 0L
  local_mocked_bindings(
    share_dialect_can_be_asked = function(con) TRUE,
    probe_share_dialect = function(con) {
      probes <<- probes + 1L
      answers[[probes]]
    }
  )

  empty_share_dialect_verdicts()
  expect_identical(share_dialect_verdict(remote, backend = backend), "unknown")
  expect_identical(ls(share_dialect_verdicts, all.names = TRUE), character())

  expect_identical(share_dialect_verdict(remote, backend = backend), "refuses")
  expect_identical(probes, 2L)
  expect_identical(share_dialect_verdicts[[key]], "refuses")

  expect_identical(share_dialect_verdict(remote, backend = backend), "refuses")
  expect_identical(probes, 2L)
})

# `share_source_checker()` routes the `other` backend kind to the dialect
# checker as well, and an input of that kind carries no connection to put the
# question to. Answering "unknown" is what refuses the share there; failing
# instead would turn a backend marginplyr merely does not recognize into a
# crash. Each helper is asserted beside the verdict because each answers for a
# different absence: no lazy table at all, and no connection behind one.
test_that("an input carrying no connection is asked nothing", {
  data <- data.frame(group = c("x", "y"), value = c(1, 3))
  backend <- grouping_backend(data)
  recorded <- ls(share_dialect_verdicts, all.names = TRUE)

  expect_null(share_dialect_connection(data))
  expect_false(share_dialect_can_be_asked(NULL))
  expect_identical(share_dialect_verdict(data, backend = backend), "unknown")
  expect_identical(ls(share_dialect_verdicts, all.names = TRUE), recorded)
})

# The verdict is read from whether executing the probe raised, so a connection
# no query can be built against at all has raised nothing to read. Falling to
# "unknown" is what keeps that from being recorded as the refusal a live
# dialect would have earned, which would switch the protection off for every
# later connection carrying that dialect.
test_that("a connection no query can be built against answers nothing", {
  expect_identical(
    probe_share_dialect(structure(list(), class = "not_a_connection")),
    "unknown"
  )
})

# Only a one-row, one-column number is the conversion. Any other answer --
# more columns, no rows, a value of another type, or something that is not a
# data frame -- is no reading of the dialect, and "unknown" refuses the share
# rather than accept a shape nothing interpreted. The converting answer is
# asserted in the same block as a control: without it, a mock that stopped
# reaching `probe_share_dialect()` would report every shape unknown and pass.
test_that("an answer of an unexpected shape is not read as a verdict", {
  verdict_for_answer <- function(answer) {
    local_mocked_bindings(
      collect = function(x, ...) answer,
      .package = "dplyr"
    )
    probe_share_dialect(dbplyr::simulate_postgres())
  }

  expect_identical(verdict_for_answer(data.frame(p = 0)), "converts")
  # A number the driver mapped to a class of its own is still the conversion.
  expect_identical(
    verdict_for_answer(data.frame(p = structure(0, class = "driver_mapped"))),
    "converts"
  )
  expect_identical(verdict_for_answer(data.frame(a = 1, b = 2)), "unknown")
  expect_identical(verdict_for_answer(data.frame(p = numeric(0))), "unknown")
  expect_identical(verdict_for_answer(data.frame(p = "x")), "unknown")
  expect_identical(verdict_for_answer(list(p = 1)), "unknown")
})

# The four answering cases are RPostgres's four `bigint` mappings, each
# measured `"refuses"` against a live PostgreSQL 17.11. The classed one carries
# a name no package defines methods for, deliberately: a double built by hand
# does not hold a `bit64::integer64` bit pattern, so with bit64 loaded
# `structure(1, class = "integer64") == 1` is `FALSE` where
# `bit64::as.integer64(1) == 1` is `TRUE`, and borrowing the name would assert
# the driver's arithmetic rather than this reading.
test_that("the control takes the scaffolding's number in any driver type", {
  verdict_when_control_returns <- function(value) {
    local_mocked_bindings(
      collect = local({
        calls <- 0L
        function(x, ...) {
          calls <<- calls + 1L
          if (calls == 1L) {
            stop("function sum(unknown) does not exist")
          }
          data.frame(p = value)
        }
      }),
      .package = "dplyr"
    )
    probe_share_dialect(dbplyr::simulate_postgres())
  }
  classed <- structure(1, class = "driver_mapped_bigint")
  expect_false(is_share_source_type(classed))

  expect_identical(verdict_when_control_returns(classed), "refuses")
  expect_identical(verdict_when_control_returns(1L), "refuses")
  expect_identical(verdict_when_control_returns(1), "refuses")
  expect_identical(verdict_when_control_returns("1"), "refuses")

  expect_identical(verdict_when_control_returns(7), "unknown")
  expect_identical(verdict_when_control_returns("x"), "unknown")
  expect_identical(verdict_when_control_returns(NA), "unknown")

  # Every one of these compares equal to `1` under R's coercion rules.
  expect_identical(verdict_when_control_returns(TRUE), "unknown")
  expect_identical(verdict_when_control_returns(factor("1")), "unknown")
  expect_identical(
    verdict_when_control_returns(as.Date("1970-01-02")),
    "unknown"
  )
  expect_identical(verdict_when_control_returns(I(list(1))), "unknown")
  expect_identical(verdict_when_control_returns(as.raw(1)), "unknown")

  # A class that passes the type test and raises inside `==`. The method is
  # registered in the global environment because one defined in this frame is
  # not dispatched from marginplyr's namespace, where the comparison runs.
  assign(
    "==.raises_on_comparison",
    function(e1, e2) stop("cannot compare"),
    envir = globalenv()
  )
  on.exit(rm("==.raises_on_comparison", envir = globalenv()), add = TRUE)
  expect_identical(
    verdict_when_control_returns(structure(1, class = "raises_on_comparison")),
    "unknown"
  )
})

# Every wrap whose value decides the verdict reads an error as something the
# connection did, so a marginplyr frame inside one would have its own defect
# reported as the dialect. `share_probe_scaffold()` is the frame both readings
# reach, and it is read outside all of them.
test_that("a defect in marginplyr's own frame is not read as a dialect", {
  local_mocked_bindings(
    share_probe_scaffold = function() stop("scaffold defect")
  )

  expect_error(
    probe_share_dialect(dbplyr::simulate_postgres()),
    "scaffold defect"
  )
  expect_error(
    probe_share_dialect_holds(1, control = TRUE),
    "scaffold defect"
  )
})

# Reading any raised query as the dialect's refusal is how the protection came
# to be off exactly where it was needed: `"refuses"` is the verdict that
# proceeds, so every unrelated reason a query can raise switched the rule off
# and cached that for the dialect. The scaffolding `SELECT 1 AS z` reaches the
# database verbatim and has no `FROM`, which Oracle and SAP HANA both reject,
# and a dropped connection raises the same way. None of those may read as a
# refusal.
test_that("a query that raises for another reason is not read as a refusal", {
  verdict_when_collect <- function(fn) {
    local_mocked_bindings(collect = fn, .package = "dplyr")
    probe_share_dialect(dbplyr::simulate_postgres())
  }

  # Nothing executes here, so the control cannot come back either.
  expect_identical(
    verdict_when_collect(function(x, ...) {
      stop("ORA-00923: FROM keyword not found where expected")
    }),
    "unknown"
  )
  expect_identical(
    verdict_when_collect(function(x, ...) stop("could not connect to server")),
    "unknown"
  )

  # Rejecting the string while answering the control is the one shape that is
  # a refusal, and it is what DuckDB does.
  raised_first <- local({
    calls <- 0L
    function(x, ...) {
      calls <<- calls + 1L
      if (calls == 1L) {
        stop("Binder Error: No function matches SUM(VARCHAR)")
      }
      data.frame(p = 1)
    }
  })
  expect_identical(verdict_when_collect(raised_first), "refuses")
})

# The control is only the price of telling two failures apart, so a dialect
# that answers the first question is never asked a second one.
test_that("a converting dialect is asked exactly one query", {
  queries <- 0L
  verdict_counting <- function() {
    local_mocked_bindings(
      collect = function(x, ...) {
        queries <<- queries + 1L
        data.frame(p = 0)
      },
      .package = "dplyr"
    )
    probe_share_dialect(dbplyr::simulate_postgres())
  }

  expect_identical(verdict_counting(), "converts")
  expect_identical(queries, 1L)
})

# An unrecognized kind is a marginplyr defect and not something a caller can
# rewrite, so it stops bare rather than raising a Package condition
# (ADR 0015). The class assertion is the load-bearing half: a defect caught by
# `tryCatch(marginplyr_error = )` would reach a caller as though their own call
# were the thing to fix.
test_that("the source checker refuses an unrecognized backend kind", {
  error <- expect_error(
    share_source_checker("nonexistent"),
    "Unknown contextual-share source-checker backend kind: nonexistent"
  )
  expect_false(inherits(error, "marginplyr_error"))
})

# #106's DuckDB half. This dialect refuses an ineligible summary itself, so
# marginplyr calculates the share and the database reports the refusal when the
# caller executes the query — which is why the column it casts carries the name
# of the summary to rewrite. Nothing here is a marginplyr condition: the
# diagnostic is the database's own, and the assertion is that it is usable.
test_that("DuckDB reports an ineligible share source against its summary", {
  skip_if_suggest_absent("duckdb", "DBI")
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
  expect_identical(local_error$source_summary, "lab")

  query <- summarize(remote)
  expect_s3_class(query, "tbl_lazy")
  remote_error <- expect_error(dplyr::collect(query))
  expect_false(inherits(remote_error, "marginplyr_error"))
  expect_match(
    conditionMessage(remote_error),
    "denominator_of_lab",
    fixed = TRUE
  )

  # #196 asks for this diagnostic to be snapshotted, and what is snapshotted is
  # the internal names it exposes rather than the message carrying them.
  # DuckDB's own text is its version's wording, a `DECIMAL` precision, and a
  # `LINE` marker with column offsets, none of which this assertion is about --
  # snapshotting it whole would fail on a DuckDB upgrade while saying nothing
  # about the property. The property is which marginplyr identifier a reader is
  # left holding, and the whole of #106's DuckDB half was that it read
  # `..marginplyr_share_value_1`, which names nothing the caller wrote.
  message <- conditionMessage(remote_error)
  expect_snapshot(
    unique(unlist(regmatches(
      message,
      gregexpr("[.][.]marginplyr_[A-Za-z0-9_]+", message)
    )))
  )
})

# The multiplication that refuses a character source is applied to a numeric
# one too, so it has to be an operation that cannot fail for a value the
# source's own declared type holds. `copy_to()` cannot express such a type,
# which is why this one is created in SQL: DuckDB types `DECIMAL(18,2) * 1.0`
# as `DECIMAL(18,3)`, which overflows at the maximum below, while `* 1` stays
# `DECIMAL(18,2)` and answers. The measures differ only in the multiplication,
# so nothing else here distinguishes them (#429).
test_that("DuckDB shares a source at its declared type's maximum", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(
    con,
    paste(
      "CREATE TEMPORARY TABLE share_source_wide_decimal_data AS",
      "SELECT 'E' AS region, CAST(9999999999999999.99 AS DECIMAL(18, 2)) AS m",
      "UNION ALL",
      "SELECT 'E', CAST(1.00 AS DECIMAL(18, 2))",
      "UNION ALL",
      "SELECT 'W', CAST(9999999999999999.99 AS DECIMAL(18, 2))"
    )
  )
  remote <- dplyr::tbl(con, "share_source_wide_decimal_data")

  result <- dplyr::collect(summarize_with_margins(
    remote,
    total = max(m),
    parent = share_of_parent(total),
    grand = share_of_total(total),
    .grouping = rollup(region)
  ))

  # Every occurrence's maximum is the same value, so each share is one exactly
  # and no comparison here depends on how the decimal rounds to a double.
  expect_type(result$parent, "double")
  expect_identical(result$parent, rep(1, 3L))
  expect_identical(result$grand, rep(1, 3L))
})

# #429. What refuses a character source here is the multiplication in the
# staged ratio, which binds by type, so the two columns below are refused for
# the same reason and neither's values are part of it. `DuckDB reports an
# ineligible share source against its summary` covers only the non-numeric
# column, which is why it did not reach #429.
test_that("DuckDB refuses a character share source whatever it holds", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(
    region = c("E", "E", "W"),
    numeric_looking = c("1", "2", "3"),
    non_numeric = c("n", "m", "o"),
    revenue = c(1, 2, 4)
  )
  remote <- dplyr::copy_to(
    con,
    data,
    "share_source_character_duckdb_data",
    overwrite = TRUE,
    temporary = TRUE
  )
  summarize <- function(source, column) {
    summarize_with_margins(
      source,
      total = max(!!rlang::sym(column)),
      parent = share_of_parent(total),
      grand = share_of_total(total),
      .grouping = rollup(region)
    )
  }

  # The refusal below is of a character source and not of every share on the
  # dialect, which is what separated this fix from reclassifying DuckDB as
  # converting. It runs first because it is also what makes those refusals
  # attributable to the source: the errors are read for their class alone,
  # since DuckDB's wording is its version's, so without this a dropped table
  # or a renamed column would satisfy them.
  eligible <- dplyr::collect(summarize(remote, "revenue"))
  expect_identical(sort(eligible$parent), c(0.5, 1, 1))
  expect_identical(sort(eligible$grand), c(0.5, 1, 1))

  for (column in c("numeric_looking", "non_numeric")) {
    expect_error(
      summarize(data, column),
      "plain integer or double scalar",
      info = column
    )
    query <- summarize(remote, column)
    expect_s3_class(query, "tbl_lazy")
    error <- expect_error(dplyr::collect(query), info = column)
    expect_false(inherits(error, "marginplyr_error"), info = column)
  }
})

# #446. The plan below gives every occurrence its own denominator, so no
# denominator is joined and the share is built as a constant. The refusal here
# is therefore of the expression that branch emits for the sole purpose of
# binding the source, and not of the ratio `DuckDB refuses a character share
# source whatever it holds` covers. Only a Total share reaches it: a Parent
# share requires a rollup, and every rollup gives some occurrence a parent.
test_that("DuckDB refuses a character source for a share that needs no join", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(
    region = c("E", "E", "W"),
    numeric_looking = c("1", "2", "3"),
    non_numeric = c("n", "m", "o"),
    revenue = c(1, 2, 4),
    zero = c(0, 0, 0),
    missing = rep(NA_real_, 3L)
  )
  remote <- dplyr::copy_to(
    con,
    data,
    "share_whole_character_duckdb_data",
    overwrite = TRUE,
    temporary = TRUE
  )
  # `.grouping` is left absent so that the shape under test is the default one
  # a Total share takes, rather than a spelling of it.
  summarize <- function(source, column, ...) {
    summarize_with_margins(
      source,
      total = max(!!rlang::sym(column)),
      whole = share_of_total(total),
      .by = region,
      ...
    )
  }

  # Runs first for the reason the joined refusal's does: the errors a
  # collection raises below are read for their class alone, so without this a
  # dropped table would satisfy them.
  for (column in c("revenue", "zero", "missing")) {
    eligible <- dplyr::collect(summarize(remote, column))
    expect_type(eligible$whole, "double")
    expect_identical(eligible$whole, rep(1, nrow(eligible)), info = column)
  }

  for (column in c("numeric_looking", "non_numeric")) {
    expect_error(
      summarize(data, column),
      "plain integer or double scalar",
      info = column
    )
    query <- summarize(remote, column)
    expect_s3_class(query, "tbl_lazy")
    error <- expect_error(dplyr::collect(query), info = column)
    expect_false(inherits(error, "marginplyr_error"), info = column)

    # `.check_share_source = FALSE` governs the dialect-verdict refusal only,
    # so it does not relax the eligible-type rule on either path.
    expect_error(
      summarize(data, column, .check_share_source = FALSE),
      "plain integer or double scalar",
      info = column
    )
    relaxed <- summarize(remote, column, .check_share_source = FALSE)
    expect_s3_class(relaxed, "tbl_lazy")
    expect_error(dplyr::collect(relaxed), info = column)
  }
})

test_that("DuckDB Parent shares agree across native, portable, local paths", {
  skip_if_suggest_absent("duckdb", "DBI")
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
  skip_if_suggest_absent("dtplyr")
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
  skip_if_suggest_absent("duckdb", "DBI")
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
  skip_if_suggest_absent("dtplyr")
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
  skip_if_suggest_absent("duckdb", "DBI")
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
  value_name <- "value"
  summary_name <- "total"
  # The name the join reserves for `total`'s denominator, written here as a
  # summary of the caller's own so that it is already in the staged result when
  # the allocator asks for it.
  denominator_name <- "..marginplyr_denominator_of_total_1"
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
      !!denominator_name := sum(.data[[value_name]]),
      !!share_name := share_of_parent(!!rlang::sym(summary_name)),
      .grouping = rollup(dplyr::all_of(group_name)),
      .margin_label = group_name,
      .check_margin_label = FALSE,
      .check_share_source = FALSE,
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

  skip_if_suggest_absent("duckdb", "DBI")
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
  skip_if_suggest_absent("RSQLite", "DBI")
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
      .margin_label = "Margin",
      .check_share_source = FALSE
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
  expect_identical(share_query_count(sql, "LEFT JOIN"), 1L)
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
  skip_if_suggest_absent("duckdb", "DBI")
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
  skip_if_suggest_absent("dtplyr")
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

# The two backends below answer an injected share selection today, and for
# reasons that are not the local backend's: dtplyr carries the caller's call as
# text, which no second defusal reads, and a lazy backend wraps no source in the
# summary at all. Neither reason is stated where it could be read off the
# result, so a change to either would be reported by nothing (#357).
test_that("dtplyr answers an injected share `across()` selection", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(group = c("x", "y"), units = c(1, 3))
  summarize <- function(source, selection) {
    summarize_with_margins(
      source,
      units = sum(units),
      dplyr::across({{ selection }}, share_of_total, .names = "{.col}_share"),
      .grouping = rollup(group),
      .margin_label = NULL
    )
  }

  expected <- summarize(data, units)
  result <- dplyr::collect(summarize(dtplyr::lazy_dt(data), units))

  expect_equal(as.data.frame(result), as.data.frame(expected))
  expect_equal(result$units_share, c(0.25, 0.75, 1))
})

test_that("DuckDB answers an injected share `across()` selection", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(group = c("x", "y"), units = c(1, 3))
  remote <- dplyr::copy_to(
    con,
    data,
    "injected_share_data",
    overwrite = TRUE,
    temporary = TRUE
  )
  summarize <- function(source, selection) {
    summarize_with_margins(
      source,
      units = sum(units),
      dplyr::across({{ selection }}, share_of_total, .names = "{.col}_share"),
      .grouping = rollup(group),
      .margin_label = NULL
    )
  }

  expected <- summarize(data, units)
  result <- dplyr::collect(summarize(remote, units)) |>
    dplyr::arrange(group)

  expect_equal(as.data.frame(result), as.data.frame(expected))
  expect_equal(result$units_share, c(0.25, 0.75, 1))
})

test_that("dtplyr rejects ineligible Total-share sources on collection", {
  skip_if_suggest_absent("dtplyr")
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
  skip_if_suggest_absent("arrow")
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

test_that("dtplyr reports a share against a call holding its own input", {
  skip_if_suggest_absent("dtplyr")
  # `do.call()` records the evaluated arguments, so the call this verb captures
  # holds the `lazy_dt` itself. Its `.internal.selfref` externalptr is what
  # `deparse()` cannot write as source, and the text the share carries to
  # `collect()` used to be that unreadable deparse (#360).
  data <- data.frame(
    group = c("x", "x", "y"),
    value = 1:3
  )

  query <- do.call(summarize_with_margins, list(
    dtplyr::lazy_dt(data),
    source = quote(range(value)),
    share = quote(share_of_total(source)),
    .grouping = rollup(group),
    .margin_label = NULL
  ))

  expect_s3_class(query, "dtplyr_step")
  error <- expect_error(
    dplyr::collect(query),
    "exactly one value per grouping row"
  )
  expect_s3_class(error, "marginplyr_share_cardinality_error")
  expect_s3_class(error, "marginplyr_error")
  expect_identical(error$share_output, "share")
  expect_identical(error$source_summary, "source")

  # The input is the one part replaced. Named by its class rather than by the
  # spelling of that class, which is dtplyr's to change.
  input <- conditionCall(error)[[2L]]
  expect_true(rlang::is_symbol(input))
  expect_match(as.character(input), "^<.+>$")

  # Every other argument is still the caller's own, `NULL` included: it is a
  # part `deparse()` writes, so the walk has nothing to answer it with.
  expect_identical(conditionCall(error)$source, quote(range(value)))
  expect_identical(conditionCall(error)$share, quote(share_of_total(source)))
  expect_null(conditionCall(error)$.margin_label)
  expect_true(".margin_label" %in% names(as.list(conditionCall(error))))
})

test_that("dtplyr reports an across share against the same call", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(
    group = c("x", "x", "y"),
    value = 1:3
  )

  query <- do.call(summarize_with_margins, list(
    dtplyr::lazy_dt(data),
    quote(dplyr::across(value, list(flag = ~ any(.x > 0)))),
    flag_share = quote(share_of_parent(value_flag)),
    .grouping = rollup(group),
    .margin_label = NULL
  ))

  expect_s3_class(query, "dtplyr_step")
  error <- expect_error(
    dplyr::collect(query),
    "plain integer or double scalar"
  )
  expect_s3_class(error, "marginplyr_error")
  expect_identical(error$share_output, "flag_share")
  expect_identical(error$source_summary, "value_flag")

  input <- conditionCall(error)[[2L]]
  expect_true(rlang::is_symbol(input))
  expect_match(as.character(input), "^<.+>$")
  expect_identical(
    conditionCall(error)$flag_share,
    quote(share_of_parent(value_flag))
  )
})
