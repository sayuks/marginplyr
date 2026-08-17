test_that("expand preserves fixed keys, types, and column order", {
  data <- data.frame(
    value = 1:3,
    group = ordered(c("b", "a", "b"), levels = c("a", "b")),
    fixed = c(2L, 1L, 1L)
  )

  result <- expand_with_margins(
    data,
    .by = fixed,
    .grouping = rollup(group),
    .margin_label = NULL
  )
  result <- dplyr::arrange(result, fixed, group, value)

  expect_identical(names(result), c("fixed", "group", "value"))
  expect_identical(dplyr::group_vars(result), character())
  expect_s3_class(result$group, "ordered")
  expect_identical(levels(result$group), c("a", "b"))
  expect_identical(result$fixed, c(1L, 1L, 1L, 1L, 2L, 2L))
  expect_identical(as.character(result$group), c("a", "b", NA, NA, "b", NA))
  expect_identical(result$value, c(2L, 3L, 2L, 3L, 1L, 1L))
})

expand_proxy_capture <- new.env(parent = emptyenv())

expand_proxy_counter_head <- function(x, ...) {
  result <- NextMethod()
  class(result) <- unique(c("margin_expand_proxy_counter", class(result)))
  result
}

expand_proxy_counter_collect <- function(x, ...) {
  expand_proxy_capture$n <- expand_proxy_capture$n + 1L
  NextMethod()
}

register_expand_proxy_methods <- function() {
  registerS3method(
    "head",
    "margin_expand_proxy_counter",
    expand_proxy_counter_head,
    envir = asNamespace("utils")
  )
  registerS3method(
    "collect",
    "margin_expand_proxy_counter",
    expand_proxy_counter_collect,
    envir = asNamespace("dplyr")
  )
}

test_that("expand rejects invalid grouping before typed metadata acquisition", {
  skip_if_suggest_absent("dtplyr")
  register_expand_proxy_methods()
  source <- dtplyr::lazy_dt(data.frame(group = c("x", "y"), value = 1:2))
  class(source) <- c("margin_expand_proxy_counter", class(source))
  expand_proxy_capture$n <- 0L

  error <- expect_error(
    expand_with_margins(source, .grouping = 1),
    "must be created with"
  )

  expect_identical(expand_proxy_capture$n, 0L)
  expect_match(
    deparse1(conditionCall(error)),
    "expand_with_margins",
    fixed = TRUE
  )

  expand_proxy_capture$n <- 0L
  expect_error(
    expand_with_margins(
      source,
      .grouping = rollup(grouping_sets(grouping_set(group)))
    ),
    "only accepts columns or `grouping_set\\(\\)`"
  )
  expect_identical(expand_proxy_capture$n, 0L)

  expand_proxy_capture$n <- 0L
  expect_error(
    expand_with_margins(source, .grouping = rollup(unknown)),
    "Column `unknown` doesn't exist"
  )
  expect_identical(expand_proxy_capture$n, 0L)

  expand_proxy_capture$n <- 0L
  expect_error(
    expand_with_margins(
      source,
      .grouping = rollup(dplyr::all_of("unknown"))
    ),
    "Element `unknown` doesn't exist"
  )
  expect_identical(expand_proxy_capture$n, 0L)

  expand_proxy_capture$n <- 0L
  expect_error(
    expand_with_margins(
      source,
      .grouping = rollup(dplyr::any_of("unknown"))
    ),
    "requires at least one dimension"
  )
  expect_identical(expand_proxy_capture$n, 0L)

  expand_proxy_capture$n <- 0L
  expect_error(
    expand_with_margins(
      source,
      .by = group,
      .grouping = rollup(group)
    ),
    "both `.by` and `.grouping`"
  )
  expect_identical(expand_proxy_capture$n, 0L)

  expand_proxy_capture$n <- 0L
  expect_error(
    expand_with_margins(
      source,
      .grouping = grouping_sets(
        grouping_set(group),
        grouping_set(group)
      )
    ),
    "Duplicate grouping sets"
  )
  expect_identical(expand_proxy_capture$n, 0L)
})

test_that("dtplyr expansion acquires typed metadata once and stays lazy", {
  skip_if_suggest_absent("dtplyr")
  register_expand_proxy_methods()
  source <- dtplyr::lazy_dt(data.frame(
    group = c("x", "y"),
    code = c(1L, 2L),
    value = c(10, 20)
  ))
  class(source) <- c("margin_expand_proxy_counter", class(source))
  expand_proxy_capture$n <- 0L

  query <- expand_with_margins(
    source,
    .grouping = rollup(where(is.numeric)),
    .margin_label = NULL
  )

  expect_s3_class(query, "dtplyr_step")
  expect_identical(expand_proxy_capture$n, 1L)
  result <- dplyr::collect(query)
  expect_identical(names(result), c("code", "value", "group"))
  expect_identical(nrow(result), 6L)
})

test_that("Arrow expansion uses schema metadata without collecting", {
  skip_if_suggest_absent("arrow")
  register_expand_proxy_methods()
  source <- arrow::Table$create(data.frame(
    group = c("x", "y"),
    value = c(1L, 2L)
  )) |>
    dplyr::mutate(doubled = value * 2L)
  class(source) <- c("margin_expand_proxy_counter", class(source))
  expand_proxy_capture$n <- 0L

  query <- expand_with_margins(
    source,
    .grouping = rollup(where(is.character)),
    .margin_label = NULL
  )

  expect_identical(expand_proxy_capture$n, 0L)
  result <- dplyr::collect(query)
  expect_identical(nrow(result), 4L)
  expect_true(anyNA(result$group))
  expect_identical(sort(result$doubled), c(2L, 2L, 4L, 4L))
})

test_that("DuckDB expansion acquires one typed selection proxy", {
  skip_if_suggest_absent("duckdb", "DBI")
  register_expand_proxy_methods()
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  source <- dplyr::copy_to(
    con,
    data.frame(
      group = c("x", "y"),
      code = c(1L, 2L),
      value = c(10, 20)
    ),
    "expand_proxy_data",
    overwrite = TRUE,
    temporary = TRUE
  )
  class(source) <- c("margin_expand_proxy_counter", class(source))
  expand_proxy_capture$n <- 0L

  query <- expand_with_margins(
    source,
    .grouping = rollup(where(is.numeric)),
    .margin_label = NULL
  )

  expect_s3_class(query, "tbl_lazy")
  expect_identical(expand_proxy_capture$n, 1L)
  result <- dplyr::collect(query)
  expect_identical(names(result), c("code", "value", "group"))
  expect_identical(nrow(result), 6L)
})

test_that("portable SQL expansion stays lazy and uses UNION ALL", {
  skip_if_no_sqlite_simulation()
  remote <- dbplyr::tbl_lazy(
    data.frame(
      check.names = FALSE,
      "fixed key" = c(1L, 1L),
      "group name" = c("x", "y"),
      value = 1:2,
      ..marginplyr_key_1 = 3:4
    ),
    con = dbplyr::simulate_sqlite()
  )

  query <- expand_with_margins(
    remote,
    .by = `fixed key`,
    .grouping = rollup(`group name`),
    .margin_label = "Director's total"
  )
  sql <- dbplyr::sql_render(query)

  expect_s3_class(query, "tbl_lazy")
  expect_match(sql, "UNION ALL", fixed = TRUE)
  expect_match(sql, "`fixed key`", fixed = TRUE)
  expect_match(sql, "`group name`", fixed = TRUE)
  expect_match(sql, "'Director''s total'", fixed = TRUE)
  expect_no_match(sql, "ORDER BY", fixed = TRUE)
  expect_identical(
    as.character(dplyr::tbl_vars(query)),
    c("fixed key", "group name", "value", "..marginplyr_key_1")
  )
})

test_that("expand preserves duplicate grouping-set policies", {
  data <- data.frame(group = c("x", "y"), value = 1:2)
  spec <- grouping_sets(grouping_set(group), grouping_set(group))

  expect_error(
    expand_with_margins(data, .grouping = spec),
    "Duplicate grouping sets"
  )
  dropped <- expand_with_margins(
    data,
    .grouping = spec,
    .duplicates = "drop"
  )
  kept <- expand_with_margins(
    data,
    .grouping = spec,
    .duplicates = "keep"
  )

  expect_identical(nrow(dropped), 2L)
  expect_identical(nrow(kept), 4L)
  expect_identical(names(kept), names(data))
})

test_that("expand rejects unsupported sources with a package condition", {
  skip_if_suggest_absent("arrow")

  error <- expect_error(
    expand_with_margins(
      arrow::as_record_batch_reader(data.frame(group = c("x", "y"))),
      .grouping = rollup(group)
    ),
    "`\\.data` must not be an object of the following classes"
  )

  expect_s3_class(error, "marginplyr_error")
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "expand_with_margins"
  )
})
