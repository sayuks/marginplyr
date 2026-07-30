test_that("summary exposes one-based Grouping set occurrence identifiers", {
  data <- data.frame(
    fixed = c(1L, 1L),
    group = c("x", "y"),
    value = c(10, 20)
  )

  result <- summarize_with_margins(
    data,
    total = sum(value),
    .by = fixed,
    .grouping = grouping_set(group),
    .id = "set"
  )

  expect_identical(names(result), c("fixed", "group", "set", "total"))
  expect_identical(result$set, c(1L, 1L))
})

test_that("all Margin verbs validate .id consistently", {
  data <- data.frame(group = "x", value = 1L)
  operations <- list(
    summary = function(id) {
      summarize_with_margins(data, n = dplyr::n(), .id = id)
    },
    expand = function(id) expand_with_margins(data, .id = id),
    nest = function(id) nest_with_margins(data, .id = id),
    nest_by = function(id) nest_by_with_margins(data, .id = id)
  )
  invalid_ids <- list(1, NA_character_, "", c("first", "second"))

  for (operation in operations) {
    for (id in invalid_ids) {
      expect_error(
        operation(id),
        paste0(
          "`\\.id` must be `NULL` or one non-missing, non-empty ",
          "character string"
        )
      )
    }
  }
})

test_that("expand identifies retained Grouping set occurrences", {
  data <- data.frame(group = c("x", "y"), value = 1:2)
  spec <- grouping_sets(grouping_set(group), grouping_set(group))

  dropped <- expand_with_margins(
    data,
    .grouping = spec,
    .duplicates = "drop",
    .id = "set"
  )
  kept <- expand_with_margins(
    data,
    .grouping = spec,
    .duplicates = "keep",
    .id = "set"
  )

  expect_identical(names(kept), c("group", "set", "value"))
  expect_identical(dropped$set, c(1L, 1L))
  expect_identical(kept$set, c(1L, 1L, 2L, 2L))
})

test_that("nesting keeps .id as an outer key", {
  data <- data.frame(group = c("x", "y"), value = 1:2)

  nested <- nest_with_margins(
    data,
    .grouping = rollup(group),
    .id = "set",
    .keep = TRUE
  ) |>
    dplyr::arrange(set, group)
  nested_by <- nest_by_with_margins(
    data,
    .grouping = rollup(group),
    .id = "set"
  )

  expect_identical(names(nested), c("group", "set", "data"))
  expect_identical(nested$set, c(1L, 1L, 2L))
  contains_id <- vapply(
    nested$data,
    function(x) "set" %in% names(x),
    logical(1)
  )
  expect_false(any(contains_id))
  expect_s3_class(nested_by, "rowwise_df")
  expect_identical(dplyr::group_vars(nested_by), c("group", "set"))
})

test_that("summary identifiers follow duplicate policy", {
  data <- data.frame(group = c("x", "y"), value = 1:2)
  spec <- grouping_sets(grouping_set(group), grouping_set(group))

  dropped <- summarize_with_margins(
    data,
    n = dplyr::n(),
    .grouping = spec,
    .duplicates = "drop",
    .id = "set"
  )
  kept <- summarize_with_margins(
    data,
    n = dplyr::n(),
    .grouping = spec,
    .duplicates = "keep",
    .id = "set"
  ) |>
    dplyr::arrange(set, group)

  expect_identical(dropped$set, c(1L, 1L))
  expect_identical(kept$set, c(1L, 1L, 2L, 2L))
})

test_that("zero-row results retain an integer .id column", {
  data <- data.frame(group = character(), value = integer())
  results <- list(
    summarize_with_margins(
      data,
      n = dplyr::n(),
      .grouping = grouping_set(group),
      .id = "set"
    ),
    expand_with_margins(
      data,
      .grouping = grouping_set(group),
      .id = "set"
    ),
    nest_with_margins(
      data,
      .grouping = grouping_set(group),
      .id = "set"
    ),
    nest_by_with_margins(
      data,
      .grouping = grouping_set(group),
      .id = "set"
    )
  )

  for (result in results) {
    expect_identical(nrow(result), 0L)
    expect_identical(result$set, integer())
  }
})

test_that("empty ungrouped nest_by retains its sole occurrence identifier", {
  data <- data.frame(group = character(), value = integer())

  result <- nest_by_with_margins(data, .id = "set")

  expect_identical(names(result), c("set", "data"))
  expect_identical(result$set, 1L)
  expect_identical(dplyr::group_vars(result), "set")
  expect_identical(names(result$data[[1L]]), names(data))
  expect_identical(nrow(result$data[[1L]]), 0L)
})

test_that(".id rejects output-name collisions", {
  data <- data.frame(group = c("x", "y"), value = 1:2)
  operations <- list(
    summary = function() {
      summarize_with_margins(data, n = dplyr::n(), .id = "group")
    },
    expand = function() expand_with_margins(data, .id = "group"),
    nest = function() nest_with_margins(data, .id = "group"),
    nest_by = function() nest_by_with_margins(data, .id = "group")
  )

  for (operation in operations) {
    expect_error(operation(), "`\\.id` \\(`group`\\) conflicts")
  }
  expect_error(
    summarize_with_margins(data, set = dplyr::n(), .id = "set"),
    "`\\.id` \\(`set`\\) conflicts with a summary output"
  )
  expect_error(
    nest_with_margins(data, .id = "data"),
    "`\\.id` \\(`data`\\) conflicts with nesting `.key`"
  )
  expect_error(
    nest_by_with_margins(data, .id = "data"),
    "`\\.id` \\(`data`\\) conflicts with nesting `.key`"
  )
})

test_that(".id preserves ordinary unnamed summary expressions", {
  data <- data.frame(value = 1:2)

  result <- summarize_with_margins(data, sum(value), .id = "set")

  expect_identical(names(result), c("set", "sum(value)"))
  expect_identical(result$set, 1L)
  expect_identical(result[["sum(value)"]], 3L)
})

test_that("native summaries derive .id from structural grouping metadata", {
  remote <- dbplyr::tbl_lazy(
    data.frame(a = "x", b = "u", value = 1),
    con = dbplyr::simulate_postgres()
  )

  query <- summarize_with_margins(
    remote,
    n = dplyr::n(),
    .grouping = rollup(a, b),
    .id = "set"
  )
  sql <- dbplyr::sql_render(query)

  expect_match(sql, "GROUP BY GROUPING SETS", fixed = TRUE)
  expect_match(sql, "CASE WHEN", fixed = TRUE)
  expect_match(sql, "GROUPING(\"a\")", fixed = TRUE)
  expect_match(sql, "GROUPING(\"b\")", fixed = TRUE)
  expect_false(grepl("UNION ALL", sql, fixed = TRUE))
  expect_identical(
    as.character(dplyr::tbl_vars(query)),
    c("a", "b", "set", "n")
  )
})

test_that("nesting reserves internal names around a user .id", {
  data <- data.frame(group = c("x", "y"), value = 1:2)

  result <- nest_with_margins(
    data,
    .grouping = grouping_set(group),
    .id = "..marginplyr_nest_1",
    .keep = TRUE
  ) |>
    dplyr::arrange(group)

  expect_identical(result[["..marginplyr_nest_1"]], c(1L, 1L))
  expect_identical(result$data[[1L]]$group, "x")
  expect_identical(result$data[[2L]]$group, "y")
})

test_that(".id distinguishes source missing values from margins", {
  data <- data.frame(group = c(NA_character_, "x"), value = 1:2)

  summary <- summarize_with_margins(
    data,
    total = sum(value),
    .grouping = rollup(group),
    .margin_label = NULL,
    .id = "set"
  )
  expansion <- expand_with_margins(
    data,
    .grouping = rollup(group),
    .margin_label = NULL,
    .id = "set"
  )

  expect_setequal(summary$set[is.na(summary$group)], c(1L, 2L))
  expect_setequal(expansion$set[is.na(expansion$group)], c(1L, 2L, 2L))
})

test_that("non-syntactic .id names work across Margin verbs", {
  data <- data.frame(group = c("x", "y"), value = 1:2)
  results <- list(
    summarize_with_margins(data, n = dplyr::n(), .id = "set id"),
    expand_with_margins(data, .id = "set id"),
    nest_with_margins(data, .id = "set id"),
    nest_by_with_margins(data, .id = "set id")
  )

  for (result in results) {
    expect_true("set id" %in% names(result))
    expect_identical(unique(result[["set id"]]), 1L)
  }
})

test_that("DuckDB native and portable summaries agree on .id", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(group = c("x", "x", "y"), value = 1:3)
  remote <- dplyr::copy_to(
    con,
    data,
    "margin_id_data",
    overwrite = TRUE,
    temporary = TRUE
  )
  spec <- rollup(group)

  native_query <- summarize_with_margins(
    remote,
    total = sum(value),
    .grouping = spec,
    .id = "set"
  )
  expect_match(
    dbplyr::sql_render(native_query),
    "GROUP BY GROUPING SETS",
    fixed = TRUE
  )
  native <- native_query |>
    dplyr::collect() |>
    dplyr::arrange(set, group)

  plan <- compile_grouping_spec(spec, names(data))
  portable <- summarize_margin_union(
    remote,
    dots = rlang::quos(total = sum(value)),
    plan = plan,
    margin_labels = resolve_margin_labels(
      "Total",
      dimensions = plan$dimensions,
      by = plan$by
    ),
    column_info = margin_column_info(
      grouping_selection_proxy(remote),
      plan$dimensions,
      backend = grouping_backend(remote)
    ),
    reserved_names = c(names(data), "total", "set"),
    set_id_name = "set"
  ) |>
    dplyr::select(group, set, total) |>
    dplyr::collect() |>
    dplyr::arrange(set, group)

  expect_equal(native, portable)

  duplicate_query <- summarize_with_margins(
    remote,
    total = sum(value),
    .grouping = grouping_sets(
      grouping_set(group),
      grouping_set(group)
    ),
    .duplicates = "keep",
    .id = "set"
  )
  duplicate_sql <- dbplyr::sql_render(duplicate_query)
  expect_match(duplicate_sql, "UNION ALL", fixed = TRUE)
  expect_false(grepl("GROUPING SETS", duplicate_sql, fixed = TRUE))
  duplicate <- duplicate_query |>
    dplyr::collect() |>
    dplyr::arrange(set, group)
  expect_identical(duplicate$set, c(1L, 1L, 2L, 2L))
})

test_that("portable .id paths remain lazy", {
  remote <- dbplyr::tbl_lazy(
    data.frame(group = c("x", "y"), value = 1:2),
    con = dbplyr::simulate_sqlite()
  )

  summary <- summarize_with_margins(
    remote,
    n = dplyr::n(),
    .grouping = rollup(group),
    .id = "set"
  )
  expansion <- expand_with_margins(
    remote,
    .grouping = rollup(group),
    .id = "set"
  )

  expect_s3_class(summary, "tbl_lazy")
  expect_s3_class(expansion, "tbl_lazy")
  expect_match(dbplyr::sql_render(summary), "UNION ALL", fixed = TRUE)
  expect_match(dbplyr::sql_render(expansion), "UNION ALL", fixed = TRUE)
  expect_identical(
    as.character(dplyr::tbl_vars(summary)),
    c("group", "set", "n")
  )
  expect_identical(
    as.character(dplyr::tbl_vars(expansion)),
    c("group", "set", "value")
  )
})
