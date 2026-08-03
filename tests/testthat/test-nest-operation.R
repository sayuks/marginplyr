nest_proxy_capture <- new.env(parent = emptyenv())

nest_proxy_counter_head <- function(x, ...) {
  result <- NextMethod()
  class(result) <- unique(c("margin_nest_proxy_counter", class(result)))
  result
}

nest_proxy_counter_collect <- function(x, ...) {
  nest_proxy_capture$n <- nest_proxy_capture$n + 1L
  NextMethod()
}

register_nest_proxy_methods <- function() {
  registerS3method(
    "head",
    "margin_nest_proxy_counter",
    nest_proxy_counter_head,
    envir = asNamespace("utils")
  )
  registerS3method(
    "collect",
    "margin_nest_proxy_counter",
    nest_proxy_counter_collect,
    envir = asNamespace("dplyr")
  )
}

test_that("nest rejects grouping before typed metadata acquisition", {
  skip_if_backend_absent("dtplyr")
  register_nest_proxy_methods()
  source <- dtplyr::lazy_dt(data.frame(group = c("x", "y"), value = 1:2))
  class(source) <- c("margin_nest_proxy_counter", class(source))
  nest_proxy_capture$n <- 0L

  error <- expect_error(
    nest_with_margins(source, .grouping = rollup(unknown)),
    "Column `unknown` doesn't exist"
  )

  expect_identical(nest_proxy_capture$n, 0L)
  expect_s3_class(error, "vctrs_error_subscript_oob")
  expect_false(inherits(error, "marginplyr_error"))

  nest_proxy_capture$n <- 0L
  expect_error(
    nest_by_with_margins(source, .key = NULL),
    "`\\.key` must be a character vector of length 1"
  )
  expect_identical(nest_proxy_capture$n, 0L)

  nest_proxy_capture$n <- 0L
  expect_error(
    nest_with_margins(source, .keep = 1),
    "`\\.keep` must be a logical scalar"
  )
  expect_identical(nest_proxy_capture$n, 0L)

  nest_proxy_capture$n <- 0L
  expect_error(
    nest_with_margins(source, .duplicates = "keep"),
    "does not support `.duplicates = \"keep\"`"
  )
  expect_identical(nest_proxy_capture$n, 0L)
})

test_that("dtplyr nesting reuses one typed snapshot and stays lazy", {
  skip_if_backend_absent("dtplyr")
  register_nest_proxy_methods()
  source <- dtplyr::lazy_dt(data.frame(
    group = c("x", "y"),
    code = c(1L, 2L),
    value = c(10, 20)
  ))
  class(source) <- c("margin_nest_proxy_counter", class(source))
  nest_proxy_capture$n <- 0L

  query <- nest_with_margins(
    source,
    .grouping = rollup(where(is.character)),
    .margin_label = NULL,
    .keep = TRUE
  )

  expect_s3_class(query, "dtplyr_step")
  expect_identical(nest_proxy_capture$n, 1L)
  result <- dplyr::collect(query)
  expect_identical(names(result), c("group", "data"))
  expect_identical(
    names(result$data[[1L]]),
    c("group", "code", "value")
  )
})

test_that("nest verbs preserve their own quosure environments", {
  data <- data.frame(
    fixed = c(1L, 1L),
    group = c("x", "y"),
    value = c(10, 20)
  )

  nest_from_local_scope <- function(data) {
    fixed_cols <- "fixed"
    dimension_cols <- "group"
    nest_with_margins(
      data,
      .by = dplyr::all_of(fixed_cols),
      .grouping = rollup(dplyr::all_of(dimension_cols))
    )
  }
  nest_by_from_local_scope <- function(data) {
    fixed_cols <- "fixed"
    dimension_cols <- "group"
    nest_by_with_margins(
      data,
      .by = dplyr::all_of(fixed_cols),
      .grouping = rollup(dplyr::all_of(dimension_cols))
    )
  }

  nested <- nest_from_local_scope(data)
  nested_by <- nest_by_from_local_scope(data)

  expect_identical(names(nested), c("fixed", "group", "data"))
  expect_identical(dplyr::group_vars(nested), character())
  expect_s3_class(nested_by, "rowwise_df")
  expect_identical(dplyr::group_vars(nested_by), c("fixed", "group"))
})

test_that("nest preflight precedes semantic margin-label validation", {
  skip_if_backend_absent("dtplyr")
  register_nest_proxy_methods()
  source <- dtplyr::lazy_dt(data.frame(group = c("Total", "x"), value = 1:2))
  class(source) <- c("margin_nest_proxy_counter", class(source))
  nest_proxy_capture$n <- 0L

  expect_error(
    nest_with_margins(
      source,
      .grouping = rollup(group),
      .key = "group",
      .check_margin_label = TRUE
    ),
    "must not be a grouping column"
  )

  expect_identical(nest_proxy_capture$n, 1L)
})

test_that("nesting option errors use the package condition seam", {
  data <- data.frame(group = c("x", "y"), value = 1:2)
  cases <- list(
    keep = list(
      expr = quote(nest_with_margins(data, .grouping = rollup(group), .keep = 1)), # nolint: line_length_linter
      message = "`\\.keep` must be a logical scalar"
    ),
    key_type = list(
      expr = quote(nest_with_margins(data, .grouping = rollup(group), .key = 1)), # nolint: line_length_linter
      message = "`\\.key` must be a character vector of length 1"
    ),
    key_missing = list(
      expr = quote(nest_with_margins(
        data,
        .grouping = rollup(group),
        .key = NA_character_
      )),
      message = "`\\.key` must not be missing"
    ),
    key_empty = list(
      expr = quote(nest_with_margins(
        data,
        .grouping = rollup(group),
        .key = ""
      )),
      message = "`\\.key` must not be empty"
    ),
    duplicates_keep = list(
      expr = quote(nest_with_margins(
        data,
        .grouping = rollup(group),
        .duplicates = "keep"
      )),
      message = "Nesting does not support `\\.duplicates = \"keep\"`"
    ),
    key_grouping_column = list(
      expr = quote(nest_with_margins(
        data,
        .grouping = rollup(group),
        .key = "group"
      )),
      message = "`\\.key` \\(`group`\\) must not be a grouping column"
    )
  )

  for (case in cases) {
    error <- expect_error(eval(case$expr), case$message)
    expect_s3_class(error, "marginplyr_error")
    expect_identical(
      rlang::call_name(conditionCall(error)),
      "nest_with_margins"
    )
  }
})

test_that("nesting drops duplicate grouping sets", {
  input <- data.frame(group = c("x", "y"), value = 1:2)
  # `rollup(group)` contributes `{group}` and `{}`; the extra `grouping_set()`
  # repeats `{group}`. Dropping must remove only that repeat and leave the
  # distinct total in place.
  spec <- grouping_sets(rollup(group), grouping_set(group))
  verbs <- list(
    nest_with_margins = nest_with_margins,
    nest_by_with_margins = nest_by_with_margins
  )
  dropped <- list()

  for (verb_name in names(verbs)) {
    verb <- verbs[[verb_name]]

    expect_error(verb(input, .grouping = spec), "Duplicate grouping sets")

    # Result row order is unspecified, so sort before comparing positions.
    result <- verb(
      input,
      .grouping = spec,
      .duplicates = "drop",
      .id = "set"
    ) |>
      dplyr::arrange(set, group)

    expect_identical(names(result), c("group", "set", "data"))
    expect_identical(result$group, c("x", "y", "Total"))
    expect_identical(result$set, c(1L, 1L, 2L))
    expect_identical(vapply(result$data, nrow, integer(1)), c(1L, 1L, 2L))
    expect_identical(names(result$data[[1L]]), "value")
    expect_setequal(result$data[[3L]]$value, 1:2)

    dropped[[verb_name]] <- result
  }

  expect_identical(dplyr::group_vars(dropped$nest_with_margins), character())

  # The row-wise return shape is what makes per-margin summaries work.
  by_result <- dropped$nest_by_with_margins
  expect_s3_class(by_result, "rowwise_df")
  expect_identical(dplyr::group_vars(by_result), c("group", "set"))
  expect_identical(dplyr::mutate(by_result, n = nrow(data))$n, c(1L, 1L, 2L))
})

test_that("nesting rejects unsupported sources with a package condition", {
  remote <- dbplyr::tbl_lazy(
    data.frame(group = c("x", "y"), value = 1:2),
    con = dbplyr::simulate_postgres()
  )

  error <- expect_error(
    nest_with_margins(remote, .grouping = rollup(group)),
    "`\\.data` must be one of the following classes"
  )

  expect_s3_class(error, "marginplyr_error")
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "nest_with_margins"
  )
})
