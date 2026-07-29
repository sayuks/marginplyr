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
  skip_if_not_installed("dtplyr")
  register_nest_proxy_methods()
  source <- dtplyr::lazy_dt(data.frame(group = c("x", "y"), value = 1:2))
  class(source) <- c("margin_nest_proxy_counter", class(source))
  nest_proxy_capture$n <- 0L

  error <- expect_error(
    nest_with_margins(source, .grouping = rollup(unknown)),
    "Column `unknown` doesn't exist"
  )

  expect_identical(nest_proxy_capture$n, 0L)
  expect_match(
    deparse1(conditionCall(error)),
    "nest_with_margins",
    fixed = TRUE
  )

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
  skip_if_not_installed("dtplyr")
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
    .keep = TRUE,
    .sort = FALSE
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
      .grouping = rollup(dplyr::all_of(dimension_cols)),
      .sort = FALSE
    )
  }
  nest_by_from_local_scope <- function(data) {
    fixed_cols <- "fixed"
    dimension_cols <- "group"
    nest_by_with_margins(
      data,
      .by = dplyr::all_of(fixed_cols),
      .grouping = rollup(dplyr::all_of(dimension_cols)),
      .sort = FALSE
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
  skip_if_not_installed("dtplyr")
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
