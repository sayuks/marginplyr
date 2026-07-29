summary_label_check_capture <- new.env(parent = emptyenv())

summary_label_check_collect <- function(con, sql, ...) {
  summary_label_check_capture$n <- summary_label_check_capture$n + 1L
  result <- data.frame(FALSE)
  names(result) <- attr(con, "check_names", exact = TRUE)
  result
}

summary_proxy_capture <- new.env(parent = emptyenv())

summary_proxy_counter_head <- function(x, ...) {
  result <- NextMethod()
  class(result) <- unique(c("margin_summary_proxy_counter", class(result)))
  result
}

summary_proxy_counter_collect <- function(x, ...) {
  summary_proxy_capture$n <- summary_proxy_capture$n + 1L
  NextMethod()
}

register_summary_proxy_methods <- function() {
  registerS3method(
    "head",
    "margin_summary_proxy_counter",
    summary_proxy_counter_head,
    envir = asNamespace("utils")
  )
  registerS3method(
    "collect",
    "margin_summary_proxy_counter",
    summary_proxy_counter_collect,
    envir = asNamespace("dplyr")
  )
}

test_that("invalid summary selections precede lazy label collision queries", {
  registerS3method(
    "db_collect",
    "summary_label_check_connection",
    summary_label_check_collect,
    envir = asNamespace("dbplyr")
  )
  con <- dbplyr::simulate_sqlite()
  class(con) <- append(
    class(con),
    "summary_label_check_connection",
    after = 1L
  )
  attr(con, "check_names") <- "group"
  remote <- dbplyr::tbl_lazy(
    data.frame(group = "x", value = 1),
    con = con
  )
  summary_label_check_capture$n <- 0L

  expect_error(
    summarize_with_margins(
      remote,
      dplyr::across(unknown, sum),
      .grouping = rollup(group),
      .check_margin_label = TRUE
    ),
    "Invalid column selection.*unknown"
  )

  expect_identical(summary_label_check_capture$n, 0L)
})

test_that("summary options are validated before context helpers", {
  expect_error(
    summarize_with_margins(
      data.frame(group = "x"),
      id = dplyr::cur_group_id(),
      .grouping = rollup(group),
      .check_margin_label = 1
    ),
    "`\\.check_margin_label` must be a logical scalar"
  )
})

test_that("summary rejects grouping before typed metadata acquisition", {
  skip_if_not_installed("dtplyr")
  register_summary_proxy_methods()
  source <- dtplyr::lazy_dt(data.frame(group = c("x", "y"), value = 1:2))
  class(source) <- c("margin_summary_proxy_counter", class(source))
  summary_proxy_capture$n <- 0L

  error <- expect_error(
    summarize_with_margins(
      source,
      n = dplyr::n(),
      .grouping = rollup(unknown)
    ),
    "Column `unknown` doesn't exist"
  )

  expect_identical(summary_proxy_capture$n, 0L)
  expect_match(
    deparse1(conditionCall(error)),
    "summarize_with_margins",
    fixed = TRUE
  )
})

test_that("dtplyr summary reuses one typed snapshot across selections", {
  skip_if_not_installed("dtplyr")
  register_summary_proxy_methods()
  source <- dtplyr::lazy_dt(data.frame(
    group = c("x", "y"),
    code = c(1L, 2L),
    value = c(10, 20)
  ))
  class(source) <- c("margin_summary_proxy_counter", class(source))
  summary_proxy_capture$n <- 0L

  query <- summarize_with_margins(
    source,
    dplyr::across(
      where(is.numeric),
      sum,
      .names = "total_{.col}"
    ),
    .grouping = rollup(where(is.character)),
    .margin_label = NULL,
    .sort = FALSE
  )

  expect_s3_class(query, "dtplyr_step")
  expect_identical(summary_proxy_capture$n, 1L)
  result <- dplyr::collect(query)
  expect_identical(
    names(result),
    c("group", "total_code", "total_value")
  )
  expect_setequal(result$total_value, c(10, 20, 30))
})
