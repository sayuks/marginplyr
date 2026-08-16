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
    "Column `unknown` doesn't exist"
  )

  expect_identical(summary_label_check_capture$n, 0L)
})

test_that("invalid summary selections precede label name coverage errors", {
  data <- data.frame(first = "x", second = "y", value = 1L)

  expect_error(
    summarize_with_margins(
      data,
      dplyr::across(unknown, sum),
      .grouping = rollup(first, second),
      .margin_label = c(first = "All first")
    ),
    "Column `unknown` doesn't exist"
  )
})

test_that("shared lifecycle options use package conditions", {
  data <- data.frame(group = "x")
  cases <- list(
    check_margin_label = list(
      expr = quote(summarize_with_margins(
        data,
        id = dplyr::cur_group_id(),
        .grouping = rollup(group),
        .check_margin_label = 1
      )),
      message = "`\\.check_margin_label` must be a logical scalar"
    ),
    check_margin_label_null = list(
      expr = quote(summarize_with_margins(
        data,
        id = dplyr::cur_group_id(),
        .grouping = rollup(group),
        .check_margin_label = NULL
      )),
      message = "`\\.check_margin_label` must be a logical scalar"
    ),
    margin_label_position = list(
      expr = quote(summarize_with_margins(
        data,
        n = dplyr::n(),
        .margin_label_position = "middle"
      )),
      message = "`\\.margin_label_position` must be one of"
    ),
    duplicates = list(
      expr = quote(summarize_with_margins(
        data,
        n = dplyr::n(),
        .duplicates = "merge"
      )),
      message = "`\\.duplicates` must be one of"
    )
  )

  for (case in cases) {
    error <- expect_error(eval(case$expr), case$message)
    expect_s3_class(error, "marginplyr_error")
    expect_identical(
      rlang::call_name(conditionCall(error)),
      "summarize_with_margins"
    )
  }
})

test_that("shared lifecycle options preserve user-expression conditions", {
  data <- data.frame(group = "x")
  user_option <- function() {
    rlang::abort(
      "User option evaluation failed.",
      class = "marginplyr_test_user_option_error",
      provenance = "user option"
    )
  }
  baseline <- expect_error(user_option())

  error <- expect_error(
    summarize_with_margins(
      data,
      n = dplyr::n(),
      .duplicates = user_option()
    )
  )

  expect_identical(class(error), class(baseline))
  expect_identical(error$provenance, "user option")
  expect_false(inherits(error, "marginplyr_error"))
})

test_that("summary rejects grouping before typed metadata acquisition", {
  skip_if_backend_absent("dtplyr")
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
  expect_s3_class(error, "vctrs_error_subscript_oob")
  expect_false(inherits(error, "marginplyr_error"))
})

test_that("removed .groups is rejected before typed metadata acquisition", {
  skip_if_backend_absent("dtplyr")
  register_summary_proxy_methods()
  source <- dtplyr::lazy_dt(data.frame(group = c("x", "y"), value = 1:2))
  class(source) <- c("margin_summary_proxy_counter", class(source))
  summary_proxy_capture$n <- 0L
  summary_options <- list(.groups = "drop")

  expect_error(
    summarize_with_margins(
      source,
      n = dplyr::n(),
      .grouping = rollup(group),
      !!!summary_options
    ),
    "`summarize_with_margins\\(\\)` has no `\\.groups` argument"
  )

  expect_identical(summary_proxy_capture$n, 0L)
})

test_that("dtplyr summary reuses one typed snapshot across selections", {
  skip_if_backend_absent("dtplyr")
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
    .margin_label = NULL
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

test_that("dtplyr unwraps a `.fns` list of one into the function it holds", {
  skip_if_backend_absent("dtplyr")
  # dtplyr is the only backend whose `across()` output names are normalized
  # before staging: the `.names` template is expanded into the selection, and
  # a `.fns` list holding one function is unwrapped to that function, since a
  # list would otherwise name the outputs a second time. The rebuild reads
  # that list off the parse rather than out of the argument list it is
  # rewriting (#174), so the unwrapping is asserted here. Compared against the
  # local result, which needs no optional backend.
  data <- data.frame(
    group = c("x", "x", "y"),
    units = c(1, 3, 6),
    revenue = c(2, 4, 8)
  )
  summarize <- function(source) {
    summarize_with_margins(
      source,
      dplyr::across(c(units, revenue), list(sum), .names = "{.col}_total"),
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
  # The template named each output once, rather than the list naming it again.
  expect_named(expected, c("group", "units_total", "revenue_total"))
})

test_that("summary selection errors use the package condition seam", {
  data <- data.frame(group = c("x", "y"), value = 1:2)
  summary_options <- list(.groups = "drop")
  cases <- list(
    removed_groups = list(
      expr = rlang::expr(summarize_with_margins(
        data,
        n = dplyr::n(),
        .grouping = rollup(group),
        !!!summary_options
      )),
      message = "`summarize_with_margins\\(\\)` has no `\\.groups` argument"
    ),
    context_helper = list(
      expr = rlang::expr(summarize_with_margins(
        data,
        id = dplyr::cur_group_id(),
        .grouping = rollup(group)
      )),
      message = "does not support `cur_group_id\\(\\)`"
    ),
    group_overwrite = list(
      expr = rlang::expr(summarize_with_margins(
        data,
        group = sum(value),
        .grouping = rollup(group)
      )),
      message = "cannot overwrite grouping column `group`"
    )
  )

  for (case in cases) {
    error <- expect_error(eval(case$expr), case$message)
    expect_s3_class(error, "marginplyr_error")
    expect_identical(
      rlang::call_name(conditionCall(error)),
      "summarize_with_margins"
    )
  }
})

test_that("summary tidyselect conditions retain their class and cause", {
  data <- data.frame(group = c("x", "y"), value = 1:2)
  baseline <- expect_error(
    tidyselect::eval_select(rlang::quo(unknown), data = data["value"])
  )

  error <- expect_error(
    summarize_with_margins(
      data,
      dplyr::across(unknown, sum),
      .grouping = rollup(group)
    )
  )

  expect_identical(class(error), class(baseline))
  expect_false(inherits(error, "marginplyr_error"))
  expect_match(conditionMessage(error), "Column `unknown` doesn't exist")
})
