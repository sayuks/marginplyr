test_that("inspect_grouping describes a rollup in Grouping plan order", {
  data <- data.frame(
    fixed = 1L,
    region = "East",
    store = "A"
  )

  result <- inspect_grouping(
    data,
    .by = fixed,
    .grouping = rollup(region, store)
  )

  expect_identical(
    result,
    tibble::tibble(
      set_id = 1:3,
      fixed = rep("(fixed)", 3L),
      included = c("(region, store)", "(region)", "()"),
      omitted = c("()", "(store)", "(region, store)"),
      grouping_bits = c(
        "region=0, store=0",
        "region=0, store=1",
        "region=1, store=1"
      ),
      grouping_id = c(0L, 1L, 3L)
    )
  )
  expect_identical(class(result), c("tbl_df", "tbl", "data.frame"))
  expect_identical(dplyr::group_vars(result), character())
})

test_that("list inspection preserves exact non-syntactic dimension names", {
  data <- data.frame(
    "region, west" = "West",
    "store=number" = "A",
    check.names = FALSE
  )

  result <- inspect_grouping(
    data,
    .grouping = rollup(`region, west`, `store=number`),
    .format = "list"
  )

  expect_identical(
    result$fixed,
    rep(list(character()), 3L)
  )
  expect_identical(
    result$included,
    list(
      c("region, west", "store=number"),
      "region, west",
      character()
    )
  )
  expect_identical(
    result$omitted,
    list(
      character(),
      "store=number",
      c("region, west", "store=number")
    )
  )
  expect_identical(
    result$grouping_bits,
    list(
      c("region, west" = 0L, "store=number" = 0L),
      c("region, west" = 0L, "store=number" = 1L),
      c("region, west" = 1L, "store=number" = 1L)
    )
  )
})

test_that("inspection preserves grouping bits at integer-mask boundaries", {
  empty_text <- inspect_grouping(data.frame(value = 1L))
  empty_list <- inspect_grouping(
    data.frame(value = 1L),
    .format = "list"
  )

  expect_identical(empty_text$included, "()")
  expect_identical(empty_text$omitted, "()")
  expect_identical(empty_text$grouping_bits, "()")
  expect_identical(empty_text$grouping_id, 0L)
  expect_identical(empty_list$included, list(character()))
  expect_identical(empty_list$omitted, list(character()))
  expect_identical(empty_list$grouping_bits, list(integer()))

  one <- data.frame(dimension = "x")
  one_result <- inspect_grouping(
    one,
    .grouping = rollup(dimension),
    .format = "list"
  )
  expect_identical(one_result$grouping_id, c(0L, 1L))
  expect_identical(
    one_result$grouping_bits,
    list(c(dimension = 0L), c(dimension = 1L))
  )

  names_31 <- paste0("dimension_", seq_len(31L))
  data_31 <- stats::setNames(
    as.data.frame(as.list(rep("x", 31L))),
    names_31
  )
  result_31 <- inspect_grouping(
    data_31,
    .grouping = rollup(dplyr::all_of(names_31)),
    .format = "list"
  )
  expect_identical(result_31$grouping_id[[1L]], 0L)
  expect_identical(result_31$grouping_id[[32L]], .Machine$integer.max)
  expect_length(result_31$grouping_bits[[32L]], 31L)

  names_32 <- paste0("dimension_", seq_len(32L))
  data_32 <- stats::setNames(
    as.data.frame(as.list(rep("x", 32L))),
    names_32
  )
  result_32 <- inspect_grouping(
    data_32,
    .grouping = rollup(dplyr::all_of(names_32)),
    .format = "list"
  )
  expect_identical(result_32$grouping_id, rep(NA_integer_, 33L))
  expect_length(result_32$grouping_bits[[33L]], 32L)
})

test_that("inspection applies duplicate policy before numbering occurrences", {
  data <- data.frame(
    first = "x",
    second = "y",
    value = 1L
  )
  specification <- grouping_sets(
    grouping_set(second),
    grouping_set(first, second),
    grouping_set(second),
    grouping_set()
  )

  expect_error(
    inspect_grouping(data, .grouping = specification),
    "Duplicate grouping sets"
  )

  dropped <- inspect_grouping(
    data,
    .grouping = specification,
    .duplicates = "drop",
    .format = "list"
  )
  kept <- inspect_grouping(
    data,
    .grouping = specification,
    .duplicates = "keep",
    .format = "list"
  )
  margin_result <- summarize_with_margins(
    data,
    n = dplyr::n(),
    .grouping = specification,
    .duplicates = "keep",
    .margin_label = NULL,
    .id = "occurrence"
  )

  expect_identical(dropped$set_id, 1:3)
  expect_identical(
    dropped$included,
    list("second", c("second", "first"), character())
  )
  expect_identical(kept$set_id, 1:4)
  expect_identical(
    kept$included,
    list(
      "second",
      c("second", "first"),
      "second",
      character()
    )
  )
  expect_identical(
    sort(margin_result$occurrence),
    kept$set_id
  )
})

test_that("inspection shares grouped and row-wise input validation", {
  data <- data.frame(
    fixed = factor("x", levels = c("x", "unused")),
    dimension = "y"
  )
  grouped <- dplyr::group_by(data, fixed)

  implicit <- inspect_grouping(
    grouped,
    .grouping = rollup(dimension)
  )
  explicit <- inspect_grouping(
    dplyr::ungroup(grouped),
    .by = fixed,
    .grouping = rollup(dimension)
  )

  expect_identical(implicit, explicit)
  expect_error(
    inspect_grouping(
      grouped,
      .by = fixed,
      .grouping = rollup(dimension)
    ),
    "Can't supply `.by`"
  )
  expect_error(
    inspect_grouping(dplyr::rowwise(data)),
    "`rowwise\\(\\)` input is not supported"
  )
  expect_error(
    inspect_grouping(dplyr::group_by(data, fixed, .drop = FALSE)),
    "`.drop = FALSE` is not supported"
  )
})

renaming_grouping_data <- function() {
  data.frame(region = c("x", "y"), revenue = 1:2)
}

renaming_grouping_message <- function() {
  paste0(
    "Can't rename grouping dimension `area = region`. ",
    "Grouping dimensions must name existing columns."
  )
}

test_that("inspection and execution refuse a renaming selection alike", {
  data <- renaming_grouping_data()

  inspected <- expect_error(
    inspect_grouping(
      data,
      .grouping = rollup(tidyselect::all_of(c(area = "region")))
    )
  )
  expect_s3_class(inspected, "marginplyr_error")
  expect_identical(conditionMessage(inspected), renaming_grouping_message())
  expect_identical(
    rlang::call_name(conditionCall(inspected)),
    "inspect_grouping"
  )

  # Every Margin verb resolves the same specification, so each one reports what
  # the inspection verb reported. The calls are written out rather than built
  # from the verb names, so `codetools` can follow them.
  summarized <- expect_error(
    summarize_with_margins(
      data,
      revenue = sum(revenue),
      .grouping = rollup(tidyselect::all_of(c(area = "region")))
    )
  )
  expect_identical(conditionMessage(summarized), renaming_grouping_message())

  expanded <- expect_error(
    expand_with_margins(
      data,
      .grouping = rollup(tidyselect::all_of(c(area = "region")))
    )
  )
  expect_identical(conditionMessage(expanded), renaming_grouping_message())

  nested <- expect_error(
    nest_with_margins(
      data,
      .grouping = rollup(tidyselect::all_of(c(area = "region")))
    )
  )
  expect_identical(conditionMessage(nested), renaming_grouping_message())

  nested_by <- expect_error(
    nest_by_with_margins(
      data,
      .grouping = rollup(tidyselect::all_of(c(area = "region")))
    )
  )
  expect_identical(conditionMessage(nested_by), renaming_grouping_message())

  for (error in list(summarized, expanded, nested, nested_by)) {
    expect_s3_class(error, "marginplyr_error")
  }
})

test_that("a renaming selection resolved from typed metadata is refused", {
  # A selection carrying a predicate cannot be resolved from column names
  # alone, so it reaches the typed selection proxy rather than the name proxy
  # that rejects every other renaming selection before a backend is read.
  error <- expect_error(
    inspect_grouping(
      renaming_grouping_data(),
      .grouping = rollup(c(area = region, where(is.numeric)))
    )
  )
  expect_s3_class(error, "marginplyr_error")
  expect_identical(conditionMessage(error), renaming_grouping_message())
})

inspect_proxy_capture <- new.env(parent = emptyenv())

inspect_proxy_counter_head <- function(x, ...) {
  result <- NextMethod()
  class(result) <- unique(c("margin_inspect_proxy_counter", class(result)))
  result
}

inspect_proxy_counter_collect <- function(x, ...) {
  inspect_proxy_capture$n <- inspect_proxy_capture$n + 1L
  NextMethod()
}

register_inspect_proxy_methods <- function() {
  registerS3method(
    "head",
    "margin_inspect_proxy_counter",
    inspect_proxy_counter_head,
    envir = asNamespace("utils")
  )
  registerS3method(
    "collect",
    "margin_inspect_proxy_counter",
    inspect_proxy_counter_collect,
    envir = asNamespace("dplyr")
  )
}

test_that("lazy inspection reads typed metadata once without executing margins", { # nolint: line_length_linter
  skip_if_backend_absent("dtplyr")
  register_inspect_proxy_methods()
  source <- dtplyr::lazy_dt(data.frame(
    group = c("Total", "x"),
    value = 1:2
  ))
  class(source) <- c("margin_inspect_proxy_counter", class(source))
  inspect_proxy_capture$n <- 0L

  result <- inspect_grouping(
    source,
    .grouping = rollup(where(is.character))
  )

  expect_identical(inspect_proxy_capture$n, 1L)
  expect_identical(class(result), c("tbl_df", "tbl", "data.frame"))
  expect_identical(result$included, c("(group)", "()"))
  expect_identical(result$grouping_id, c(0L, 1L))
})

test_that("dbplyr inspection returns local plan data without a source query", {
  source <- dbplyr::tbl_lazy(
    data.frame(group = "Total", value = 1L),
    con = dbplyr::simulate_sqlite()
  )

  result <- inspect_grouping(
    source,
    .grouping = rollup(group)
  )

  expect_identical(class(result), c("tbl_df", "tbl", "data.frame"))
  expect_identical(result$included, c("(group)", "()"))
})

test_that("inspect_grouping() options use the package condition seam", {
  data <- data.frame(group = c("x", "y"), value = 1:2)

  error <- expect_error(
    inspect_grouping(data, .grouping = rollup(group), .format = "json"),
    "`\\.format` must be one of"
  )

  expect_s3_class(error, "marginplyr_error")
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "inspect_grouping"
  )
})
