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
    "`\\.duplicates` must be one of \"error\", \"drop\"\\."
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
      expr = quote(nest_with_margins(
        data,
        .grouping = rollup(group),
        .keep = 1
      )),
      message = "`\\.keep` must be a logical scalar"
    ),
    key_type = list(
      expr = quote(nest_with_margins(
        data,
        .grouping = rollup(group),
        .key = 1
      )),
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
      # `"keep"` is refused by the nesting vocabulary itself rather than by a
      # second guard, so it reads like any other value outside it (#110).
      # test-verb-argument-admission.R asserts the whole message; what this
      # case adds is that it reaches the caller through the condition seam.
      message = "`\\.duplicates` must be one of \"error\", \"drop\"\\."
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

# A nesting whose payload has no columns still stands for a known number of
# source rows, and the count is the only thing left to carry. Every case below
# pins it, because a cell that lost it is still a data frame and still nests
# under the right key — the loss shows up only as a row count no source row
# produced.
keys_only_sales <- function() {
  data.frame(
    region = c("East", "East", "West"),
    store = c("A", "A", "B"),
    stringsAsFactors = FALSE
  )
}

# The same keys carrying one payload column, so the branch that keeps
# `pick()` is exercised beside the branch that counts rows.
payload_sales <- function() {
  cbind(keys_only_sales(), units = c(10L, 20L, 30L))
}

# Sorted by the outer keys rather than by `.sort`, so the expectation reads the
# same for both verbs and both backends whatever Margin order they return.
keys_only_expected <- function() {
  list(
    region = c("East", "East", "Total", "West", "West"),
    store = c("A", "Total", "Total", "B", "Total"),
    rows = c(2L, 2L, 3L, 1L, 1L)
  )
}

# Sorting by name rather than by symbol keeps the outer keys out of this
# file's global-variable surface, which the linter reads without a data mask.
arrange_outer_keys <- function(result) {
  dplyr::arrange(
    dplyr::ungroup(result),
    dplyr::across(dplyr::all_of(c("region", "store")))
  )
}

nested_cell_dims <- function(result) {
  ordered <- arrange_outer_keys(result)
  list(
    region = ordered$region,
    store = ordered$store,
    rows = vapply(ordered$data, nrow, integer(1)),
    cols = vapply(ordered$data, ncol, integer(1)),
    names = lapply(ordered$data, names)
  )
}

test_that("nesting keeps source-row cardinality with no payload columns", {
  input <- keys_only_sales()
  expected <- keys_only_expected()
  verbs <- list(
    nest_with_margins = nest_with_margins,
    nest_by_with_margins = nest_by_with_margins
  )

  for (verb_name in names(verbs)) {
    result <- verbs[[verb_name]](
      input,
      .grouping = rollup(region, store),
      .sort = "last"
    )
    actual <- nested_cell_dims(result)

    expect_identical(actual$region, expected$region, info = verb_name)
    expect_identical(actual$store, expected$store, info = verb_name)
    # Repeated detail groups, the `East` subtotal, and the grand total each
    # nest every source row they stand for.
    expect_identical(actual$rows, expected$rows, info = verb_name)
    expect_identical(
      actual$cols,
      rep(0L, length(expected$rows)),
      info = verb_name
    )
    expect_identical(
      actual$names,
      rep(list(character()), length(expected$rows)),
      info = verb_name
    )

    # An internal column that survived into a cell would be invisible in the
    # counts above once it was the thing being counted.
    printed <- paste(utils::capture.output(print(result)), collapse = "\n")
    expect_false(grepl("marginplyr", printed, fixed = TRUE), info = verb_name)
  }
})

test_that("nesting keeps cardinality under both keep options", {
  input <- keys_only_sales()
  expected <- keys_only_expected()

  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    kept <- verb(
      input,
      .grouping = rollup(region, store),
      .sort = "last",
      .keep = TRUE
    )
    actual <- nested_cell_dims(kept)

    expect_identical(actual$rows, expected$rows)
    expect_identical(actual$cols, rep(2L, length(expected$rows)))
    expect_identical(
      actual$names,
      rep(list(c("region", "store")), length(expected$rows))
    )
    # `.keep = TRUE` nests pre-margin values, so the grand-total cell still
    # holds the source keys rather than the Margin label.
    total <- arrange_outer_keys(kept)$data[[3L]]
    expect_identical(
      sort(as.character(total$region)),
      c("East", "East", "West")
    )
  }
})

test_that("dtplyr nesting agrees with the local result and stays lazy", {
  skip_if_backend_absent("dtplyr")
  # Both inputs are needed: whether a payload column remains is what selects
  # the cell expression, and a keys-only input alone would pass however that
  # choice was made.
  inputs <- list(
    keys_only = keys_only_sales(),
    with_payload = payload_sales()
  )
  expected_names <- list(
    keys_only = list(character(), c("region", "store")),
    with_payload = list("units", c("region", "store", "units"))
  )

  for (input_name in names(inputs)) {
    input <- inputs[[input_name]]

    for (keep in c(FALSE, TRUE)) {
      query <- nest_with_margins(
        dtplyr::lazy_dt(input),
        .grouping = rollup(region, store),
        .sort = "last",
        .keep = keep
      )
      expect_s3_class(query, "dtplyr_step")

      local_result <- nest_with_margins(
        input,
        .grouping = rollup(region, store),
        .sort = "last",
        .keep = keep
      )
      lazy_result <- dplyr::collect(query)

      # Reading the cell names back separately, because comparing the two
      # backends only says they agree, not that either kept the payload.
      expect_identical(
        nested_cell_dims(lazy_result)$names[[1L]],
        expected_names[[input_name]][[keep + 1L]]
      )

      # The element class follows the backend and is not part of the API, so
      # compare the cells as tibbles; everything else must match exactly.
      as_cells <- function(result) {
        ordered <- arrange_outer_keys(result)
        ordered$data <- lapply(ordered$data, dplyr::as_tibble)
        dplyr::as_tibble(ordered)
      }
      expect_equal(as_cells(lazy_result), as_cells(local_result))

      by_result <- nest_by_with_margins(
        dtplyr::lazy_dt(input),
        .grouping = rollup(region, store),
        .sort = "last",
        .keep = keep
      )
      expect_identical(
        nested_cell_dims(by_result)$rows,
        nested_cell_dims(local_result)$rows
      )
      expect_identical(
        nested_cell_dims(by_result)$names[[1L]],
        expected_names[[input_name]][[keep + 1L]]
      )
    }
  }
})

test_that("zero-column and empty nesting match an independent construction", {
  # A frame with rows and no columns at all: every cell is a payload-free
  # cell, and `dplyr::nest_by()` is the upstream verb that answers it.
  rows_only <- data.frame(row.names = 1:3)
  expect_identical(dim(rows_only), c(3L, 0L))

  nested <- nest_with_margins(rows_only, .sort = "last")
  expect_identical(names(nested), "data")
  expect_identical(nrow(nested), 1L)
  expect_identical(dim(nested$data[[1L]]), c(3L, 0L))

  by_nested <- nest_by_with_margins(rows_only, .sort = "last")
  expect_identical(
    dim(by_nested$data[[1L]]),
    dim(dplyr::nest_by(rows_only)$data[[1L]])
  )

  # An empty input keeps `tidyr::nest()` semantics for one verb and
  # `dplyr::nest_by()` semantics for the other, with no columns to infer a
  # size from in either.
  empty <- data.frame()
  expect_identical(nrow(nest_with_margins(empty, .sort = "last")), 0L)

  by_empty <- nest_by_with_margins(empty, .sort = "last")
  expect_identical(nrow(by_empty), 1L)
  expect_identical(
    dim(by_empty$data[[1L]]),
    dim(dplyr::nest_by(empty)$data[[1L]])
  )

  # A keyed but rowless input nests nothing under either verb.
  keyed_empty <- data.frame(region = character())
  expect_identical(
    nrow(nest_with_margins(keyed_empty, .grouping = rollup(region))),
    0L
  )
  expect_identical(
    nrow(nest_by_with_margins(keyed_empty, .grouping = rollup(region))),
    0L
  )
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
