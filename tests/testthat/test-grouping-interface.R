test_that("rollup uses Total and exposes SQL-compatible grouping bits", {
  data <- data.frame(
    a = c("x", NA_character_),
    b = c("u", "u"),
    value = c(1, 2)
  )

  expect_no_message(
    result <- summarize_with_margins(
      data,
      n = dplyr::n(),
      ga = grouping_bit(a),
      gb = grouping_bit(b),
      gid = grouping_id(a, b),
      .grouping = rollup(a, b)
    )
  )
  result <- dplyr::arrange(result, gid, a, b)

  expect_equal(result$gid, c(0L, 0L, 1L, 1L, 3L))
  expect_equal(result$ga, c(0L, 0L, 0L, 0L, 1L))
  expect_equal(result$gb, c(0L, 0L, 1L, 1L, 1L))

  source_na <- dplyr::filter(result, is.na(a), gid == 0L)
  subtotal_na <- dplyr::filter(result, is.na(a), gid == 1L)
  grand_total <- dplyr::filter(result, gid == 3L)
  expect_equal(nrow(source_na), 1L)
  expect_equal(nrow(subtotal_na), 1L)
  expect_equal(grand_total$a, "Total")
  expect_equal(grand_total$b, "Total")
})

test_that("arbitrary and empty grouping sets match explicit summaries", {
  data <- data.frame(
    a = c("x", "x", "y"),
    b = c("u", "v", "u"),
    value = 1:3
  )

  result <- summarize_with_margins(
    data,
    total = sum(value),
    gid = grouping_id(a, b),
    .grouping = grouping_sets(
      grouping_set(a, b),
      grouping_set(a),
      grouping_set()
    )
  )
  result <- dplyr::arrange(result, gid, a, b)

  expect_equal(result$gid, c(0L, 0L, 0L, 1L, 1L, 3L))
  expect_equal(result$total, c(1, 2, 3, 3, 3, 6))
})

test_that("Cartesian products, nesting, and composite dimensions execute", {
  data <- expand.grid(
    country = c("JP", "US"),
    state = c("A", "B"),
    year = c(2024L, 2025L),
    stringsAsFactors = FALSE
  )

  composite <- summarize_with_margins(
    data,
    n = dplyr::n(),
    gid = grouping_id(country, state, year),
    .grouping = cube(grouping_set(country, state), year)
  )
  expect_setequal(unique(composite$gid), c(0L, 1L, 6L, 7L))
  expect_false(any(composite$gid %in% c(2L, 3L, 4L, 5L)))

  product <- summarize_with_margins(
    data,
    n = dplyr::n(),
    gid = grouping_id(country, state, year),
    .grouping = grouping_spec(rollup(country, state), cube(year))
  )
  expect_setequal(unique(product$gid), c(0L, 1L, 2L, 3L, 6L, 7L))

  nested <- summarize_with_margins(
    data,
    n = dplyr::n(),
    .grouping = grouping_sets(
      rollup(country, state),
      cube(year)
    ),
    .duplicates = "drop"
  )
  expect_true(nrow(nested) > 0L)
})

test_that("grouping grammar errors retain each public verb call", {
  data <- data.frame(a = c("x", "y"), value = 1:2)
  calls <- list(
    summarize_with_margins = quote(
      summarize_with_margins(
        data,
        n = dplyr::n(),
        .grouping = rollup(cube(a))
      )
    ),
    expand_with_margins = quote(
      expand_with_margins(data, .grouping = rollup(cube(a)))
    ),
    nest_with_margins = quote(
      nest_with_margins(data, .grouping = rollup(cube(a)))
    ),
    nest_by_with_margins = quote(
      nest_by_with_margins(data, .grouping = rollup(cube(a)))
    )
  )
  expected_message <- paste0(
    "`rollup()` only accepts columns or `grouping_set()` ",
    "composite dimensions."
  )

  for (verb in names(calls)) {
    error <- expect_error(eval(calls[[verb]]))
    expect_s3_class(error, "marginplyr_error")
    expect_identical(conditionMessage(error), expected_message)
    expect_identical(rlang::call_name(conditionCall(error)), verb)
  }
})

test_that("Grouping plan errors use the package condition seam", {
  data <- data.frame(a = c("x", "y"), value = 1:2)

  error <- expect_error(
    summarize_with_margins(
      data,
      n = dplyr::n(),
      .grouping = grouping_sets()
    ),
    "requires at least one set"
  )

  expect_s3_class(error, "marginplyr_error")
  expect_match(conditionMessage(error), "Use `grouping_set\\(\\)`")
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "summarize_with_margins"
  )
})

test_that("Grouping tidyselect conditions retain their class and cause", {
  data <- data.frame(a = c("x", "y"), value = 1:2)
  selection <- rlang::quo(unknown)
  baseline <- expect_error(
    tidyselect::eval_select(selection, data = data)
  )

  error <- expect_error(
    summarize_with_margins(
      data,
      n = dplyr::n(),
      .grouping = rollup(unknown)
    )
  )

  expect_identical(class(error), class(baseline))
  expect_false(inherits(error, "marginplyr_error"))
  expect_match(conditionMessage(error), "Column `unknown` doesn't exist")
})

test_that("a nested specification from a caller's function names its verb", {
  data <- data.frame(a = c("x", "y"), value = 1:2)
  spec_from_caller <- function(...) rollup(...)
  calls <- list(
    summarize_with_margins = quote(
      summarize_with_margins(
        data,
        n = dplyr::n(),
        .grouping = grouping_sets(spec_from_caller(a), a)
      )
    ),
    expand_with_margins = quote(
      expand_with_margins(
        data,
        .grouping = grouping_sets(spec_from_caller(a), a)
      )
    ),
    nest_with_margins = quote(
      nest_with_margins(
        data,
        .grouping = grouping_sets(spec_from_caller(a), a)
      )
    ),
    nest_by_with_margins = quote(
      nest_by_with_margins(
        data,
        .grouping = grouping_sets(spec_from_caller(a), a)
      )
    )
  )

  for (verb in names(calls)) {
    error <- expect_error(eval(calls[[verb]]))
    expect_s3_class(error, "marginplyr_error")
    expect_match(
      conditionMessage(error),
      paste0(
        "`spec_from_caller(a)` is a grouping specification, but a nested ",
        "position recognizes one only when it is a call to"
      ),
      fixed = TRUE
    )
    expect_identical(rlang::call_name(conditionCall(error)), verb)
  }

  # The workaround the diagnostic names executes, and it is the same plan the
  # constructor spelling builds.
  bound <- spec_from_caller(a)
  expect_equal(
    summarize_with_margins(
      data,
      n = dplyr::n(),
      .grouping = grouping_sets(bound, a),
      .duplicates = "drop"
    ),
    summarize_with_margins(
      data,
      n = dplyr::n(),
      .grouping = grouping_sets(rollup(a), a),
      .duplicates = "drop"
    )
  )
})

test_that("fixed .by columns are never replaced", {
  data <- data.frame(
    year = c(2024L, 2024L, 2025L),
    region = c("East", "West", "East")
  )

  result <- summarize_with_margins(
    data,
    n = dplyr::n(),
    gy = grouping_bit(year),
    gid = grouping_id(year, region),
    .by = year,
    .grouping = rollup(region)
  )

  expect_type(result$year, "integer")
  expect_false(any(result$year == "Total"))
  expect_true(all(result$gy == 0L))
  expect_setequal(unique(result$gid), c(0L, 1L))
})

test_that("grouping constructors resolve tidy-select predicates locally", {
  data <- data.frame(a = c("x", "y"), b = 1:2, value = c(3, 4))
  result <- summarize_with_margins(
    data,
    n = dplyr::n(),
    .grouping = rollup(where(is.character))
  )

  expect_equal(names(result), c("a", "n"))
  expect_true("Total" %in% result$a)
})

test_that("duplicate policies affect result cardinality", {
  data <- data.frame(a = c("x", "y"))
  spec <- grouping_sets(grouping_set(a), grouping_set(a))

  expect_error(
    summarize_with_margins(data, n = dplyr::n(), .grouping = spec),
    "Duplicate grouping sets"
  )
  dropped <- summarize_with_margins(
    data,
    n = dplyr::n(),
    .grouping = spec,
    .duplicates = "drop"
  )
  kept <- summarize_with_margins(
    data,
    n = dplyr::n(),
    .grouping = spec,
    .duplicates = "keep"
  )
  expect_equal(nrow(dropped), 2L)
  expect_equal(nrow(kept), 4L)
})

test_that("margin labels are display-only and can be disabled", {
  collision <- data.frame(a = c("Total", "x"), value = 1:2)
  expect_error(
    summarize_with_margins(
      collision,
      n = dplyr::n(),
      .grouping = rollup(a)
    ),
    "already present"
  )

  labelled <- summarize_with_margins(
    collision,
    n = dplyr::n(),
    g = grouping_bit(a),
    .grouping = rollup(a),
    .check_margin_label = FALSE
  )
  expect_equal(sum(labelled$a == "Total"), 2L)
  expect_setequal(labelled$g[labelled$a == "Total"], c(0L, 1L))

  typed <- summarize_with_margins(
    data.frame(
      a = 1:2,
      day = as.Date(c("2025-01-01", "2025-01-02")),
      moment = as.POSIXct(
        c("2025-01-01 01:00:00", "2025-01-02 02:00:00"),
        tz = "UTC"
      ),
      elapsed = as.difftime(c(1, 2), units = "hours")
    ),
    n = dplyr::n(),
    gid = grouping_id(a, day, moment, elapsed),
    .grouping = rollup(a, day, moment, elapsed),
    .margin_label = NULL
  )
  expect_type(typed$a, "integer")
  expect_s3_class(typed$day, "Date")
  expect_s3_class(typed$moment, c("POSIXct", "POSIXt"))
  expect_equal(attr(typed$moment, "tzone"), "UTC")
  expect_s3_class(typed$elapsed, "difftime")
  expect_equal(attr(typed$elapsed, "units"), "hours")
  expect_true(any(is.na(typed$a) & typed$gid == 15L))
})

test_that("all Margin verbs eagerly check local margin-label collisions", {
  data <- data.frame(group = c("Total", "x"), value = 1:2)
  operations <- list(
    summarize = function(data) {
      summarize_with_margins(
        data,
        n = dplyr::n(),
        .grouping = rollup(group)
      )
    },
    expand = function(data) {
      expand_with_margins(data, .grouping = rollup(group))
    },
    nest = function(data) {
      nest_with_margins(data, .grouping = rollup(group))
    },
    nest_by = function(data) {
      nest_by_with_margins(data, .grouping = rollup(group))
    }
  )

  for (operation in operations) {
    expect_error(operation(data), "already present")
  }
})

test_that("Margin verbs skip label checks for lazy inputs by default", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(group = c("Total", "x"), value = 1:2)
  source <- dtplyr::lazy_dt(data)

  expect_no_error(
    summarize_with_margins(
      source,
      n = dplyr::n(),
      .grouping = rollup(group)
    ) |>
      dplyr::collect()
  )
  expect_no_error(
    expand_with_margins(source, .grouping = rollup(group)) |>
      dplyr::collect()
  )
  expect_no_error(
    nest_with_margins(source, .grouping = rollup(group)) |>
      dplyr::collect()
  )
  expect_no_error(
    nest_by_with_margins(source, .grouping = rollup(group))
  )
})

test_that("margin label checks handle missing and non-syntactic columns", {
  missing <- data.frame(
    check.names = FALSE,
    "first group" = c(NA_character_, "x"),
    "second group" = c("y", NA_character_),
    value = 1:2
  )
  expect_error(
    summarize_with_margins(
      missing,
      n = dplyr::n(),
      .grouping = rollup(`first group`, `second group`),
      .margin_label = NA_character_
    ),
    "grouping columns `first group`, `second group`"
  )

  # A value of a factor column is a level of it, so this collision is declared
  # and is reported as one.
  factors <- data.frame(
    group = factor(c("Total", "x")),
    value = 1:2
  )
  expect_error(
    summarize_with_margins(
      factors,
      n = dplyr::n(),
      .grouping = rollup(group)
    ),
    "already a factor level"
  )

  empty <- missing[0, , drop = FALSE]
  expect_no_error(
    summarize_with_margins(
      empty,
      n = dplyr::n(),
      .grouping = rollup(`first group`, `second group`)
    )
  )
})

test_that("factor and ordered factor columns are reconstructed", {
  data <- data.frame(
    a = ordered(c("x", "y"), levels = c("x", "y")),
    b = factor(c("u", "v"))
  )
  result <- summarize_with_margins(
    data,
    n = dplyr::n(),
    .grouping = rollup(a, b)
  )

  expect_true(is.ordered(result$a))
  expect_true(is.factor(result$b))
  expect_equal(levels(result$a), c("x", "y", "Total"))
  expect_equal(levels(result$b), c("u", "v", "Total"))
})

test_that("grouping helpers validate their context and columns", {
  bit_context_error <- expect_error(grouping_bit(a), "only be used inside")
  id_context_error <- expect_error(grouping_id(a), "only be used inside")
  expect_s3_class(bit_context_error, "marginplyr_error")
  expect_s3_class(id_context_error, "marginplyr_error")

  data <- data.frame(a = 1, b = 1)
  column_error <- expect_error(
    summarize_with_margins(
      data,
      bad = grouping_bit(b),
      .grouping = rollup(a)
    ),
    "not part of"
  )
  expect_s3_class(column_error, "marginplyr_error")
  expect_identical(
    rlang::call_name(conditionCall(column_error)),
    "summarize_with_margins"
  )
  expect_error(
    summarize_with_margins(
      data,
      bad = grouping_id(a, a),
      .grouping = rollup(a)
    ),
    "duplicate columns"
  )

  qualified <- summarize_with_margins(
    data,
    bit = marginplyr::grouping_bit(a),
    .grouping = marginplyr::rollup(a)
  )
  expect_setequal(qualified$bit, c(0L, 1L))
})

test_that("expand and nest verbs consume the same grouping plan", {
  data <- data.frame(a = c("x", "x", "y"), b = c("u", "v", "u"), x = 1:3)

  expanded <- expand_with_margins(data, .grouping = rollup(a, b))
  expect_equal(nrow(expanded), 9L)
  expect_equal(sum(expanded$a == "Total"), 3L)

  expect_no_message(
    nested <- nest_with_margins(data, .grouping = rollup(a, b))
  )
  expect_equal(nrow(nested), 6L)
  expect_equal(names(nested), c("a", "b", "data"))
  expect_equal(names(nested$data[[1]]), "x")

  nested_keep <- nest_with_margins(
    data,
    .grouping = rollup(a, b),
    .keep = TRUE
  )
  expect_equal(names(nested_keep$data[[1]]), c("a", "b", "x"))
  subtotal <- nested_keep[nested_keep$a == "x" & nested_keep$b == "Total", ]
  expect_equal(subtotal$data[[1]]$a, c("x", "x"))
  expect_equal(subtotal$data[[1]]$b, c("u", "v"))

  grand_total <- nested_keep[
    nested_keep$a == "Total" & nested_keep$b == "Total",
  ]
  expect_equal(grand_total$data[[1]]$a, c("x", "x", "y"))
  expect_equal(grand_total$data[[1]]$b, c("u", "v", "u"))

  nested_keep <- nest_by_with_margins(
    data,
    .grouping = rollup(a, b),
    .keep = TRUE
  )
  expect_equal(names(nested_keep$data[[1]]), c("a", "b", "x"))
  expect_s3_class(nested_keep, "rowwise_df")
  subtotal <- nested_keep[nested_keep$a == "x" & nested_keep$b == "Total", ]
  expect_equal(subtotal$data[[1]]$a, c("x", "x"))
  expect_equal(subtotal$data[[1]]$b, c("u", "v"))

  grand_total <- nested_keep[
    nested_keep$a == "Total" & nested_keep$b == "Total",
  ]
  expect_equal(grand_total$data[[1]]$a, c("x", "x", "y"))
  expect_equal(grand_total$data[[1]]$b, c("u", "v", "u"))

  rowwise <- nest_by_with_margins(data, .grouping = rollup(a, b))
  expect_s3_class(rowwise, "rowwise_df")
})

test_that("both nesting interfaces expose .keep", {
  expect_true(".keep" %in% names(formals(nest_with_margins)))
  expect_true(".keep" %in% names(formals(nest_by_with_margins)))
})

test_that("nest internals never reserve user-visible column names", {
  data <- data.frame(
    a = c("x", "y"),
    value = 1:2,
    .marginplyr_set_id = 3:4,
    a_COPY__MARGINPLYR_ = 5:6
  )

  expect_error(
    nest_with_margins(data, .grouping = rollup(a), .key = ""),
    "must not be empty"
  )
  expect_no_error(
    nested <- nest_with_margins(
      data,
      .grouping = rollup(a),
      .key = ".marginplyr_set_id"
    )
  )
  expect_equal(names(nested), c("a", ".marginplyr_set_id"))

  expect_no_error(
    nest_by_with_margins(
      data,
      .grouping = rollup(a),
      .keep = TRUE
    )
  )
})

test_that("nest interfaces follow upstream .key and empty-input contracts", {
  data <- data.frame(group = c("a", "b"), value = 1:2)
  empty <- data[0, , drop = FALSE]

  default_key <- nest_with_margins(data, .by = group, .key = NULL)
  expect_equal(names(default_key), c("group", "data"))
  expect_error(
    nest_by_with_margins(data, .by = group, .key = NULL),
    "`.key` must be a character vector of length 1"
  )

  empty_nested <- nest_with_margins(empty)
  expect_equal(nrow(empty_nested), 0L)

  empty_rowwise <- nest_by_with_margins(empty)
  expect_s3_class(empty_rowwise, "rowwise_df")
  expect_equal(nrow(empty_rowwise), 1L)
  expect_equal(names(empty_rowwise), "data")
  expect_equal(names(empty_rowwise$data[[1]]), names(empty))
  expect_equal(nrow(empty_rowwise$data[[1]]), 0L)
})

test_that("nesting rejects duplicate grouping sets without visible identity", {
  data <- data.frame(group = c("a", "b"), value = 1:2)
  spec <- grouping_sets(grouping_set(group), grouping_set(group))

  expect_error(
    nest_with_margins(
      data,
      .grouping = spec,
      .duplicates = "keep"
    ),
    "`\\.duplicates` must be one of \"error\", \"drop\"\\."
  )
  expect_error(
    nest_by_with_margins(
      data,
      .grouping = spec,
      .duplicates = "keep"
    ),
    "`\\.duplicates` must be one of \"error\", \"drop\"\\."
  )
})

test_that("existing groups become implicit fixed keys", {
  data <- data.frame(
    year = c(2025L, 2025L, 2026L, 2026L),
    region = c("East", "West", "East", "West"),
    value = c(1, 10, 100, 1000)
  )
  grouped <- dplyr::group_by(data, year)
  ungrouped <- dplyr::ungroup(grouped)

  implicit_summary <- summarize_with_margins(
    grouped,
    value = sum(value),
    .grouping = rollup(region)
  )
  explicit_summary <- summarize_with_margins(
    ungrouped,
    value = sum(value),
    .by = year,
    .grouping = rollup(region)
  )
  implicit_summary <- dplyr::arrange(implicit_summary, year, region)
  explicit_summary <- dplyr::arrange(explicit_summary, year, region)
  expect_equal(implicit_summary, explicit_summary)
  expect_equal(dplyr::group_vars(implicit_summary), character())

  implicit_union <- expand_with_margins(
    grouped,
    .grouping = rollup(region)
  )
  explicit_union <- expand_with_margins(
    ungrouped,
    .by = year,
    .grouping = rollup(region)
  )
  implicit_union <- dplyr::arrange(implicit_union, year, region, value)
  explicit_union <- dplyr::arrange(explicit_union, year, region, value)
  expect_equal(implicit_union, explicit_union)
  expect_equal(dplyr::group_vars(implicit_union), character())

  implicit_nest <- nest_with_margins(
    grouped,
    .grouping = rollup(region)
  )
  explicit_nest <- nest_with_margins(
    ungrouped,
    .by = year,
    .grouping = rollup(region)
  )
  implicit_nest <- dplyr::arrange(implicit_nest, year, region)
  explicit_nest <- dplyr::arrange(explicit_nest, year, region)
  expect_equal(implicit_nest, explicit_nest)
  expect_equal(dplyr::group_vars(implicit_nest), character())

  implicit_nest_by <- nest_by_with_margins(
    grouped,
    .grouping = rollup(region)
  )
  explicit_nest_by <- nest_by_with_margins(
    ungrouped,
    .by = year,
    .grouping = rollup(region)
  )
  implicit_nest_by <- dplyr::arrange(implicit_nest_by, year, region)
  explicit_nest_by <- dplyr::arrange(explicit_nest_by, year, region)
  expect_equal(implicit_nest_by, explicit_nest_by)
  expect_equal(dplyr::group_vars(implicit_nest_by), c("year", "region"))
})

test_that("Margin results take their class from the underlying dplyr verb", {
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 2, 3)
  )

  summary <- summarize_with_margins(
    data,
    value = sum(value),
    .grouping = rollup(region)
  )

  expect_identical(class(summary), "data.frame")
  # ADR 0016 promises delegation rather than the literal class above:
  # marginplyr adds no class of its own to what the corresponding dplyr verb
  # returned. Were dplyr to change what `summarise()` gives a plain data
  # frame, this expectation would keep passing while the previous one failed,
  # which is the reading that says marginplyr still honours the promise.
  expect_identical(
    class(summary),
    class(dplyr::summarise(data, value = sum(value), .by = region))
  )

  expansion <- expand_with_margins(data, .grouping = rollup(region))

  expect_identical(class(expansion), "data.frame")
  expect_identical(
    class(expansion),
    class(dplyr::union_all(data, dplyr::mutate(data, region = "Total")))
  )

  nested <- nest_with_margins(data, .grouping = rollup(region))

  expect_identical(class(nested), "data.frame")
})

test_that("nest_by_with_margins() is row-wise whatever the input class", {
  data <- data.frame(
    region = c("East", "East", "West"),
    value = c(1, 2, 3)
  )

  nested_by <- nest_by_with_margins(data, .grouping = rollup(region))

  # Unlike the verbs above, the row-wise shape is marginplyr's own
  # construction, so ADR 0016 promises it for every input class. There is no
  # row-wise plain data frame in dplyr to delegate to.
  expect_s3_class(nested_by, "rowwise_df")
  expect_identical(dplyr::group_vars(nested_by), "region")
  # The promise about list-column elements is only that each is a data frame.
  # Their exact class follows the backend and is documented, not guaranteed.
  expect_s3_class(nested_by$data[[1L]], "data.frame")
})

test_that("grouped inputs reject conflicting grouping instructions", {
  data <- data.frame(
    year = c(2025L, 2026L),
    region = c("East", "West"),
    value = 1:2
  )
  grouped <- dplyr::group_by(data, year)

  expect_error(
    summarize_with_margins(
      grouped,
      value = sum(value),
      .by = year,
      .grouping = rollup(region)
    ),
    "Can't supply `.by`"
  )
  expect_error(
    expand_with_margins(
      grouped,
      .by = year,
      .grouping = rollup(region)
    ),
    "Can't supply `.by`"
  )
  expect_error(
    nest_with_margins(
      grouped,
      .by = year,
      .grouping = rollup(region)
    ),
    "Can't supply `.by`"
  )
  expect_error(
    nest_by_with_margins(
      grouped,
      .by = year,
      .grouping = rollup(region)
    ),
    "Can't supply `.by`"
  )

  expect_error(
    summarize_with_margins(
      grouped,
      value = sum(value),
      .grouping = rollup(year, region)
    ),
    "both `.by` and `.grouping`"
  )
})

test_that("row-wise inputs require explicit ungrouping", {
  data <- dplyr::rowwise(
    data.frame(region = c("East", "West"), value = 1:2)
  )

  expect_error(
    summarize_with_margins(
      data,
      value = sum(value),
      .grouping = rollup(region)
    ),
    "`rowwise\\(\\)` input is not supported"
  )
  expect_error(
    expand_with_margins(data, .grouping = rollup(region)),
    "`rowwise\\(\\)` input is not supported"
  )
  expect_error(
    nest_with_margins(data, .grouping = rollup(region)),
    "`rowwise\\(\\)` input is not supported"
  )
  expect_error(
    nest_by_with_margins(data, .grouping = rollup(region)),
    "`rowwise\\(\\)` input is not supported"
  )
})

test_that("empty persistent groups are rejected instead of silently dropped", {
  data <- data.frame(
    year = factor(2025L, levels = c(2025L, 2026L)),
    value = 1
  ) |>
    dplyr::group_by(year, .drop = FALSE)

  expect_error(
    summarize_with_margins(data, value = sum(value)),
    "`.drop = FALSE` is not supported"
  )
})

test_that("summary expressions reject the removed .groups argument", {
  data <- data.frame(group = c("b", "a", "b"), value = 1:3)

  expect_error(
    summarize_with_margins(
      data,
      value = sum(value),
      .by = group,
      .groups = "drop"
    ),
    paste0(
      "`summarize_with_margins\\(\\)` has no `\\.groups` argument; ",
      "Margin-summary results are always ungrouped\\."
    ),
    class = "marginplyr_error"
  )

  summary_options <- list(.groups = "drop")
  expect_error(
    summarize_with_margins(
      data,
      value = sum(value),
      .by = group,
      !!!summary_options
    ),
    "`summarize_with_margins\\(\\)` has no `\\.groups` argument",
    class = "marginplyr_error"
  )
})

test_that("Margin verb formals expose only the supported common options", {
  verbs <- list(
    summarize_with_margins,
    expand_with_margins,
    nest_with_margins,
    nest_by_with_margins
  )

  expect_false(".groups" %in% names(formals(summarize_with_margins)))
  expect_false(".groups" %in% names(formals(summarise_with_margins)))
  expect_true(all(vapply(
    verbs,
    function(verb) {
      identical(
        formals(verb)$.check_margin_label,
        quote(is.data.frame(.data))
      )
    },
    logical(1)
  )))
  expect_true(all(vapply(
    verbs,
    function(verb) {
      identical(
        formals(verb)$.margin_label_position,
        quote(c("last", "first"))
      )
    },
    logical(1)
  )))
  expect_true(all(vapply(
    verbs,
    function(verb) {
      identical(
        formals(verb)$.sort,
        quote(c("none", "last", "first"))
      )
    },
    logical(1)
  )))
})

test_that("callers can arrange Margin results explicitly", {
  data <- data.frame(group = c("b", "a", "b"), value = 1:3)

  summarized <- summarize_with_margins(
    data,
    value = sum(value),
    .by = group
  ) |>
    dplyr::arrange(group)
  nested <- nest_with_margins(data, .by = group) |>
    dplyr::arrange(group)

  expect_identical(class(summarized), "data.frame")
  expect_equal(summarized$group, c("a", "b"))
  expect_equal(nested$group, c("a", "b"))
})

test_that("British and American summary spellings are synonyms", {
  data <- data.frame(group = c("a", "a", "b"), value = 1:3)

  american <- summarize_with_margins(
    data,
    value = sum(value),
    .by = group
  )
  british <- summarise_with_margins(
    data,
    value = sum(value),
    .by = group
  )

  expect_equal(british, american)
  expect_identical(formals(summarise_with_margins), formals(
    summarize_with_margins
  ))
})

test_that("column-wise summaries exclude all grouping dimensions", {
  data <- data.frame(
    year = c(2026L, 2025L, 2026L),
    value = c(1, 2, 3)
  )

  across_result <- summarize_with_margins(
    data,
    dplyr::across(
      dplyr::everything(),
      dplyr::n_distinct,
      .names = "n_{.col}"
    ),
    .grouping = rollup(year)
  )
  pick_result <- summarize_with_margins(
    data,
    picked = paste(names(dplyr::pick(dplyr::everything())), collapse = ","),
    .grouping = rollup(year)
  )
  across_result <- dplyr::arrange(across_result, year)

  expect_equal(names(across_result), c("year", "n_value"))
  expect_equal(across_result$year, c("2025", "2026", "Total"))
  expect_equal(across_result$n_value, c(1L, 2L, 3L))
  expect_equal(pick_result$picked, rep("value", 3L))
})

test_that("column-wise summaries preserve names and unpacked outputs", {
  data <- data.frame(
    group = c("a", "a", "b"),
    x = c(1, 3, 5),
    y = c(2, 4, 6)
  )

  named <- summarize_with_margins(
    data,
    dplyr::across(
      c(first = x, y),
      list(lo = min, hi = max),
      .names = "{.col}_{.fn}"
    ),
    .grouping = rollup(group)
  )
  expect_equal(
    names(named),
    c("group", "first_lo", "first_hi", "y_lo", "y_hi")
  )

  range_frame <- function(x) {
    data.frame(lo = min(x), hi = max(x))
  }
  unpacked <- summarize_with_margins(
    data,
    dplyr::across(x, range_frame, .unpack = TRUE),
    .grouping = rollup(group)
  )
  expect_equal(names(unpacked), c("group", "x_lo", "x_hi"))
})

test_that("data-frame summaries cannot overwrite grouping columns", {
  data <- data.frame(group = c("a", "a", "b"), value = 1:3)

  expect_error(
    summarize_with_margins(
      data,
      tibble::tibble(group = dplyr::n()),
      .by = group
    ),
    "cannot overwrite grouping column.*`group`"
  )
  expect_error(
    summarize_with_margins(
      data,
      dplyr::across(value, mean, .names = "group"),
      .by = group
    ),
    "cannot overwrite grouping column.*`group`"
  )
})

test_that("branch-local dplyr group context helpers are rejected", {
  data <- data.frame(group = c("a", "a", "b"), value = 1:3)

  expect_error(
    summarize_with_margins(
      data,
      key = list(dplyr::cur_group()),
      .by = group
    ),
    "does not support.*cur_group"
  )
  expect_error(
    summarize_with_margins(
      data,
      id = dplyr::cur_group_id(),
      .by = group
    ),
    "does not support.*cur_group_id"
  )
  expect_error(
    summarize_with_margins(
      data,
      rows = list(dplyr::cur_group_rows()),
      .by = group
    ),
    "does not support.*cur_group_rows"
  )
  expect_error(
    summarize_with_margins(
      data,
      current = list(dplyr::cur_data_all()),
      .by = group
    ),
    "does not support.*cur_data_all"
  )
})

test_that("the documented `marginplyr_error` handler catches conditions", {
  # The handler pattern promised in `?marginplyr`. `abort_marginplyr()` is the
  # only constructor for Package conditions, so this covers the class contract
  # for every one of them. See ADR 0015 for which errors stay outside it.
  data <- data.frame(group = "x", value = 1)

  caught <- tryCatch(
    summarize_with_margins(
      data,
      n = dplyr::n(),
      .grouping = rollup(group),
      .duplicates = "merge"
    ),
    marginplyr_error = function(cnd) cnd
  )

  expect_s3_class(caught, "marginplyr_error")
  # `marginplyr_error` is the whole promise, so the rest of the class vector
  # stays unasserted. `simpleError` was a transitional shim and is gone.
  expect_false(inherits(caught, "simpleError"))
})

test_that("documented option formals match the shared choice vocabularies", {
  formal_default <- function(fn, arg) {
    eval(formals(fn)[[arg]])
  }

  duplicates_verbs <- list(
    summarize_with_margins = margin_duplicates_choices,
    summarise_with_margins = margin_duplicates_choices,
    expand_with_margins = margin_duplicates_choices,
    inspect_grouping = margin_duplicates_choices,
    nest_with_margins = nest_duplicates_choices,
    nest_by_with_margins = nest_duplicates_choices
  )

  for (verb in names(duplicates_verbs)) {
    expect_identical(
      formal_default(get(verb), ".duplicates"),
      duplicates_verbs[[verb]],
      info = verb
    )
  }

  position_verbs <- c(
    "summarize_with_margins",
    "summarise_with_margins",
    "expand_with_margins",
    "nest_with_margins",
    "nest_by_with_margins"
  )

  for (verb in position_verbs) {
    expect_identical(
      formal_default(get(verb), ".margin_label_position"),
      margin_label_position_choices,
      info = verb
    )
  }

  expect_identical(
    formal_default(inspect_grouping, ".format"),
    grouping_format_choices
  )
})

test_that("a printed Grouping specification names the constructor called", {
  exports <- getNamespaceExports("marginplyr")

  for (kind in names(grouping_kind_rules())) {
    constructor <- find_grouping_kind_rule(kind)$constructor
    expect_true(constructor %in% exports, info = kind)

    spec <- eval(rlang::call2(constructor, rlang::sym("a")))
    # The printed name is read from the kind stored on the object, so a rule
    # naming a constructor that produces some other kind would print the wrong
    # name for both of them.
    expect_identical(spec$type, kind, info = kind)
    expect_identical(
      utils::capture.output(print(spec)),
      paste0("<marginplyr grouping specification: ", constructor, ">"),
      info = kind
    )
  }

  # No constructor makes a kind the rules do not know, and the verbs reject
  # one. Printing it still names something rather than nothing.
  expect_identical(
    utils::capture.output(print(new_grouping_spec("nonesuch", list()))),
    "<marginplyr grouping specification: nonesuch>"
  )
})
