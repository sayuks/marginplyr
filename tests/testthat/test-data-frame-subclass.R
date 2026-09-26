# A data frame subclass is admitted input, and `[` is what made one fail.
#
# The public verbs accept any object dplyr can group (#77), which includes every
# `data.frame` subclass. A subclass is free to give `[` semantics of its own,
# and `data.table`'s reads a character index as a join key rather than as a
# column selection. Typed metadata -- the factor levels
# `restore_margin_factors()` rebuilds from, and the prototypes an absent Margin
# label falls back to -- was read with `data_proxy[dimensions]`, so a raw
# `data.table` failed before any grouping happened, with data.table's join
# diagnostic and nothing naming the input class as the reason (#176).
#
# `summarise_with_margins()` is deliberately not swept alongside the verbs
# below. "British and American summary spellings are synonyms" in
# `test-grouping-interface.R` asserts it is the same object with the same
# formals, so a copy here would restate that test rather than add a subclass it
# does not already cover.
#
# `data.table` is guarded like any other optional Suggest, so these skip where
# it is absent and the generated `data.table` job in `release-matrix.yaml` is
# what executes them. dtplyr is not the guard even though it brings data.table:
# guarding on a package other than the one used is the mistake `AGENTS.md`
# names, and the two are separate entries in `optional_suggest_spec()`.

subclass_data <- function() {
  data.frame(
    region = c("East", "East", "West", "West"),
    size = factor(
      c("small", "large", NA, "small"),
      levels = c("small", "large")
    ),
    units = c(1, 2, 4, 8)
  )
}

# Runs one Margin call twice -- over the raw `data.table` and over the same data
# as a base `data.frame` -- and compares the results. The call is written once
# and the input is the parameter, so the two sides cannot drift into comparing
# different computations; it is `expect_margin_order_agrees()`'s shape in
# `test-margin-order.R`, and so is the `all_of()` spelling of the dimensions,
# because `codetools` cannot follow an NSE pronoun through the closure that a
# `test_that()` block does not create.
#
# Both sides are flattened to a base data frame first. `as.data.frame()` removes
# the one difference ADR 0016 refuses to promise either way, and it removes
# nothing else: column classes, factor levels, and row order all survive it,
# which is the whole of what these tests are about.
expect_margin_agrees <- function(margin_call) {
  data <- subclass_data()
  expect_equal(
    as.data.frame(margin_call(data.table::as.data.table(data))),
    as.data.frame(margin_call(data))
  )
}

test_that("summarize_with_margins() accepts a raw data.table", {
  skip_if_suggest_absent("data.table")

  expect_margin_agrees(function(input) {
    summarize_with_margins(
      input,
      total = sum(units),
      share = share_of_total(total),
      .grouping = rollup(dplyr::all_of(c("region", "size"))),
      .sort = "last"
    )
  })
})

test_that("expand_with_margins() accepts a raw data.table", {
  skip_if_suggest_absent("data.table")

  expect_margin_agrees(function(input) {
    expand_with_margins(
      input,
      .grouping = cube(dplyr::all_of(c("region", "size"))),
      .id = "set"
    )
  })
})

test_that("ordered expansion accepts a one-row data.table", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(g = "a")
  before <- serialize(data, NULL)

  for (sort in c("first", "last")) {
    result <- expand_with_margins(
      data, .grouping = rollup(g), .sort = sort
    )
    expect_identical(names(result), "g")
    expect_identical(nrow(result), 2L)
    expect_identical(
      result$g,
      if (identical(sort, "first")) c("Total", "a") else c("a", "Total")
    )
    expect_identical(serialize(data, NULL), before)
  }
})

test_that("ordered data.table expansion keeps fixed and missing keys", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(
    value = c(20L, 10L, 30L),
    region = c("b", "a", NA_character_),
    fixed = c(2L, 1L, 1L)
  )
  before <- serialize(data, NULL)

  for (sort in c("first", "last")) {
    result <- expand_with_margins(
      data,
      .by = fixed,
      .grouping = rollup(region),
      .id = "set",
      .sort = sort
    )

    expect_identical(names(result), c("fixed", "region", "set", "value"))
    expect_identical(nrow(result), 6L)
    if (identical(sort, "last")) {
      expect_identical(result$fixed, c(1L, 1L, 1L, 1L, 2L, 2L))
      expect_identical(result$region,
                       c("a", NA_character_, "Total", "Total", "b", "Total"))
      expect_identical(result$set, c(1L, 1L, 2L, 2L, 1L, 2L))
      expect_identical(result$value[c(1L, 2L, 5L, 6L)],
                       c(10L, 30L, 20L, 20L))
      expect_identical(base::sort(result$value[3:4]), c(10L, 30L))
    } else {
      expect_identical(result$fixed, c(1L, 1L, 1L, 1L, 2L, 2L))
      expect_identical(result$region,
                       c("Total", "Total", "a", NA_character_, "Total", "b"))
      expect_identical(result$set, c(2L, 2L, 1L, 1L, 2L, 1L))
      expect_identical(base::sort(result$value[1:2]), c(10L, 30L))
      expect_identical(result$value[3:6], c(10L, 30L, 20L, 20L))
    }
    expect_identical(serialize(data, NULL), before)
  }
})

test_that("unordered data.table expansion remains available", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(g = "a")
  before <- serialize(data, NULL)
  result <- expand_with_margins(data, .grouping = rollup(g), .sort = "none")

  expect_identical(nrow(result), 2L)
  expect_identical(sort(result$g), c("Total", "a"))
  expect_identical(serialize(data, NULL), before)
})

test_that("ordered expansion keeps the plain data.frame control", {
  data <- data.frame(g = "a")
  before <- serialize(data, NULL)
  result <- expand_with_margins(data, .grouping = rollup(g), .sort = "last")

  expect_identical(result$g, c("a", "Total"))
  expect_identical(serialize(data, NULL), before)
})

test_that("ordered expansion keeps the immutable dtplyr control", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(g = "a")
  source <- dtplyr::lazy_dt(data, immutable = TRUE)
  before <- serialize(data, NULL)
  result <- dplyr::collect(expand_with_margins(
    source, .grouping = rollup(g), .sort = "last"
  ))

  expect_identical(result$g, c("a", "Total"))
  expect_identical(serialize(data, NULL), before)
})

test_that("the nesting verbs accept a raw data.table", {
  skip_if_suggest_absent("data.table")

  expect_margin_agrees(function(input) {
    nest_with_margins(
      input,
      .grouping = rollup(dplyr::all_of(c("region", "size")))
    )
  })
  expect_margin_agrees(function(input) {
    nest_by_with_margins(
      input,
      .grouping = rollup(dplyr::all_of(c("region", "size")))
    )
  })
})

test_that("inspect_grouping() accepts a raw data.table", {
  skip_if_suggest_absent("data.table")
  # Not through `expect_margin_agrees()`: this returns no data frame, and the
  # plan it reports is one object to compare rather than two to flatten.
  data <- subclass_data()

  expect_identical(
    inspect_grouping(
      data.table::as.data.table(data),
      .grouping = cube(region, size),
      .format = "list"
    ),
    inspect_grouping(data, .grouping = cube(region, size), .format = "list")
  )
})

test_that("a factor dimension of a raw data.table keeps its typed metadata", {
  skip_if_suggest_absent("data.table")
  # The narrower half of the agreement above, stated on its own because it is
  # the metadata the old `[` was reading: a Margin label arrives as a synthetic
  # level rather than coercing the column to character, and the missing value
  # stays missing rather than becoming one.
  result <- summarize_with_margins(
    data.table::as.data.table(subclass_data()),
    total = sum(units),
    .grouping = rollup(size),
    .margin_label_position = "first"
  )

  expect_s3_class(result$size, "factor")
  expect_identical(levels(result$size), c("Total", "small", "large"))
  expect_true(anyNA(result$size))
})

test_that("a Margin-label collision is found in a raw data.table", {
  skip_if_suggest_absent("data.table")
  # The collision check reads the same columns, so a subclass that reached the
  # verbs without it would accept a label already present and silently merge
  # two meanings into one row.
  data <- data.frame(region = c("East", "Total"), units = c(1, 2))

  expect_error(
    summarize_with_margins(
      data.table::as.data.table(data),
      total = sum(units),
      .grouping = rollup(region)
    ),
    class = "marginplyr_error"
  )
  expect_error(
    summarize_with_margins(
      data,
      total = sum(units),
      .grouping = rollup(region)
    ),
    class = "marginplyr_error"
  )
})

test_that("a raw data.table is not modified by reference", {
  skip_if_suggest_absent("data.table")
  # A data.table can be changed in place, so "the input still works afterwards"
  # is not the same claim as "the input is unchanged". Compared against an
  # independent copy taken before the call, which catches a column rewritten in
  # place, a key added, and a class or attribute set on the object itself.
  data <- data.table::as.data.table(subclass_data())
  before <- data.table::copy(data)

  summarize_with_margins(
    data,
    total = sum(units),
    .grouping = cube(region, size)
  )
  expand_with_margins(data, .grouping = rollup(region, size))
  nest_with_margins(data, .grouping = rollup(region))

  expect_identical(data, before)
})

test_that("an ordinary data frame reads the same typed metadata", {
  # The other half of the regression. `proxy_columns()` replaced a `[` whose
  # behavior on a base `data.frame` was correct, so the subclass tests above
  # cannot tell a working replacement from one that returns the wrong columns
  # in the right shape.
  data <- subclass_data()
  info <- margin_column_info(
    data,
    dimensions = c("size", "region"),
    backend = grouping_backend(data)
  )

  expect_identical(names(info$prototypes), c("size", "region"))
  expect_identical(info$prototypes$region, NA_character_)
  expect_identical(
    info$prototypes$size,
    factor(NA_character_, levels = c("small", "large"))
  )
  expect_identical(
    vapply(info$factors, function(x) x$col, character(1)),
    "size"
  )
  expect_identical(info$factors[[1]]$levels, c("small", "large"))
})

test_that("a proxy that cannot answer for a column is an internal invariant", {
  # Every dimension is resolved against the same data by tidyselect before this
  # runs, so a `NULL` here reports a defect -- a subclass whose `[[` is not
  # column extraction -- and no rewrite of the call avoids it. Bare per ADR
  # 0015, which is what keeps it distinguishable from the validation errors the
  # verbs raise on input a caller can fix.
  data <- data.frame(region = "East", units = 1)

  condition <- expect_error(proxy_columns(data, c("region", "absent")))
  expect_false(inherits(condition, "marginplyr_error"))
  expect_match(conditionMessage(condition), "`absent`")
})
