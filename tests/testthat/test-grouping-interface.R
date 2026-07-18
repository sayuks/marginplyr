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
      ga = grouping(a),
      gb = grouping(b),
      gid = grouping_id(a, b),
      .grouping = rollup(a, b),
      .sort = FALSE
    )
  )

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
    ),
    .sort = FALSE
  )

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
    .grouping = cube(grouping_set(country, state), year),
    .sort = FALSE
  )
  expect_setequal(unique(composite$gid), c(0L, 1L, 6L, 7L))
  expect_false(any(composite$gid %in% c(2L, 3L, 4L, 5L)))

  product <- summarize_with_margins(
    data,
    n = dplyr::n(),
    gid = grouping_id(country, state, year),
    .grouping = grouping_spec(rollup(country, state), cube(year)),
    .sort = FALSE
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

test_that("fixed .by columns are never replaced", {
  data <- data.frame(
    year = c(2024L, 2024L, 2025L),
    region = c("East", "West", "East")
  )

  result <- summarize_with_margins(
    data,
    n = dplyr::n(),
    gy = grouping(year),
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
    g = grouping(a),
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
  expect_equal(levels(result$a), c("Total", "x", "y"))
  expect_equal(levels(result$b), c("Total", "u", "v"))
})

test_that("grouping helpers validate their context and columns", {
  expect_error(grouping(a), "only be used inside")
  expect_error(grouping_id(a), "only be used inside")

  data <- data.frame(a = 1, b = 1)
  expect_error(
    summarize_with_margins(
      data,
      bad = grouping(b),
      .grouping = rollup(a)
    ),
    "not part of"
  )
  expect_error(
    summarize_with_margins(
      data,
      bad = grouping_id(a, a),
      .grouping = rollup(a)
    ),
    "duplicate columns"
  )
})

test_that("union and nest verbs consume the same grouping plan", {
  data <- data.frame(a = c("x", "x", "y"), b = c("u", "v", "u"), x = 1:3)

  expanded <- union_all_with_margins(data, .grouping = rollup(a, b))
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

  rowwise <- nest_by_with_margins(data, .grouping = rollup(a, b))
  expect_s3_class(rowwise, "rowwise_df")
})

test_that("nest list-column names cannot collide with internal columns", {
  data <- data.frame(a = c("x", "y"), value = 1:2)

  expect_error(
    nest_with_margins(data, .grouping = rollup(a), .key = ""),
    "must not be empty"
  )
  expect_error(
    nest_with_margins(
      data,
      .grouping = rollup(a),
      .key = ".marginplyr_set_id"
    ),
    "reserved name"
  )
})
