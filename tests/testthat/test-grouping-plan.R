test_that("rollup and cube compile to concrete grouping sets", {
  rollup_plan <- compile_grouping_spec(
    rollup(a, b, c),
    data_vars = c("a", "b", "c")
  )
  expect_equal(
    rollup_plan$sets,
    list(c("a", "b", "c"), c("a", "b"), "a", character())
  )
  expect_equal(
    unname(rollup_plan$grouping_masks),
    matrix(c(0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 1L, 1L, 1L, 1L),
      ncol = 3L,
      byrow = TRUE
    )
  )

  cube_plan <- compile_grouping_spec(
    cube(grouping_set(country, state), year),
    data_vars = c("country", "state", "year")
  )
  expect_equal(
    cube_plan$sets,
    list(
      c("country", "state", "year"),
      c("country", "state"),
      "year",
      character()
    )
  )
})

test_that("grouping families support union, nesting, and Cartesian product", {
  vars <- c("a", "b", "c", "d")

  product <- compile_grouping_spec(
    grouping_spec(rollup(a, b), cube(c)),
    data_vars = vars
  )
  expect_equal(
    product$sets,
    list(
      c("a", "b", "c"), c("a", "b"),
      c("a", "c"), "a", "c", character()
    )
  )

  nested <- compile_grouping_spec(
    grouping_sets(rollup(a, b), cube(c, d)),
    data_vars = vars,
    .duplicates = "drop"
  )
  expect_equal(
    nested$sets,
    list(c("a", "b"), "a", character(), c("c", "d"), "c", "d")
  )
})

test_that("selectors and fixed .by columns are resolved once", {
  selected <- c("a", "b")
  plan <- compile_grouping_spec(
    rollup(tidyselect::all_of(selected)),
    data_vars = c("year", "a", "b", "value"),
    .by = "year"
  )

  expect_equal(plan$by, "year")
  expect_equal(plan$dimensions, c("a", "b"))
  expect_equal(
    plan$sets,
    list(c("year", "a", "b"), c("year", "a"), "year")
  )
})

test_that("duplicate grouping sets have explicit policies", {
  spec <- grouping_sets(grouping_set(a), grouping_set(a))

  expect_error(
    compile_grouping_spec(spec, "a"),
    "Duplicate grouping sets"
  )
  expect_equal(
    compile_grouping_spec(spec, "a", .duplicates = "drop")$sets,
    list("a")
  )
  expect_equal(
    compile_grouping_spec(spec, "a", .duplicates = "keep")$sets,
    list("a", "a")
  )
})

test_that("invalid or ambiguous specifications fail early", {
  expect_error(
    compile_grouping_spec(rollup(a), "a", .by = "a"),
    "both `.by` and `.grouping`"
  )
  expect_error(
    compile_grouping_spec(rollup(cube(a)), "a"),
    "only accepts columns"
  )
  expect_error(
    compile_grouping_spec(grouping_sets(), "a"),
    "requires at least one set"
  )
  expect_error(
    compile_grouping_spec(rollup(floor(a)), "a"),
    "Invalid grouping column selection"
  )
})
