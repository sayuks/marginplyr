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

test_that("grouping specification kinds enforce the nesting grammar", {
  nested_calls <- list(
    set = quote(grouping_set(a)),
    sets = quote(grouping_sets(grouping_set(a))),
    rollup = quote(rollup(a)),
    cube = quote(cube(a)),
    product = quote(grouping_spec(grouping_set(a)))
  )
  allowed_nested <- list(
    set = character(),
    sets = names(nested_calls),
    rollup = "set",
    cube = "set",
    product = names(nested_calls)
  )
  nesting_errors <- c(
    set = paste0(
      "A `grouping_set()` can contain columns, not another ",
      "grouping family."
    ),
    rollup = paste0(
      "`rollup()` only accepts columns or `grouping_set()` ",
      "composite dimensions."
    ),
    cube = paste0(
      "`cube()` only accepts columns or `grouping_set()` ",
      "composite dimensions."
    )
  )
  constructors <- c(
    set = "grouping_set",
    sets = "grouping_sets",
    rollup = "rollup",
    cube = "cube",
    product = "grouping_spec"
  )

  compile_nested <- function(parent, child) {
    spec <- eval(
      rlang::call2(constructors[[parent]], nested_calls[[child]])
    )
    compile_grouping_spec(spec, "a", .duplicates = "keep")
  }

  for (parent in names(constructors)) {
    for (child in names(nested_calls)) {
      if (child %in% allowed_nested[[parent]]) {
        expect_no_error(compile_nested(parent, child))
        next
      }

      error <- expect_error(compile_nested(parent, child))
      expect_s3_class(error, "simpleError")
      expect_identical(conditionMessage(error), nesting_errors[[parent]])
    }
  }

  for (constructor in constructors) {
    spec <- eval(rlang::call2(constructor, rlang::sym("a")))
    expect_no_error(
      compile_grouping_spec(spec, "a", .duplicates = "keep")
    )
  }
})

test_that("empty grouping rules preserve their phase and error precedence", {
  expect_equal(
    compile_grouping_spec(grouping_set(a), "a")$sets,
    list("a")
  )
  expect_equal(
    compile_grouping_spec(grouping_set(), "a")$sets,
    list(character())
  )
  expect_equal(
    compile_grouping_spec(grouping_spec(), "a")$sets,
    list(character())
  )

  sets_error <- expect_error(
    compile_grouping_spec(grouping_sets(), "a")
  )
  expect_s3_class(sets_error, "simpleError")
  expect_identical(
    conditionMessage(sets_error),
    paste0(
      "`grouping_sets()` requires at least one set. Use `grouping_set()` ",
      "for the empty grouping set."
    )
  )

  for (constructor in c("rollup", "cube")) {
    spec <- eval(rlang::call2(constructor))
    error <- expect_error(compile_grouping_spec(spec, "a"))
    expect_s3_class(error, "simpleError")
    expect_identical(
      conditionMessage(error),
      sprintf("`%s()` requires at least one dimension.", constructor)
    )
  }

  for (constructor in c("rollup", "cube")) {
    resolved_spec <- eval(rlang::call2(
      constructor,
      quote(tidyselect::any_of("missing"))
    ))
    resolved_empty <- expect_error(
      compile_grouping_spec(resolved_spec, "a")
    )
    expect_identical(
      conditionMessage(resolved_empty),
      sprintf("`%s()` requires at least one dimension.", constructor)
    )

    composite_spec <- eval(rlang::call2(
      constructor,
      quote(grouping_set(tidyselect::any_of("missing")))
    ))
    empty_composite <- expect_error(
      compile_grouping_spec(composite_spec, "a")
    )
    expect_identical(
      conditionMessage(empty_composite),
      "An empty `grouping_set()` cannot be a composite dimension."
    )
  }

  child_error <- expect_error(
    compile_grouping_spec(rollup(grouping_sets()), "a")
  )
  expect_identical(conditionMessage(child_error), conditionMessage(sets_error))
})

test_that("invalid grouping input lists every supported constructor", {
  error <- expect_error(compile_grouping_spec(1, "a"))
  expect_s3_class(error, "simpleError")
  expect_identical(
    conditionMessage(error),
    paste0(
      "`.grouping` must be created with `grouping_set()`, ",
      "`grouping_sets()`, `rollup()`, `cube()`, or `grouping_spec()`."
    )
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
