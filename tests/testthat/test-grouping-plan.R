test_that("rollup and cube compile to concrete grouping sets", {
  rollup_plan <- compile_grouping_spec(
    rollup(a, b, c),
    data_vars = c("a", "b", "c"),
    duplicates_choices = margin_duplicates_choices
  )
  expect_equal(
    rollup_plan$sets,
    list(c("a", "b", "c"), c("a", "b"), "a", character())
  )
  expect_identical(rollup_plan$set_ids, 1:4)
  expect_equal(
    unname(rollup_plan$grouping_masks),
    matrix(c(0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 1L, 1L, 1L, 1L),
      ncol = 3L,
      byrow = TRUE
    )
  )

  cube_plan <- compile_grouping_spec(
    cube(grouping_set(country, state), year),
    data_vars = c("country", "state", "year"),
    duplicates_choices = margin_duplicates_choices
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
    data_vars = vars,
    duplicates_choices = margin_duplicates_choices
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
    .duplicates = "drop",
    duplicates_choices = margin_duplicates_choices
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
    compile_grouping_spec(
      spec,
      "a",
      .duplicates = "keep",
      duplicates_choices = margin_duplicates_choices
    )
  }

  for (parent in names(constructors)) {
    for (child in names(nested_calls)) {
      if (child %in% allowed_nested[[parent]]) {
        expect_no_error(compile_nested(parent, child))
        next
      }

      error <- expect_error(compile_nested(parent, child))
      expect_s3_class(error, "marginplyr_error")
      expect_identical(conditionMessage(error), nesting_errors[[parent]])
    }
  }

  for (constructor in constructors) {
    spec <- eval(rlang::call2(constructor, rlang::sym("a")))
    expect_no_error(
      compile_grouping_spec(
        spec,
        "a",
        .duplicates = "keep",
        duplicates_choices = margin_duplicates_choices
      )
    )
  }
})

test_that("empty grouping rules preserve their phase and error precedence", {
  expect_equal(
    compile_grouping_spec(
      grouping_set(a),
      "a",
      duplicates_choices = margin_duplicates_choices
    )$sets,
    list("a")
  )
  expect_equal(
    compile_grouping_spec(
      grouping_set(),
      "a",
      duplicates_choices = margin_duplicates_choices
    )$sets,
    list(character())
  )
  expect_equal(
    compile_grouping_spec(
      grouping_spec(),
      "a",
      duplicates_choices = margin_duplicates_choices
    )$sets,
    list(character())
  )

  sets_error <- expect_error(
    compile_grouping_spec(
      grouping_sets(),
      "a",
      duplicates_choices = margin_duplicates_choices
    )
  )
  expect_s3_class(sets_error, "marginplyr_error")
  expect_identical(
    conditionMessage(sets_error),
    paste0(
      "`grouping_sets()` requires at least one set. Use `grouping_set()` ",
      "for the empty grouping set."
    )
  )

  for (constructor in c("rollup", "cube")) {
    spec <- eval(rlang::call2(constructor))
    error <- expect_error(compile_grouping_spec(
      spec,
      "a",
      duplicates_choices = margin_duplicates_choices
    ))
    expect_s3_class(error, "marginplyr_error")
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
      compile_grouping_spec(
        resolved_spec,
        "a",
        duplicates_choices = margin_duplicates_choices
      )
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
      compile_grouping_spec(
        composite_spec,
        "a",
        duplicates_choices = margin_duplicates_choices
      )
    )
    expect_identical(
      conditionMessage(empty_composite),
      "An empty `grouping_set()` cannot be a composite dimension."
    )
  }

  child_error <- expect_error(
    compile_grouping_spec(
      rollup(grouping_sets()),
      "a",
      duplicates_choices = margin_duplicates_choices
    )
  )
  expect_identical(conditionMessage(child_error), conditionMessage(sets_error))
})

test_that("invalid grouping input lists every supported constructor", {
  error <- expect_error(compile_grouping_spec(
    1,
    "a",
    duplicates_choices = margin_duplicates_choices
  ))
  expect_s3_class(error, "marginplyr_error")
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
    .by = "year",
    duplicates_choices = margin_duplicates_choices
  )

  expect_equal(plan$by, "year")
  expect_equal(plan$dimensions, c("a", "b"))
  expect_equal(
    plan$sets,
    list(c("year", "a", "b"), c("year", "a"), "year")
  )
})

test_that("a renaming grouping selection is refused by every constructor", {
  data_vars <- c("region", "year", "value")
  renamed_message <- paste0(
    "Can't rename grouping dimension `area = region`. ",
    "Grouping dimensions must name existing columns."
  )
  renaming_calls <- list(
    quote(tidyselect::all_of(c(area = "region"))),
    quote(c(area = region))
  )
  constructors <- c(
    "grouping_set",
    "grouping_sets",
    "rollup",
    "cube",
    "grouping_spec"
  )

  for (constructor in constructors) {
    for (selection in renaming_calls) {
      spec <- eval(rlang::call2(constructor, selection))
      error <- expect_error(compile_grouping_spec(
        spec,
        data_vars,
        duplicates_choices = margin_duplicates_choices
      ))
      expect_s3_class(error, "marginplyr_error")
      expect_identical(conditionMessage(error), renamed_message)
    }
  }

  nested <- expect_error(
    compile_grouping_spec(
      rollup(grouping_set(tidyselect::all_of(c(area = "region")))),
      data_vars,
      duplicates_choices = margin_duplicates_choices
    )
  )
  expect_s3_class(nested, "marginplyr_error")
  expect_identical(conditionMessage(nested), renamed_message)

  several <- expect_error(
    compile_grouping_spec(
      rollup(tidyselect::all_of(c(area = "region", when = "year"))),
      data_vars,
      duplicates_choices = margin_duplicates_choices
    )
  )
  expect_s3_class(several, "marginplyr_error")
  expect_identical(
    conditionMessage(several),
    paste0(
      "Can't rename grouping dimensions `area = region`, `when = year`. ",
      "Grouping dimensions must name existing columns."
    )
  )
})

test_that("non-renaming grouping selections keep resolving", {
  data_vars <- c("region", "region_code", "year")
  selected <- c("region", "year")

  expect_equal(
    compile_grouping_spec(
      rollup(region, year),
      data_vars,
      duplicates_choices = margin_duplicates_choices
    )$dimensions,
    c("region", "year")
  )
  expect_equal(
    compile_grouping_spec(
      rollup(tidyselect::all_of(selected)),
      data_vars,
      duplicates_choices = margin_duplicates_choices
    )$dimensions,
    c("region", "year")
  )
  expect_equal(
    compile_grouping_spec(
      rollup(tidyselect::starts_with("region")),
      data_vars,
      duplicates_choices = margin_duplicates_choices
    )$dimensions,
    c("region", "region_code")
  )
  expect_equal(
    compile_grouping_spec(
      rollup(-year),
      data_vars,
      duplicates_choices = margin_duplicates_choices
    )$dimensions,
    c("region", "region_code")
  )
  # A name that repeats the column it selects renames nothing, so the plan it
  # builds still names a column of the input.
  expect_equal(
    compile_grouping_spec(
      rollup(tidyselect::all_of(c(region = "region"))),
      data_vars,
      duplicates_choices = margin_duplicates_choices
    )$dimensions,
    "region"
  )
})

by_rename_vars <- function() {
  c("region", "area", "region_code", "value")
}

by_rename_message <- function() {
  paste0(
    "Can't rename `.by` column `area = region`. ",
    "Fixed `.by` keys must name existing columns."
  )
}

test_that("a renaming .by selection is refused", {
  data_vars <- by_rename_vars()

  # The renamed-to name is another column of the input, so a resolution that
  # reports the name the caller wrote fixes the plan on `area` and never groups
  # by the column the selection named (#134).
  clashing <- expect_error(
    resolve_fixed_keys(rlang::quo(c(area = region)), character(), data_vars)
  )
  expect_s3_class(clashing, "marginplyr_error")
  expect_identical(conditionMessage(clashing), by_rename_message())

  # The renamed-to name is no column at all, which used to be rejected as an
  # unknown `.by` column the caller never wrote.
  absent <- expect_error(
    resolve_fixed_keys(
      rlang::quo(c(area = region)),
      character(),
      c("region", "value")
    )
  )
  expect_s3_class(absent, "marginplyr_error")
  expect_identical(conditionMessage(absent), by_rename_message())

  several <- expect_error(
    resolve_fixed_keys(
      rlang::quo(tidyselect::all_of(c(area = "region", size = "value"))),
      character(),
      data_vars
    )
  )
  expect_s3_class(several, "marginplyr_error")
  expect_identical(
    conditionMessage(several),
    paste0(
      "Can't rename `.by` columns `area = region`, `size = value`. ",
      "Fixed `.by` keys must name existing columns."
    )
  )
})

test_that("fixed keys settled by name alone need no typed metadata", {
  data_vars <- by_rename_vars()
  selected <- c("region", "value")

  expect_identical(
    resolve_fixed_keys(rlang::quo(region), character(), data_vars),
    "region"
  )
  expect_identical(
    resolve_fixed_keys(rlang::quo(c(region, area)), character(), data_vars),
    c("region", "area")
  )
  expect_identical(
    resolve_fixed_keys(
      rlang::quo(tidyselect::all_of(selected)),
      character(),
      data_vars
    ),
    c("region", "value")
  )
  expect_identical(
    resolve_fixed_keys(
      rlang::quo(tidyselect::starts_with("region")),
      character(),
      data_vars
    ),
    c("region", "region_code")
  )
  expect_identical(
    resolve_fixed_keys(rlang::quo(NULL), character(), data_vars),
    character()
  )
  # A name that repeats the column it selects renames nothing.
  expect_identical(
    resolve_fixed_keys(
      rlang::quo(tidyselect::all_of(c(region = "region"))),
      character(),
      data_vars
    ),
    "region"
  )
  # Grouping columns are names dplyr resolved, so a grouped input's keys are
  # taken as they stand rather than selected again.
  expect_identical(
    resolve_fixed_keys(rlang::quo(NULL), "region", data_vars),
    "region"
  )
  # A predicate is the one selection column names cannot answer, so it is left
  # for the typed snapshot.
  expect_null(
    resolve_fixed_keys(rlang::quo(where(is.numeric)), character(), data_vars)
  )
})

test_that("duplicate grouping sets have explicit policies", {
  spec <- grouping_sets(grouping_set(a), grouping_set(a))

  expect_error(
    compile_grouping_spec(
      spec,
      "a",
      duplicates_choices = margin_duplicates_choices
    ),
    "Duplicate grouping sets"
  )
  dropped <- compile_grouping_spec(
    spec,
    "a",
    .duplicates = "drop",
    duplicates_choices = margin_duplicates_choices
  )
  kept <- compile_grouping_spec(
    spec,
    "a",
    .duplicates = "keep",
    duplicates_choices = margin_duplicates_choices
  )
  expect_equal(dropped$sets, list("a"))
  expect_identical(dropped$set_ids, 1L)
  expect_equal(kept$sets, list("a", "a"))
  expect_identical(kept$set_ids, 1:2)
})

test_that("invalid or ambiguous specifications fail early", {
  expect_error(
    compile_grouping_spec(
      rollup(a),
      "a",
      .by = "a",
      duplicates_choices = margin_duplicates_choices
    ),
    "both `.by` and `.grouping`"
  )
  expect_error(
    compile_grouping_spec(
      rollup(cube(a)),
      "a",
      duplicates_choices = margin_duplicates_choices
    ),
    "only accepts columns"
  )
  expect_error(
    compile_grouping_spec(
      grouping_sets(),
      "a",
      duplicates_choices = margin_duplicates_choices
    ),
    "requires at least one set"
  )
  expect_error(
    compile_grouping_spec(
      rollup(floor(a)),
      "a",
      duplicates_choices = margin_duplicates_choices
    ),
    "object 'a' not found"
  )
})

# The plan compiler was reachable without going through
# `compile_grouping_spec()`, so the preflight and the `.duplicates` matching
# the wrapper performs were a second source of truth that production supplied
# for itself (#119). A test that compiled through the wrapper therefore proved
# nothing about the sequence production ran. This holds the two together: the
# wrapper is the entry point, and a new call site that skips it fails here
# rather than in whichever plan silently stopped being preflighted.
test_that("compile_grouping_spec() is the only caller of the plan compiler", {
  # Assembled rather than written out, so the source scan below does not
  # report this file for holding the name it searches for.
  impl <- paste0("compile_grouping_spec", "_impl")
  ns <- asNamespace("marginplyr")
  objects <- ls(ns, all.names = TRUE)
  calls_impl <- vapply(
    objects,
    function(name) {
      object <- get(name, envir = ns)
      if (!is.function(object)) {
        return(FALSE)
      }
      any(grepl(impl, deparse(body(object)), fixed = TRUE))
    },
    logical(1)
  )
  expect_identical(unname(objects[calls_impl]), "compile_grouping_spec")

  # The namespace scan cannot see a test, and a test reaching the
  # implementation with `:::` is the half of #119 that was actually there. The
  # sources sit beside this file whenever the suite runs, so scanning them
  # needs no installed copy; the assertion below refuses a scan that found no
  # files rather than passing on one.
  sources <- list.files(".", pattern = "^test-.*\\.R$", full.names = TRUE)
  expect_gt(length(sources), 1L)
  reached_by <- Filter(
    function(path) {
      any(grepl(paste0(impl, "("), readLines(path), fixed = TRUE))
    },
    sources
  )
  expect_identical(reached_by, character())
})

# The Margin vocabulary was hard-coded here, so the nesting verbs' narrower one
# reached the compiler through `prepare_grouping_plan()` and through no test.
test_that("compile_grouping_spec() reads a narrowed duplicates vocabulary", {
  spec <- grouping_sets(grouping_set(a), grouping_set(a))

  refused <- expect_error(
    compile_grouping_spec(
      spec,
      "a",
      .duplicates = "keep",
      duplicates_choices = nest_duplicates_choices
    )
  )
  expect_s3_class(refused, "marginplyr_error")
  expect_identical(
    conditionMessage(refused),
    "`.duplicates` must be one of \"error\", \"drop\"."
  )

  # An untouched `.duplicates` stands for the first entry of the list the
  # caller stated, not of the Margin one.
  duplicated <- expect_error(
    compile_grouping_spec(
      spec,
      "a",
      duplicates_choices = nest_duplicates_choices
    )
  )
  # The diagnostic offers the policies this caller could have asked for
  # instead, so a narrowed vocabulary must not offer `"keep"` (#110).
  expect_identical(
    conditionMessage(duplicated),
    paste0(
      "Duplicate grouping sets were produced at positions 1, 2. ",
      "Use `.duplicates = \"drop\"`."
    )
  )

  dropped <- compile_grouping_spec(
    spec,
    "a",
    .duplicates = "drop",
    duplicates_choices = nest_duplicates_choices
  )
  expect_equal(dropped$sets, list("a"))
})
