test_that("parenthesized omitted public arguments retain their defaults", {
  data <- data.frame(region = c("a", "b"), value = 1:2)
  wrappers <- list(
    summarize_with_margins = function(data, by, grouping) {
      summarize_with_margins(data, total = sum(value),
        .by = ({{ by }}), .grouping = ({{ grouping }}))
    },
    summarise_with_margins = function(data, by, grouping) {
      summarise_with_margins(data, total = sum(value),
        .by = ({{ by }}), .grouping = ({{ grouping }}))
    },
    expand_with_margins = function(data, by, grouping) {
      expand_with_margins(data, .by = ({{ by }}), .grouping = ({{ grouping }}))
    },
    nest_with_margins = function(data, by, grouping) {
      nest_with_margins(data, .by = ({{ by }}), .grouping = ({{ grouping }}))
    },
    nest_by_with_margins = function(data, by, grouping) {
      nest_by_with_margins(data, .by = ({{ by }}), .grouping = ({{ grouping }}))
    },
    inspect_grouping = function(data, by, grouping) {
      inspect_grouping(data, .by = ({{ by }}), .grouping = ({{ grouping }}))
    }
  )
  expect_setequal(names(wrappers), verbs_taking(".by"))
  expect_setequal(names(wrappers), verbs_taking(".grouping"))
  for (name in names(wrappers)) {
    expect_identical(wrappers[[name]](data, grouping = rollup(region)),
      forwarded_verbs[[name]](data, grouping = rollup(region)), info = name)
    expect_identical(wrappers[[name]](data, by = NULL),
      forwarded_verbs[[name]](data, by = NULL), info = name)
  }
})

test_that("parenthesized omitted constructor arguments name their position", {
  data <- data.frame(region = c("a", "b"), grade = c("x", "y"), value = 1:2)
  constructors <- c("grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec")
  expect_setequal(constructors, grouping_constructor_names())
  for (constructor in constructors) {
    make_leading <- eval(substitute(
      function(x) CONSTRUCTOR(({{ x }}), region),
      list(CONSTRUCTOR = as.name(constructor))
    ))
    make_interior <- eval(substitute(
      function(x) CONSTRUCTOR(region, ({{ x }}), grade),
      list(CONSTRUCTOR = as.name(constructor))
    ))
    for (position in 1:2) {
      spec <- if (position == 1L) make_leading() else make_interior()
      for (name in names(forwarded_verbs)) {
        error <- expect_error(forwarded_verbs[[name]](data, grouping = !!spec))
        expect_s3_class(error, "marginplyr_error")
        expect_identical(conditionMessage(error),
          paste0("Argument ", position, " of `", constructor, "()` is empty.\n",
            "i Remove the comma, or write the columns that position selects."))
        expect_identical(conditionCall(error)[[1L]], as.name(name))
      }
    }
  }
})
