# The byte-exact record of every diagnostic that pluralizes its own noun, both
# arms of each. It is one file rather than an addition to each site's own test
# file because these pins are one artifact with one purpose: #223 re-authors
# these messages in the cli idiom file by file, and each of those PRs diffs
# against this baseline. Spread across the five files that raise them, a
# dropped arm would read as an ordinary deletion; here the set is visible at
# once, and a file-by-file re-authoring edits a contiguous block of it (#224).
#
# The regex pins already beside each site stay where they are. They assert that
# a caller reaches the diagnostic, which is a different question from what it
# says, and a phrase match survives the re-wording this file exists to make
# visible.
#
# Each Package condition is reached through the public verb that raises it, so
# a message that stops being reachable fails here rather than passing on an
# internal call that production no longer performs. The two internal invariants
# have no such path by construction -- that is what makes them invariants -- so
# they are called directly, and the classification is asserted beside the
# wording because ADR 0015 makes it part of what the site promises.
#
# Plain expectations rather than snapshots: snapshots are skipped under CRAN
# semantics, and these hold there too. Nothing here needs an optional backend,
# so every configuration of the release matrix executes all of it.

test_that("the selection proxy invariant pluralizes its column noun", {
  data <- data.frame(region = "East", units = 1)

  singular <- expect_error(proxy_columns(data, c("region", "absent")))
  expect_false(inherits(singular, "marginplyr_error"))
  expect_identical(
    conditionMessage(singular),
    "The selection proxy has no column `absent`."
  )

  plural <- expect_error(
    proxy_columns(data, c("region", "absent", "missing"))
  )
  expect_false(inherits(plural, "marginplyr_error"))
  expect_identical(
    conditionMessage(plural),
    "The selection proxy has no columns `absent`, `missing`."
  )
})

test_that("the unknown `.by` invariant pluralizes its column noun", {
  compile <- function(by) {
    compile_grouping_spec(
      rollup(region),
      "region",
      .by = by,
      duplicates_choices = margin_duplicates_choices
    )
  }

  singular <- expect_error(compile("nope"))
  expect_false(inherits(singular, "marginplyr_error"))
  expect_identical(
    conditionMessage(singular),
    "Unknown `.by` column `nope`."
  )

  plural <- expect_error(compile(c("nope", "nor")))
  expect_false(inherits(plural, "marginplyr_error"))
  expect_identical(
    conditionMessage(plural),
    "Unknown `.by` columns `nope`, `nor`."
  )
})

test_that("the fixed `.by` label refusal pluralizes its column noun", {
  data <- data.frame(region = "E", grade = "A", n = 1)

  singular <- expect_error(expand_with_margins(
    data,
    .by = region,
    .grouping = rollup(grade),
    .margin_label = c(region = "A", grade = "A")
  ))
  expect_s3_class(singular, "marginplyr_error")
  expect_identical(
    conditionMessage(singular),
    "`.margin_label` must not name fixed `.by` column `region`."
  )

  plural <- expect_error(expand_with_margins(
    data,
    .by = c(region, grade),
    .grouping = rollup(n),
    .margin_label = c(region = "A", grade = "A", n = "A")
  ))
  expect_s3_class(plural, "marginplyr_error")
  expect_identical(
    conditionMessage(plural),
    "`.margin_label` must not name fixed `.by` columns `region`, `grade`."
  )
})

test_that("the unknown label dimension refusal pluralizes its name noun", {
  data <- data.frame(region = "E", grade = "A", n = 1)
  operation <- function(label) {
    expand_with_margins(data, .grouping = rollup(region), .margin_label = label)
  }

  singular <- expect_error(operation(c(region = "A", u = "A")))
  expect_s3_class(singular, "marginplyr_error")
  expect_identical(
    conditionMessage(singular),
    "`.margin_label` has unknown dimension name `u`."
  )

  plural <- expect_error(operation(c(region = "A", u = "A", v = "A")))
  expect_s3_class(plural, "marginplyr_error")
  expect_identical(
    conditionMessage(plural),
    "`.margin_label` has unknown dimension names `u`, `v`."
  )
})

test_that("the NA-level refusal pluralizes its grouping column noun", {
  na_factor <- function(x) {
    factor(x, levels = c(unique(x[!is.na(x)]), NA), exclude = NULL)
  }
  data <- data.frame(
    g = na_factor(c("a", NA)),
    h = na_factor(c("p", NA)),
    n = 1:2
  )
  operation <- function(grouping) {
    summarize_with_margins(
      data,
      k = sum(n),
      .grouping = grouping,
      .margin_label = NA_character_
    )
  }

  singular <- expect_error(operation(rollup(g)))
  expect_s3_class(singular, "marginplyr_error")
  expect_identical(
    conditionMessage(singular),
    paste0(
      "`NA_character_` is already a factor level in grouping column `g`. ",
      "Use `NULL` for a typed-missing Margin label while preserving the NA ",
      "level."
    )
  )

  plural <- expect_error(operation(rollup(g, h)))
  expect_s3_class(plural, "marginplyr_error")
  expect_identical(
    conditionMessage(plural),
    paste0(
      "`NA_character_` is already a factor level in grouping columns `g`, ",
      "`h`. Use `NULL` for a typed-missing Margin label while preserving ",
      "the NA level."
    )
  )
})

# Both `kind` values are pinned, because the pluralized noun sits between words
# the kind chooses and a re-authoring reads the whole sentence rather than the
# branch. The last case is the builder's other label arm: labels that are not
# all one value replace the quoted label with a plural subject and its own
# verb, so it is the one place the plural spelling of that subject exists.
test_that("the margin label collision pluralizes its grouping column noun", {
  declared <- data.frame(
    a = factor(c("All", "x")),
    b = factor(c("All", "y")),
    n = 1:2
  )
  observed <- data.frame(a = c("All", "x"), b = c("All", "y"), n = 1:2)

  declared_singular <- expect_error(summarize_with_margins(
    declared,
    k = sum(n),
    .grouping = rollup(a),
    .margin_label = "All"
  ))
  expect_s3_class(declared_singular, "marginplyr_error")
  expect_identical(
    conditionMessage(declared_singular),
    paste0(
      "\"All\" is already a factor level in grouping column `a`. ",
      "Choose another `.margin_label`."
    )
  )

  declared_plural <- expect_error(summarize_with_margins(
    declared,
    k = sum(n),
    .grouping = rollup(a, b),
    .margin_label = "All"
  ))
  expect_s3_class(declared_plural, "marginplyr_error")
  expect_identical(
    conditionMessage(declared_plural),
    paste0(
      "\"All\" is already a factor level in grouping columns `a`, `b`. ",
      "Choose another `.margin_label`."
    )
  )

  observed_singular <- expect_error(summarize_with_margins(
    observed,
    k = sum(n),
    .grouping = rollup(a),
    .margin_label = "All",
    .check_margin_label = TRUE
  ))
  expect_s3_class(observed_singular, "marginplyr_error")
  expect_identical(
    conditionMessage(observed_singular),
    paste0(
      "\"All\" is already present in grouping column `a`. ",
      "Choose another `.margin_label` or set `.check_margin_label = FALSE`."
    )
  )

  observed_plural <- expect_error(summarize_with_margins(
    observed,
    k = sum(n),
    .grouping = rollup(a, b),
    .margin_label = "All",
    .check_margin_label = TRUE
  ))
  expect_s3_class(observed_plural, "marginplyr_error")
  expect_identical(
    conditionMessage(observed_plural),
    paste0(
      "\"All\" is already present in grouping columns `a`, `b`. ",
      "Choose another `.margin_label` or set `.check_margin_label = FALSE`."
    )
  )

  mixed_plural <- expect_error(summarize_with_margins(
    observed,
    k = sum(n),
    .grouping = rollup(a, b),
    .margin_label = c(a = "All", b = "y"),
    .check_margin_label = TRUE
  ))
  expect_s3_class(mixed_plural, "marginplyr_error")
  expect_identical(
    conditionMessage(mixed_plural),
    paste0(
      "Margin labels are already present in grouping columns `a`, `b`. ",
      "Choose another `.margin_label` or set `.check_margin_label = FALSE`."
    )
  )
})

test_that("the grouping helper refusal pluralizes its noun and its verb", {
  data <- data.frame(region = "E", grade = "A", n = 1)

  singular <- expect_error(summarize_with_margins(
    data,
    k = grouping_id(nowhere),
    .grouping = rollup(region)
  ))
  expect_s3_class(singular, "marginplyr_error")
  expect_identical(
    conditionMessage(singular),
    "Column `nowhere` is not part of `.by` or `.grouping`."
  )

  plural <- expect_error(summarize_with_margins(
    data,
    k = grouping_id(nowhere, nor),
    .grouping = rollup(region)
  ))
  expect_s3_class(plural, "marginplyr_error")
  expect_identical(
    conditionMessage(plural),
    "Columns `nowhere`, `nor` are not part of `.by` or `.grouping`."
  )
})

test_that("the summary overwrite refusal pluralizes its column noun", {
  data <- data.frame(region = "E", grade = "A", n = 1)

  singular <- expect_error(summarize_with_margins(
    data,
    region = sum(n),
    .grouping = rollup(region, grade)
  ))
  expect_s3_class(singular, "marginplyr_error")
  expect_identical(
    conditionMessage(singular),
    "Summary results cannot overwrite grouping column `region`."
  )

  plural <- expect_error(summarize_with_margins(
    data,
    region = sum(n),
    grade = sum(n),
    .grouping = rollup(region, grade)
  ))
  expect_s3_class(plural, "marginplyr_error")
  expect_identical(
    conditionMessage(plural),
    "Summary results cannot overwrite grouping columns `region`, `grade`."
  )
})
