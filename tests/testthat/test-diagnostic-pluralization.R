# The byte-exact record of the eight diagnostics that pluralize a noun by
# suffixing it, both arms of each. It is one file rather than an addition to
# each site's own test file because these pins are one artifact with one
# purpose: #223 re-authors these messages in the cli idiom file by file, and
# each of those PRs diffs against this baseline. Spread across the five files
# that raise them, a dropped arm would read as an ordinary deletion; here the
# set is visible at once, and a file-by-file re-authoring edits a contiguous
# block of it (#224).
#
# Suffixing is what chose the eight, and two diagnostics were left out of them.
# One is still out for the reason it was: the duplicate-grouping-set refusal
# switches a whole phrase rather than suffixing one; both of its arms are
# pinned in `test-grouping-plan.R`, the plural one separately from this file
# because it needs a specification shape none of these eight do (#225).
#
# The other no longer has the construction it was described by. The
# renaming-selection refusal -- one caller mistake, written out once for
# `.grouping` and once for `.by` -- picked between a singular and a plural noun
# held in a labels list, and #223 dissolved that pair into `{?s}` when it
# re-authored `R/grouping-plan.R`, so it suffixes now as these eight do. Its
# reason for being out is untouched by that: all four of its messages are
# already pinned exactly in `test-grouping-plan.R`, so recording them again
# would buy nothing. That is a thinner reason than the one the paragraph below
# gives for recording three of these eight at both sites, and it is #224's to
# revisit: re-authoring how a diagnostic is spelled is not where the question
# of which diagnostics this baseline covers gets decided. Suffixing is
# therefore where this set came from rather than a test a ninth diagnostic is
# admitted by.
#
# The pins already beside each site stay where they are. Most match a phrase
# rather than a whole message, which is enough to assert that a caller reaches
# the diagnostic -- a different question from what it says -- and which
# survives the re-wording this file exists to make visible. Two are already
# identities: the singular arms of the `.by` invariant and of the
# summary-overwrite refusal. A third, the grouping-helper refusal's singular
# arm, matches its whole message but as a substring, so a re-wording fails it
# while an addition to the end would not. All three are recorded again here,
# because a baseline with holes in it is one a reader has to reassemble from
# five files before trusting it.
#
# Each Package condition is reached through the public verb that raises it, so
# a message that stops being reachable fails here rather than passing on an
# internal call that production no longer performs. The two internal invariants
# have no such path by construction -- that is what makes them invariants -- so
# they are called directly.
#
# Only those two assert a condition class. `design/architecture.md` keeps the
# `marginplyr_error` assertions next to the behaviour that raises them, paired
# with the External-condition half, so that the ADR 0015 boundary stays
# reviewable; collecting them here would be the single file that section rules
# out. Leaving them out loses nothing, because each of those assertions is
# still at its own site, which is where the boundary is read. The invariants
# assert the absence of that class rather than its presence; their own sites
# assert it too, and #224 asks for it here by name.
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
  expect_identical(
    conditionMessage(singular),
    paste0(
      "`.margin_label` must not name fixed `.by` column:\n",
      "i `region`."
    )
  )

  plural <- expect_error(expand_with_margins(
    data,
    .by = c(region, grade),
    .grouping = rollup(n),
    .margin_label = c(region = "A", grade = "A", n = "A")
  ))
  expect_identical(
    conditionMessage(plural),
    paste0(
      "`.margin_label` must not name fixed `.by` columns:\n",
      "i `region` and `grade`."
    )
  )
})

test_that("the unknown label dimension refusal pluralizes its name noun", {
  data <- data.frame(region = "E", grade = "A", n = 1)
  operation <- function(label) {
    expand_with_margins(data, .grouping = rollup(region), .margin_label = label)
  }

  singular <- expect_error(operation(c(region = "A", u = "A")))
  expect_identical(
    conditionMessage(singular),
    paste0(
      "`.margin_label` has unknown dimension name:\n",
      "i `u`."
    )
  )

  plural <- expect_error(operation(c(region = "A", u = "A", v = "A")))
  expect_identical(
    conditionMessage(plural),
    paste0(
      "`.margin_label` has unknown dimension names:\n",
      "i `u` and `v`."
    )
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
  expect_identical(
    conditionMessage(singular),
    paste0(
      "`NA_character_` is already a factor level in grouping column:\n",
      "i `g`.\n",
      "i Use `NULL` for a typed-missing Margin label while preserving the NA ",
      "level."
    )
  )

  plural <- expect_error(operation(rollup(g, h)))
  expect_identical(
    conditionMessage(plural),
    paste0(
      "`NA_character_` is already a factor level in grouping columns:\n",
      "i `g` and `h`.\n",
      "i Use `NULL` for a typed-missing Margin label while preserving the NA ",
      "level."
    )
  )
})

# Both collision kinds are pinned, and #223's re-authoring of this file made
# the reason a stronger one rather than retiring it. The pluralized noun sat
# between words the kind chose, so reading the whole sentence was what a
# re-wording called for; now each kind is a template of its own, and the
# pluralized column noun, the clause naming the collision, and the remedy are
# each written out once per kind, so neither arm's pin reads the other's words
# at all. The mixed cases are each builder's other subject arm: labels that are
# not all one value replace the quoted label with a plural subject carrying its
# own verb, and under the declared kind a second noun pluralizes with it. That
# arm exists under both kinds and is plural-only, since two distinct label
# values need two columns, so both are recorded and neither stands in for the
# other.
test_that("the margin label collision pluralizes its grouping column noun", {
  declared <- data.frame(
    a = factor(c("All", "x")),
    b = factor(c("All", "y")),
    n = 1:2
  )
  observed <- data.frame(a = c("All", "x"), b = c("All", "y"), n = 1:2)
  mixed <- c(a = "All", b = "y")
  declare <- function(grouping, label) {
    summarize_with_margins(
      declared,
      k = sum(n),
      .grouping = grouping,
      .margin_label = label
    )
  }
  observe <- function(grouping, label) {
    summarize_with_margins(
      observed,
      k = sum(n),
      .grouping = grouping,
      .margin_label = label,
      .check_margin_label = TRUE
    )
  }

  declared_singular <- expect_error(declare(rollup(a), "All"))
  expect_identical(
    conditionMessage(declared_singular),
    paste0(
      "\"All\" is already a factor level in grouping column:\n",
      "i `a`.\n",
      "i Choose another `.margin_label`."
    )
  )

  declared_plural <- expect_error(declare(rollup(a, b), "All"))
  expect_identical(
    conditionMessage(declared_plural),
    paste0(
      "\"All\" is already a factor level in grouping columns:\n",
      "i `a` and `b`.\n",
      "i Choose another `.margin_label`."
    )
  )

  declared_mixed <- expect_error(declare(rollup(a, b), mixed))
  expect_identical(
    conditionMessage(declared_mixed),
    paste0(
      "Margin labels are already factor levels in grouping columns:\n",
      "i `a` and `b`.\n",
      "i Choose another `.margin_label`."
    )
  )

  observed_singular <- expect_error(observe(rollup(a), "All"))
  expect_identical(
    conditionMessage(observed_singular),
    paste0(
      "\"All\" is already present in grouping column:\n",
      "i `a`.\n",
      "i Choose another `.margin_label` or set ",
      "`.check_margin_label = FALSE`."
    )
  )

  observed_plural <- expect_error(observe(rollup(a, b), "All"))
  expect_identical(
    conditionMessage(observed_plural),
    paste0(
      "\"All\" is already present in grouping columns:\n",
      "i `a` and `b`.\n",
      "i Choose another `.margin_label` or set ",
      "`.check_margin_label = FALSE`."
    )
  )

  observed_mixed <- expect_error(observe(rollup(a, b), mixed))
  expect_identical(
    conditionMessage(observed_mixed),
    paste0(
      "Margin labels are already present in grouping columns:\n",
      "i `a` and `b`.\n",
      "i Choose another `.margin_label` or set ",
      "`.check_margin_label = FALSE`."
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
  expect_identical(
    conditionMessage(singular),
    paste0(
      "Column is not part of `.by` or `.grouping`:\n",
      "i `nowhere`."
    )
  )

  plural <- expect_error(summarize_with_margins(
    data,
    k = grouping_id(nowhere, nor),
    .grouping = rollup(region)
  ))
  expect_identical(
    conditionMessage(plural),
    paste0(
      "Columns are not part of `.by` or `.grouping`:\n",
      "i `nowhere` and `nor`."
    )
  )
})

test_that("the summary overwrite refusal pluralizes its column noun", {
  data <- data.frame(region = "E", grade = "A", n = 1)

  singular <- expect_error(summarize_with_margins(
    data,
    region = sum(n),
    .grouping = rollup(region, grade)
  ))
  expect_identical(
    conditionMessage(singular),
    paste0(
      "Summary results cannot overwrite grouping column:\n",
      "i `region`."
    )
  )

  plural <- expect_error(summarize_with_margins(
    data,
    region = sum(n),
    grade = sum(n),
    .grouping = rollup(region, grade)
  ))
  expect_identical(
    conditionMessage(plural),
    paste0(
      "Summary results cannot overwrite grouping columns:\n",
      "i `region` and `grade`."
    )
  )
})
