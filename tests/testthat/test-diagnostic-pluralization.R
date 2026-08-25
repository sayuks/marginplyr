# The byte-exact record of eight diagnostics that pluralize a noun, both arms
# of each, and the gate that says which diagnostics have to be recorded
# somewhere. It is one file rather than an addition to each site's own test
# file because these pins are one artifact with one purpose: #223 re-authored
# these messages in the cli idiom file by file, and each of those PRs diffed
# against this baseline. Spread across the five files that raise them, a
# dropped arm would read as an ordinary deletion; here the set is visible at
# once, and a file-by-file re-authoring edited a contiguous block of it (#224).
#
# The rule deciding what belongs is about coverage and not about how this set
# came to be (#236):
#
#   Every diagnostic this package pluralizes has both arms reached by a test,
#   and the inflected span asserted in each.
#
# Three things follow that the rule it replaces could not say. It is stated
# over the property rather than over a construction, so `{?s}`, `{?is/are}`,
# `{?a/b}`, and the `if` a bare `stop()` invariant spells a plural with are all
# inside it -- what the baseline is for is that a re-wording of an inflection is
# visible, and the construction the inflection is written in does not change
# that. It does not require a byte-exact identity: the eight below achieve it
# that way and keep it, but `report_branch_warnings()` cannot, its message being
# a dplyr-rendered warning that carries the caller's own diagnostic, so an
# identity there would pin dplyr's aggregation text. And it says nothing about
# which file a pin lives in, which is the whole of why eight of the sixteen
# sites are here and the rest are pinned where they are raised.
#
# The rule it replaces read "suffixing is where this set came from rather than
# a test a ninth diagnostic is admitted by", which was right to refuse the
# question and wrong to leave it unanswered: it turned away the `cur_group*()`
# refusal in #246 and #223's own Definition of done in the same breath. The
# census beside it -- eight diagnostics, and a named list of two left out --
# is deleted rather than corrected, because under a coverage rule this file
# does not have to say what it omits. `pluralizing_coverage()` below says where
# each one went, and is checked against the corpus in both directions.
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

# Where both arms of each pluralizing diagnostic are pinned, keyed by the
# namespace binding that raises it, with one entry per site that binding holds.
#
# This is a table and not a list, which is the distinction `.github/scripts/
# verify-site.R` draws about its markers and `AGENTS.md` explains there: the
# set of things that must be covered is derived, and the table only says where
# each one went, so a table that fell behind fails rather than shrinking the
# question. What it cannot check is that a line reference still points at the
# assertion it names -- that is review surface, as a marker's text is.
#
# A binding holding more than one is why the value is a vector. The plan
# compiler's implementation, named as a key below, raises the
# duplicate-grouping-set refusal and the unknown-`.by` invariant, which are
# pinned in different places; the two label-collision constructors hold two
# arms each, all four pinned in the one block below.
#
# That name is written without its parentheses, here and in the key, because
# `test-grouping-plan.R` fails any test file whose source spells it as a call
# -- the scan behind "the only caller of the plan compiler" is deliberately
# blunt, and a key in a table is not a call.
pluralizing_coverage <- function() {
  list(
    # Both arms sit with the Arrow contracts rather than here, because what
    # each asserts is which summaries the refusal names, and that is decided
    # by whether Arrow's warning could be placed -- a property of the
    # behaviour, not of the wording. The plural arm is the backstop's, which
    # has no warning to read.
    abort_absorbed_summary = "test-grouping-backends.R:192 and :386",
    abort_by_rename = "test-grouping-plan.R:604 and :645",
    abort_declared_label_collision = rep("this file, the collision block", 2L),
    abort_grouping_rename = "test-grouping-plan.R:493 and :543",
    abort_observed_label_collision = rep("this file, the collision block", 2L),
    check_summary_context_helpers = "test-contextual-helpers.R:696 and :728",
    check_summary_group_overwrite = "this file, the summary-overwrite block",
    compile_grouping_spec_impl = c(
      "test-grouping-plan.R:915 and :768",
      "this file, the unknown `.by` invariant block"
    ),
    grouping_helper_vars = "this file, the grouping-helper block",
    proxy_columns = "this file, the selection proxy invariant block",
    # The one sentence that pluralizes outside a Package condition, and the one
    # covered by a fragment rather than an identity. ADR 0023's *Caller text is
    # a value* puts it in scope, phase 2b moved it to `cli::pluralize()`, and
    # #223's Definition of done asked for its arms here. They stay where they
    # are: what the fragments assert is the inflected noun and the count in
    # each arm, which is what the rule asks for, and an identity would pin the
    # aggregation dplyr rendered around it (#236).
    report_branch_warnings = "test-execution-conditions.R:332 and :291",
    validate_margin_label = "this file, the NA-level block",
    validate_margin_label_names = c(
      "this file, the fixed `.by` label block",
      "this file, the unknown label dimension block"
    )
  )
}

# The two readings the gate rests on, driven over source `R/` does not contain,
# for the reason `test-diagnostic-authoring.R` gives about its own fixtures:
# every site in the corpus satisfies the rule, so nothing but a fixture
# executes a refusing branch, and a predicate that stopped refusing anything
# reports the verdict a covered corpus reports.
#
# The corpus does execute both accepting branches -- twelve bindings between
# them -- so what these add is the refusals, and the two shapes each reader has
# to keep apart from the one it recognizes.
test_that("the readings behind the coverage gate", {
  cli_plural <- function() {
    abort_marginplyr("{cli::qty(n)}column{?s}:")
    abort_marginplyr(paste0("Can't rename ", "dimension{?s}:"))
    abort_marginplyr("A refusal naming no count.")
  }
  found <- diagnostic_message_arguments(body(cli_plural), "abort_marginplyr")
  # The third is the one a `{?}` test must not claim, and the second is the
  # one it would lose by reading a literal instead of the expression: a
  # re-authored template is split at a space, so the `{?}` can sit in either
  # half (ADR 0023's second amendment).
  expect_identical(
    vapply(found, has_cli_plural, logical(1)),
    c(TRUE, TRUE, FALSE)
  )

  invariants <- function(n) {
    stop("column", if (n == 1L) " " else "s ", call. = FALSE)
    stop("a flat invariant naming no count.", call. = FALSE)
    warning("column", if (n == 1L) " " else "s ")
  }
  # One `stop()` of the three, so a plain invariant is not claimed and neither
  # is a call that is not `stop()` at all.
  expect_identical(invariant_plural_sites(body(invariants)), 1L)

  # `cli::pluralize()` is written qualified and `abort_marginplyr()` is not, so
  # the head reader has to answer both spellings and refuse the wrong name.
  expect_true(is_call_to(quote(cli::pluralize("x")), "pluralize"))
  expect_true(is_call_to(quote(pluralize("x")), "pluralize"))
  expect_false(is_call_to(quote(cli::format_inline("x")), "pluralize"))
})

# The gate. A seventeenth pluralizing site fails until it is pinned and named
# here, and a site that stops pluralizing fails until its entry goes -- neither
# of which is a number anyone maintains, which is what the census this replaced
# turned out to be.
#
# Both directions are asserted because each catches what the other cannot: a
# derived site the table does not name is a diagnostic nothing may be asserting
# at all, and a table entry naming no derived site is a pin pointing at a
# message that no longer exists.
test_that("every pluralizing diagnostic is covered somewhere", {
  sites <- marginplyr_pluralizing_sites()
  declared <- pluralizing_coverage()

  # That the namespace was read at all, in the shape `test-diagnostic-
  # authoring.R` uses for the same reason: a scan that found no site reports
  # exactly what a corpus with nothing left to cover reports.
  expect_gt(sum(sites), 0L)

  expect_identical(setdiff(names(sites), names(declared)), character())
  expect_identical(setdiff(names(declared), names(sites)), character())
  expect_identical(
    lengths(declared[names(sites)]),
    sites
  )
})

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
