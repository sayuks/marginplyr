# The reproduction #141 was filed with. `as.numeric(grade)` is an ordinary
# coercion warning rather than anything marginplyr or dplyr raises, so what
# these tests hold is the treatment of an External condition and not the
# behaviour of one particular diagnostic.
coercion_frame <- function() {
  data.frame(
    region = c("East", "East", "West"),
    grade = c("a", "b", "a"),
    units = c(1, 3, 6)
  )
}

# The ticket's own reproduction: four grouping sets, each raising the same
# coercion warning. Wrapped in a function so that the tests below share one
# reading of it; the Margin verb is still the call each condition reports,
# because a verb captures its own call rather than its caller's.
summarize_coercion_cube <- function() {
  # `region` and `grade` are columns of the frame, which codetools reads as
  # undefined globals wherever a verb's arguments are written inside a
  # function.
  # nolint start: object_usage_linter.
  summarize_with_margins(
    coercion_frame(),
    total = sum(as.numeric(grade)),
    .grouping = cube(region, grade)
  )
  # nolint end
}

# The rewrite #199 is about. `resolve_summary_selections()` turns the caller's
# `c(grade)` into `dplyr::all_of("grade")` before either adapter runs, so dplyr
# quotes a call the caller never typed. `grade` is summarized here rather than
# made a dimension, because the selection a Margin summary resolves is over the
# columns no grouping set holds.
summarize_across_coercion <- function() {
  # `grade` and `region` are columns of the frame, which codetools reads as
  # undefined globals wherever a verb's arguments are written inside a
  # function.
  # nolint start: object_usage_linter.
  summarize_with_margins(
    coercion_frame(),
    dplyr::across(c(grade), ~ sum(as.numeric(.x))),
    .grouping = rollup(region)
  )
  # nolint end
}

# A share is where the labels slide. `plan_share_expressions()` replaces the
# across-share dot with one dot per output, so the warning raised by the dot
# after it sits at a position the caller's dots do not have -- and a label read
# by position there describes another argument, or none.
summarize_share_coercion <- function() {
  data <- coercion_frame()
  data$revenue <- c(2, 4, 8)
  # `units`, `revenue`, `grade`, and `region` are columns of the frame, which
  # codetools reads as undefined globals inside a function.
  # nolint start: object_usage_linter.
  summarize_with_margins(
    data,
    dplyr::across(c(units, revenue), sum),
    dplyr::across(c(units, revenue), share_of_parent, .names = "{.col}_share"),
    dplyr::across(c(grade), ~ sum(as.numeric(.x))),
    .grouping = rollup(region)
  )
  # nolint end
}

# The rewrite that differs between branches rather than being shared by them:
# `grouping_bit(region)` is `0L` in the detail set and `1L` in the Grand total
# set, so dplyr quoted a different argument in each and the deduplication key
# saw two conditions where the caller wrote one expression. `grouping_bit()`
# is a Contextual helper, and the constant is the branch-local rewrite
# `rewrite_grouping_dots()` gives it.
summarize_helper_coercion <- function() {
  # `grade` and `region` are columns of the frame, which codetools reads as
  # undefined globals inside a function.
  # nolint start: object_usage_linter.
  summarize_with_margins(
    coercion_frame(),
    total = sum(as.numeric(grade)) + grouping_bit(region),
    .grouping = rollup(region)
  )
  # nolint end
}

# The same rewrite reaching the caller as an error rather than a warning. What
# the two paths share is the map; what they do not is where the bullet sits --
# `$message` is the whole of it here, and one line of a rendered text there.
summarize_across_failure <- function() {
  # `grade` and `region` are columns of the frame, which codetools reads as
  # undefined globals inside a function.
  # nolint start: object_usage_linter.
  summarize_with_margins(
    coercion_frame(),
    dplyr::across(c(grade), ~ sum(nope(.x))),
    .grouping = rollup(region)
  )
  # nolint end
}

# #108's reproduction: an error from the caller's own expression, which aborts
# the first branch that raises it.
summarize_failing_rollup <- function() {
  summarize_with_margins(
    data.frame(g = c("a", "b"), v = c(1, 2)),
    x = stop("my error"),
    .grouping = rollup(dplyr::all_of("g"))
  )
}

# The native grouping-sets adapter's own reproduction. `simulate_postgres()`
# holds that capability and needs no optional backend, dbplyr being an Import,
# so the native path is asserted wherever the suite runs. What the adapter
# hands dplyr splices a SQL literal whose own deparse overflows the width
# `as_label()` deparses at, so the label dbplyr's error quotes collapses to
# `+...` for any dot combining a helper with anything else (#410).
native_grouping_sets_input <- function() {
  dbplyr::tbl_lazy(
    data.frame(a = c("x", "y"), value = c(1, 2)),
    con = dbplyr::simulate_postgres()
  )
}

# A summary expression dbplyr cannot translate, on that input.
native_translation_failure <- function() {
  # `a`, `value`, and `no_such_column` are read from the lazy input, which
  # codetools reads as undefined globals wherever a verb's arguments are
  # written inside a function.
  # nolint start: object_usage_linter.
  summarize_with_margins(
    native_grouping_sets_input(),
    total = sum(value) + grouping_bit(a) + no_such_column,
    .grouping = rollup(a)
  )
  # nolint end
}

# The same failure under two dots the caller spelled differently and named the
# same. Both collapse to one label, so the span says which expression raised
# the error but not which argument did.
#
# The name is what makes the two collide, unnamed dots no longer being able to:
# each carries the caller's own expression as its name since #430, so two dots
# the caller spelled differently label differently as well.
native_shared_label_failure <- function() {
  # nolint start: object_usage_linter.
  summarize_with_margins(
    native_grouping_sets_input(),
    x = grouping_bit(a) + no_such_column,
    x = grouping_bit(a) + value,
    .grouping = rollup(a)
  )
  # nolint end
}

# One plan whose two grouping sets raise whatever the caller asks them to.
# `dplyr::n()` is 3 only in the grouping set that groups by nothing, which
# `rollup()` runs second, so `whole` is the later branch and `fail` makes it
# abort the operation after the earlier one has already warned.
summarize_branch_diagnostics <- function(whole, detail, fail = FALSE) {
  summarize_with_margins(
    coercion_frame(),
    total = {
      if (dplyr::n() == 3L) {
        if (fail) stop("aborting branch") else warning(whole)
      } else {
        warning(detail)
      }
      0
    },
    .grouping = rollup(dplyr::all_of("region"))
  )
}

# Every warning a call raises, not just the first: the whole subject here is
# how many times one condition reaches the caller, which `expect_warning()`
# cannot report.
collect_warnings <- function(expr) {
  warnings <- list()
  withCallingHandlers(
    expr,
    warning = function(cnd) {
      warnings[[length(warnings) + 1L]] <<- cnd
      invokeRestart("muffleWarning")
    }
  )
  warnings
}

# Every rendering decision cli makes for itself, as the configurations they
# combine into. The identity is over the message as written, so none of them may
# change how many conditions a caller receives -- and a suite green under one of
# them says nothing about the others, which is the whole of #217: the width was
# the only one asserted, and the contract held under neither of the other two.
#
# Generated rather than listed, so a fourth variable is one column here and
# every assertion below picks it up rather than being extended to meet it.
# Each configuration is named for itself, because these are asserted as one
# named vector per property: a failure then says which renderings broke and
# which held, where twenty separate expectations would say only that one did.
rendering_configurations <- function() {
  grid <- expand.grid(
    width = c(80L, 60L, 40L, 20L, 16L),
    num_colors = c(1L, 256L),
    hyperlink = c(FALSE, TRUE),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  configs <- lapply(
    split(grid, seq_len(nrow(grid))),
    function(row) rendering_config(row$width, row$num_colors, row$hyperlink)
  )
  stats::setNames(configs, vapply(configs, rendering_label, character(1)))
}

# The one shape a configuration has, so that a test naming a rendering the grid
# does not cover -- a width wide enough that nothing wraps -- builds the same
# thing the grid does rather than a second shape the helpers must also accept.
rendering_config <- function(width, num_colors, hyperlink) {
  list(width = width, num_colors = num_colors, hyperlink = hyperlink)
}

rendering_label <- function(config) {
  sprintf(
    "width %d, num_colors %d, hyperlink %s",
    config$width,
    config$num_colors,
    config$hyperlink
  )
}

# `expr` is forced inside `collect_warnings()`, so it is rendered under the
# options set here. `cli.condition_width` is the one that has to be set for the
# width: testthat's own `local_reproducible_output()` sets it to `Inf` so that a
# snapshot does not depend on the pane it was recorded in, and rlang consults it
# ahead of `cli.width` -- so a test setting `cli.width` alone would render every
# case unwrapped and pass whatever the code did. `cli.hyperlink_run` is the one
# dplyr's pointer at `last_dplyr_warnings()` follows; `cli.hyperlink` is set
# beside it because a terminal advertising one advertises the other, so a
# configuration separating them would name a session nobody runs.
collect_warnings_rendered <- function(config, expr) {
  original <- options(
    cli.width = config$width,
    cli.condition_width = config$width,
    cli.num_colors = config$num_colors,
    cli.hyperlink = config$hyperlink,
    cli.hyperlink_run = config$hyperlink
  )
  on.exit(options(original), add = TRUE)
  collect_warnings(expr)
}

# The styling on its own, for the two properties below that are about the
# reading rather than about a rendering: neither varies with the width or the
# hyperlinks, so crossing them would assert one thing twenty times.
at_num_colors <- function(num_colors, expr) {
  original <- options(cli.num_colors = num_colors)
  on.exit(options(original), add = TRUE)
  expr
}

# One call's warnings under each configuration, keyed by the configuration.
# `expr` is quoted rather than taken as a promise, because a promise forces
# once and every configuration has to render the call again.
warnings_under_every_rendering <- function(expr) {
  quoted <- substitute(expr)
  env <- parent.frame()
  lapply(
    rendering_configurations(),
    function(config) {
      collect_warnings_rendered(config, eval(quoted, env))
    }
  )
}

# Whether each configuration's reported message carries a literal, keyed by the
# configuration, so that what a failure names is the rendering and not the
# expectation's position in a loop. A configuration that reported nothing reads
# as `""` rather than raising: the count is asserted separately, and reading
# past the end of an empty result would replace that assertion's failure with an
# error.
reported_contains <- function(collected, text) {
  vapply(
    collected,
    function(warnings) {
      message <- if (length(warnings) == 0L) {
        ""
      } else {
        conditionMessage(warnings[[1L]])
      }
      grepl(text, message, fixed = TRUE)
    },
    logical(1)
  )
}

# What stops a configuration passing by not being the configuration it claims.
# cli decides for itself whether to style and whether to link, so an environment
# that refused would turn every colour row into a second unstyled one: a suite
# still green while asserting nothing about the case those rows exist for. This
# is asserted beside the collapse rather than inferred from it, for the reason
# `verify-suite-coverage.R` asserts its own mechanism before concluding
# anything.
#
# It is also what pins the other half of ADR 0022's contract: only a restated
# line is rendered plain, so a reported message still carries the styling of
# every line the restatement did not touch, and a colour row that arrived
# unstyled would mean the whole message had been rewritten.
expect_rendering_markers <- function(collected) {
  configs <- rendering_configurations()

  expect_identical(
    reported_contains(collected, "\033["),
    vapply(configs, function(config) config$num_colors > 1L, logical(1))
  )
  expect_identical(
    reported_contains(collected, "\033]8;;"),
    vapply(configs, function(config) config$hyperlink, logical(1))
  )
}

# The value every configuration has to agree on, shaped like the vectors the
# assertions compare against it.
for_every_rendering <- function(value) {
  configs <- rendering_configurations()
  stats::setNames(rep(value, length(configs)), names(configs))
}

test_that("a warning repeated across grouping sets is reported once", {
  warnings <- collect_warnings(summarize_coercion_cube())

  expect_length(warnings, 1L)
  message <- conditionMessage(warnings[[1L]])
  expect_match(message, "NAs introduced by coercion", fixed = TRUE)
  # Four grouping sets raise it, so three further ones follow the one reported.
  expect_match(message, "3 further grouping sets", fixed = TRUE)
  # The condition itself is the caller's to receive unchanged.
  expect_s3_class(warnings[[1L]], "rlang_warning")
})

test_that("a reported warning names the caller's own grouping columns", {
  warnings <- collect_warnings(summarize_coercion_cube())

  message <- conditionMessage(warnings[[1L]])
  expect_match(message, "`region = \"East\"`", fixed = TRUE)
  expect_match(message, "`grade = \"a\"`", fixed = TRUE)
  expect_false(grepl("marginplyr_key", message, fixed = TRUE))
})

test_that("a share does not shift which argument is restored", {
  warnings <- collect_warnings(summarize_share_coercion())

  expect_length(warnings, 1L)
  message <- conditionMessage(warnings[[1L]])
  expect_match(
    message,
    "`dplyr::across(c(grade), ~sum(as.numeric(.x)))`",
    fixed = TRUE
  )
  expect_false(grepl("all_of", message, fixed = TRUE))
})

# Restoring the spelling is what makes this one report, so the identity is read
# after the restatement and not before (ADR 0022). Which grouping set produced
# an occurrence is not part of an identity, and `grouping_bit()`'s branch
# constant is that same fact reaching the argument bullet.
test_that("a warning is one report where only a branch constant differed", {
  warnings <- collect_warnings(summarize_helper_coercion())

  expect_length(warnings, 1L)
  message <- conditionMessage(warnings[[1L]])
  expect_match(
    message,
    "`total = sum(as.numeric(grade)) + grouping_bit(region)`",
    fixed = TRUE
  )
  expect_match(message, "1 further grouping set", fixed = TRUE)
})

# Three rendering variables, one property, which ADR 0021's *No rendering
# decision takes part in the identity* is authoritative for. What this fixture
# contributes is the counts each variable produced on it: three reports of one
# condition at 60 columns, four at 40, and two anywhere in 15 to 24; two above
# `cli.num_colors = 1`, quoting the branch constant `0L` the caller never
# wrote; and two again under `cli.hyperlink_run` at `cli.num_colors = 1`.
#
# Asserting the restoration here as well as the collapse is what says #199's
# restatement is covered rather than leaving it inferred from the count: the two
# read the same message through the same helper, and in a colour session both
# no-opped.
test_that("the constant-rewrite collapse holds under every rendering", {
  collected <- warnings_under_every_rendering(summarize_helper_coercion())

  expect_rendering_markers(collected)
  expect_identical(lengths(collected), for_every_rendering(1L))
  expect_identical(
    reported_contains(collected, "grouping_bit(region)"),
    for_every_rendering(TRUE)
  )
  expect_identical(
    reported_contains(collected, "+ 0L"),
    for_every_rendering(FALSE)
  )
})

# The other rewrite #199 restores, under the same configurations: a selection
# the caller wrote as `c(grade)` reaches dplyr as `dplyr::all_of("grade")`.
# Unlike the branch constant, this one is shared by every branch, so it never
# split an identity -- which is why it needs its own assertion. A colour session
# quoted the rewrite here while reporting the expected single condition, and a
# test reading the count alone would have called that green.
#
# The third part of #141's context leak (ADR 0022). What is quoted is
# `rlang::as_label()`'s rendering of the expression the caller wrote rather than
# their source text, because that is the rendering dplyr would have quoted had
# nothing been rewritten -- the spelling restored is the caller's, not their
# whitespace. This subsumes the single-rendering assertion that stood here
# before, which read the same fixture for the same two facts under one
# rendering.
test_that("the selection rewrite is restored under every rendering", {
  spelled <- "`dplyr::across(c(grade), ~sum(as.numeric(.x)))`"
  collected <- warnings_under_every_rendering(summarize_across_coercion())

  expect_rendering_markers(collected)
  expect_identical(lengths(collected), for_every_rendering(1L))
  expect_identical(
    reported_contains(collected, spelled),
    for_every_rendering(TRUE)
  )
  expect_identical(
    reported_contains(collected, "all_of"),
    for_every_rendering(FALSE)
  )
})

# The degradation the contract requires, exercised through a real call rather
# than described: dplyr abbreviates a long infix expression to `+...`, which
# equals no label marginplyr rendered, so the quotation stays as dplyr wrote it.
# What goes away is the restoration, never the report.
#
# Asserting `+...` couples this to an abbreviation ADR 0022 declines to
# reproduce, and does so deliberately: it is the fixture's premise rather than
# a promise about dplyr. A dplyr that stopped abbreviating would restore the
# spelling here and fail this loudly, which is the report that the case no
# longer reproduces; a test asserting only the degradation would pass by then
# while exercising nothing.
#
# The second case is why reproducing dplyr's abbreviation would buy nothing
# (ADR 0022): the same truncation removes `grouping_bit()`'s branch constant,
# so the branches agree on an identity without any restoration.
test_that("an abbreviated argument keeps the quotation dplyr wrote", {
  data <- coercion_frame()

  plain <- collect_warnings(summarize_with_margins(
    data,
    total = sum(as.numeric(grade)) +
      0 * (sum(units) + sum(units) + sum(units) + sum(units) + sum(units)),
    .grouping = rollup(region)
  ))
  with_helper <- collect_warnings(summarize_with_margins(
    data,
    total = sum(as.numeric(grade)) + grouping_bit(region) +
      0 * (sum(units) + sum(units) + sum(units) + sum(units) + sum(units)),
    .grouping = rollup(region)
  ))

  expect_length(plain, 1L)
  expect_match(conditionMessage(plain[[1L]]), "`total = +...`", fixed = TRUE)
  expect_length(with_helper, 1L)
  expect_match(
    conditionMessage(with_helper[[1L]]),
    "1 further grouping set",
    fixed = TRUE
  )
})

test_that("a repeated warning is one report under every rendering", {
  collected <- warnings_under_every_rendering(summarize_coercion_cube())

  expect_rendering_markers(collected)
  expect_identical(lengths(collected), for_every_rendering(1L))
  expect_identical(
    reported_contains(collected, "3 further grouping sets"),
    for_every_rendering(TRUE)
  )
})

# A test that only asserted collapsing would pass if everything collapsed, so
# the plan whose branches raise genuinely different diagnostics is asserted
# beside it. `dplyr::n()` is 3 only in the grouping set that groups by nothing.
test_that("branches raising different diagnostics are reported separately", {
  warnings <- collect_warnings(summarize_branch_diagnostics(
    "whole table",
    "one region"
  ))

  messages <- vapply(warnings, conditionMessage, character(1))
  expect_length(warnings, 2L)
  expect_match(messages, "whole table", fixed = TRUE, all = FALSE)
  expect_match(messages, "one region", fixed = TRUE, all = FALSE)
  expect_false(any(grepl("further grouping set", messages, fixed = TRUE)))
})

# What a grouping value or a caller diagnostic says is data, and none of it may
# decide what gets removed from an identity. Each case below reproduced a
# collapse or a split when the removal read a marker anywhere in the message
# rather than at the start of a line: `Hawaii Region` carries `i `, and the
# last pair are two diagnostics that differ only where a marker made the
# difference invisible.
test_that("a marker inside a value or a diagnostic decides nothing", {
  regions <- c("Hawaii Region", "Hawaii Region", "West Region")
  data <- coercion_frame()
  data$region <- regions

  expect_length(
    collect_warnings(summarize_with_margins(
      data,
      total = sum(as.numeric(grade)),
      .grouping = cube(region, grade)
    )),
    1L
  )
  expect_length(
    collect_warnings(summarize_branch_diagnostics(
      "i In group A is bad",
      "i In group B is bad"
    )),
    2L
  )
  expect_length(
    collect_warnings(summarize_branch_diagnostics(
      "bad value\nin data",
      "bad value in data"
    )),
    2L
  )
  # cli renders the second line of a caller's diagnostic at column zero, where
  # a marker on it is indistinguishable from one dplyr wrote.
  expect_length(
    collect_warnings(summarize_branch_diagnostics(
      "bad value\ni In group A",
      "bad value\ni In group B"
    )),
    2L
  )
  expect_length(
    collect_warnings(summarize_branch_diagnostics(
      "bad value\ni Run `dplyr::last_dplyr_warnings()` A",
      "bad value\ni Run `dplyr::last_dplyr_warnings()` B"
    )),
    2L
  )
  # An indented line of the caller's own is what a wrap looks like, so nothing
  # may rejoin one: rewriting a line that is kept is what turns two diagnostics
  # into one.
  expect_length(
    collect_warnings(summarize_branch_diagnostics(
      "bad value\n  in data",
      "bad value in data"
    )),
    2L
  )
})

# dplyr appends its pointer only at the end of a message whose header said there
# was more than one warning, and a caller's own text may spell that line too.
# The plan below raises one warning in a branch of two groups, where dplyr
# appends the pointer, and in a branch of one, where it appends none -- so a
# removal that reads the caller's line as dplyr's computes a different identity
# in each and reports the warning twice (#341). Nothing else in the suite
# reaches this: the pair above differ, so they are two reports either way.
test_that("a caller's own pointer line does not split one warning", {
  text <- "bad value\ni Run `dplyr::last_dplyr_warnings()` tail"
  collected <- warnings_under_every_rendering(
    summarize_branch_diagnostics(text, text)
  )

  expect_rendering_markers(collected)
  expect_identical(lengths(collected), for_every_rendering(1L))
  expect_identical(
    reported_contains(collected, "1 further grouping set"),
    for_every_rendering(TRUE)
  )
  # The caller's line is no part of what the identity removes, so the report
  # still carries its tail. The word alone is what every width can be asked
  # for: the line it sits on wraps at the narrow ones.
  expect_identical(
    reported_contains(collected, "tail"),
    for_every_rendering(TRUE)
  )
})

# ADR 0022's contract for a restated line, asserted line by line rather than in
# aggregate. `expect_rendering_markers()` above says only that some marker
# survived somewhere, which a message that had lost the styling on one other
# line would still satisfy; what the contract promises is which line is plain.
# The width is set wide enough that nothing wraps, so a line here is a line as
# dplyr wrote it.
test_that("only the line a restatement rewrote is rendered plain", {
  config <- rendering_config(200L, 256L, FALSE)
  warnings <- collect_warnings_rendered(config, summarize_helper_coercion())
  lines <- strsplit(conditionMessage(warnings[[1L]]), "\n", fixed = TRUE)[[1L]]
  styled <- grepl("\033[", lines, fixed = TRUE)

  restated <- grepl("In argument:", lines, fixed = TRUE)
  expect_identical(sum(restated), 1L)
  expect_false(any(styled[restated]))
  # Every other bullet dplyr wrote keeps the styling it arrived with, including
  # the two the identity removed from its key -- what a key drops is not what a
  # caller stops being shown.
  expect_true(all(styled[grepl("In group ", lines, fixed = TRUE)]))
  expect_true(all(styled[grepl("last_dplyr_warnings", lines, fixed = TRUE)]))
})

# Removing the styling is a change to a *reading*, and only to a reading: the
# identity is still assembled from the lines as they arrived. Two diagnostics
# differing only by an escape sequence read alike once the styling is off, so an
# identity computed over the stripped text would collapse them -- a caller's own
# diagnostic losing the difference between two conditions, which is the one
# outcome the removal may not produce.
test_that("a diagnostic differing only by an escape sequence stays distinct", {
  reports <- vapply(
    c("1" = 1L, "256" = 256L),
    function(num_colors) {
      at_num_colors(num_colors, length(collect_warnings(
        summarize_branch_diagnostics("bad \033[36mvalue\033[39m", "bad value")
      )))
    },
    integer(1)
  )

  expect_identical(reports, c("1" = 2L, "256" = 2L))
})

# The reading is shared between the two condition kinds, so changing it for the
# warning path changes the error path too. An error's context is separately
# addressable and carries no styling at all -- rlang holds the bare bullets and
# cli formats them at print -- so the removal is a no-op here. That is asserted
# rather than left to the warning tests, for the reason those tests now cross
# the rendering variables at all: a green result under one styling says nothing
# about the other.
test_that("a branch error's restatement carries no styling and does not vary", {
  restated <- vapply(
    c("1" = 1L, "256" = 256L),
    function(num_colors) {
      error <- at_num_colors(
        num_colors,
        tryCatch(summarize_across_failure(), error = function(cnd) cnd)
      )
      unname(error$message)
    },
    character(1)
  )

  expect_identical(restated[["1"]], restated[["256"]])
  expect_false(any(grepl("\033", restated, fixed = TRUE)))
  expect_match(
    restated,
    "`dplyr::across(c(grade), ~sum(nope(.x)))`",
    fixed = TRUE,
    all = TRUE
  )
})

# Withholding a warning and then leaving by a path that never replays it would
# lose it outright, which is worse than the repetition this ticket is about.
# `rollup()` runs the detail set before the Grand total set, so the warning is
# raised and buffered before the error aborts the operation.
test_that("a warning a branch raised survives a later branch's error", {
  reported <- NULL
  error <- expect_error(withCallingHandlers(
    summarize_branch_diagnostics("unreached", "early warning", fail = TRUE),
    warning = function(cnd) {
      reported <<- c(reported, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  ))

  expect_match(conditionMessage(error), "aborting branch", fixed = TRUE)
  expect_length(reported, 1L)
  expect_match(reported[[1L]], "early warning", fixed = TRUE)
})

# Asserted on the seam rather than through a verb, because neither case can be
# produced through one: dplyr aggregates a branch's warnings into one condition
# of its own before signalling, so the class it carries is always
# `rlang_warning` and its message is never empty. The identity is stated over
# the class as well as the diagnostic (CONTEXT.md, *Repeated condition*), and a
# warning that reached this without dplyr's aggregation must get a cause rather
# than an error -- replacing an External condition with one of marginplyr's own
# is the one outcome the contract rules out.
test_that("a warning's identity covers its class and admits an empty message", {
  text <- "There was 1 warning in `dplyr::summarize()`.\n! NAs introduced"

  expect_false(identical(
    branch_warning_identity(rlang::warning_cnd("one_class", message = text)),
    branch_warning_identity(rlang::warning_cnd("other_class", message = text))
  ))
  expect_identical(
    branch_warning_identity(rlang::warning_cnd("one_class", message = text)),
    branch_warning_identity(rlang::warning_cnd("one_class", message = text))
  )
  expect_type(
    branch_warning_identity(rlang::warning_cnd("one_class", message = "")),
    "character"
  )
})

# #108's reproduction. An error aborts the first branch that raises it, so
# there is nothing to deduplicate; what it needs is the context.
test_that("a branch error reports the caller's column, group, and verb", {
  error <- expect_error(summarize_failing_rollup())

  message <- conditionMessage(error)
  expect_match(message, "In group 1: `g = \"a\"`", fixed = TRUE)
  expect_false(grepl("marginplyr_key", message, fixed = TRUE))
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "summarize_with_margins"
  )
})

test_that("a branch error quotes the caller's own spelling", {
  error <- expect_error(summarize_across_failure())

  message <- conditionMessage(error)
  # The trailing period is part of the assertion: eager dplyr's sentence
  # carries one, and the rebuild puts back the punctuation it found rather
  # than dropping or doubling it.
  expect_match(
    message,
    "`dplyr::across(c(grade), ~sum(nope(.x)))`.",
    fixed = TRUE
  )
  expect_false(grepl("all_of", message, fixed = TRUE))
  # The condition itself is still the caller's, restated context and all.
  expect_s3_class(error$parent, "condition")
})

# The one condition the native adapter raises while the verb runs, and the
# only part of ADR 0022 that reaches it: an error dbplyr raises translating the
# rewritten expression. Both directions are asserted, because a restoration
# that quietly stopped happening reads exactly like a package whose contexts
# were all still faithful.
test_that("a native translation error quotes the caller's own spelling", {
  error <- expect_error(native_translation_failure())

  message <- conditionMessage(error)
  # The trailing newline is part of the assertion: dbplyr's sentence carries no
  # period, and the restatement puts back the punctuation it found rather than
  # a period of its own.
  expect_match(
    message,
    "In argument: `total = sum(value) + grouping_bit(a) + no_such_column`\n",
    fixed = TRUE
  )
  expect_false(grepl("+...", message, fixed = TRUE))
  # The condition itself is still dbplyr's, restated context and all.
  expect_s3_class(error$parent, "condition")
})

test_that("a native label two dots share is left as dplyr wrote it", {
  error <- expect_error(native_shared_label_failure())

  # `branch_argument_map()` finds no single candidate for the shared label and
  # drops the entry, so dplyr's own quotation stands.
  expect_match(
    conditionMessage(error),
    "In argument: `x = +...`",
    fixed = TRUE
  )
})

test_that("a propagated error keeps its class, diagnostic, and cause", {
  error <- expect_error(summarize_failing_rollup())

  expect_s3_class(error, "rlang_error")
  expect_false(inherits(error, "marginplyr_error"))
  expect_s3_class(error$parent, "simpleError")
  expect_identical(conditionMessage(error$parent), "my error")
  expect_match(conditionMessage(error), "Caused by error", fixed = TRUE)
})

# The hazard a fixed `gsub()` per key carries: replacing `..marginplyr_key_1`
# before `..marginplyr_key_10` leaves the caller's first column name followed
# by a stray `0`. Ten dimensions in one grouping set is the cheapest plan that
# allocates both tokens.
test_that("ten grouping columns substitute without corrupting a name", {
  columns <- c(
    "region", "grade", "store", "channel", "segment",
    "brand", "month", "city", "country", "tier"
  )
  data <- as.data.frame(lapply(
    rlang::set_names(columns),
    function(column) c("x", "y")
  ))
  data$value <- c(1, 2)

  error <- expect_error(summarize_with_margins(
    data,
    x = stop("my error"),
    .grouping = grouping_sets(dplyr::all_of(columns))
  ))

  # Ten grouping values do not fit on one line at any ordinary width, and where
  # cli wraps the bullet is not what this test is about.
  message <- gsub("[[:space:]]+", " ", conditionMessage(error))
  expect_false(grepl("marginplyr_key", message, fixed = TRUE))
  for (column in columns) {
    expect_match(message, paste0("`", column, " = \"x\"`"), fixed = TRUE)
  }
  # A naive replacement leaves `region0` where the tenth column belongs.
  expect_false(grepl("region0", message, fixed = TRUE))
})

# What the verb rewrites is the Condition context dplyr built from marginplyr's
# query. A Package condition carries its own context and is not an External
# condition at all, so nothing here may reach it.
test_that("a Package condition raised while branches run is untouched", {
  error <- expect_error(summarize_with_margins(
    coercion_frame(),
    region = sum(units),
    .grouping = rollup(region)
  ))

  expect_s3_class(error, "marginplyr_error")
  expect_identical(
    conditionMessage(error),
    paste0(
      "Summary results cannot overwrite grouping column:\n",
      "i `region`."
    )
  )
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "summarize_with_margins"
  )
})

# The non-goal, pinned so that a later reader does not read it as this bug
# unfixed. A lazy input's branch `summarize()` builds a query without
# evaluating the caller's expression, so the warning is raised inside the
# caller's own `collect()` with no marginplyr frame on the stack to intercept
# it -- and CONTEXT.md's *Repeated condition* says so: a verb answers only for
# the occurrences raised while it runs.
test_that("a lazy input leaves its execution warnings to the caller", {
  skip_if_suggest_absent("dtplyr")

  query <- summarize_with_margins(
    dtplyr::lazy_dt(coercion_frame()),
    total = sum(as.numeric(grade)),
    .grouping = cube(region, grade)
  )

  collected <- collect_warnings(dplyr::collect(query))
  expect_gt(length(collected), 1L)
  expect_match(
    vapply(collected, conditionMessage, character(1)),
    "NAs introduced by coercion",
    fixed = TRUE
  )
})

test_that("the reported conditions read as they are written", {
  warnings <- collect_warnings(summarize_coercion_cube())
  error <- expect_error(summarize_failing_rollup())

  # Targeted snapshots: a structural assertion would not catch a dplyr
  # formatting change, which is the whole reason the deduplication key is
  # computed from rendered text. Regenerated whenever the wording is
  # deliberately improved.
  expect_snapshot(cat(conditionMessage(warnings[[1L]])))
  expect_snapshot(cat(conditionMessage(error)))
})

# Asserted on the seam rather than through a verb, because what these stand in
# for is a dplyr release: a bullet whose wording this cannot read leaves the
# condition as it arrived. The matching case is asserted beside them, since a
# test of non-matches alone would pass just as well if nothing ever matched.
test_that("a bullet this cannot read leaves the condition alone", {
  arguments <- c(`dplyr::all_of("x")` = "c(x)")
  # A rendered warning always introduces the caller's own diagnostic with a
  # `Caused by` line, so the fixtures carry one: it is what separates dplyr's
  # region of the message from the caller's, and a warning without one is a
  # case of its own below.
  cause <- "\nCaused by warning:\n! boom"
  restated <- function(text) {
    conditionMessage(restate_condition_arguments(
      rlang::warning_cnd("test_warning", message = text),
      arguments
    ))
  }

  expect_identical(
    restated(paste0("i In argument: `dplyr::all_of(\"x\")`.", cause)),
    paste0("i In argument: `c(x)`.", cause)
  )
  # dbplyr's bullet carries no trailing period, and one is neither required to
  # read the span nor added when the bullet is rebuilt.
  expect_identical(
    restated(paste0("i In argument: `dplyr::all_of(\"x\")`", cause)),
    paste0("i In argument: `c(x)`", cause)
  )
  # A wording dplyr could move to.
  expect_identical(
    restated(paste0("i In summary: `dplyr::all_of(\"x\")`.", cause)),
    paste0("i In summary: `dplyr::all_of(\"x\")`.", cause)
  )
  # A span that is no label this branch handed dplyr.
  expect_identical(
    restated(paste0("i In argument: `dplyr::all_of(\"y\")`.", cause)),
    paste0("i In argument: `dplyr::all_of(\"y\")`.", cause)
  )
  # A bullet cli wrapped is read as the line it was written as, so one that
  # rejoins to a label is restated -- as one line, which is what it was written
  # as. cli breaks at a space, so this is the ordinary case at a narrow width.
  wrapped <- conditionMessage(restate_condition_arguments(
    rlang::warning_cnd(
      "test_warning",
      message = paste0(
        "i In argument: `dplyr::all_of(c(\"x\",\n  \"y\"))`.",
        cause
      )
    ),
    c(`dplyr::all_of(c("x", "y"))` = "c(x, y)")
  ))
  expect_identical(wrapped, paste0("i In argument: `c(x, y)`.", cause))
  # A wrap that does not rejoin to a label is left alone, which is what happens
  # wherever cli had to break inside a token rather than at a space.
  expect_identical(
    restated(paste0("i In argument: `dplyr::all_of(\n  \"x\")`.", cause)),
    paste0("i In argument: `dplyr::all_of(\n  \"x\")`.", cause)
  )
  # The structured shape, where the marker is the name of the message vector
  # rather than part of its text, and has to survive the restatement. An
  # error's `$message` is dplyr's bullet alone -- the caller's diagnostic
  # lives in `$parent` -- so no `Caused by` line bounds it.
  restated_error <- restate_condition_arguments(
    rlang::error_cnd("test_error", message = c(i = "In argument: `x`.")),
    c(x = "c(x)")
  )
  expect_identical(restated_error$message, c(i = "In argument: `c(x)`."))
})

# The two halves of the degradation the issue's constraint rests on: what this
# cannot attribute to dplyr's own region of the message it may not touch at
# all, byte for byte. A caller's diagnostic can spell anything -- including a
# line reading exactly like dplyr's bullet over a label a branch really handed
# dplyr -- and rewriting one is replacing an External condition's diagnostic,
# which ADR 0015 rules out.
test_that("a caller's own text is never restated", {
  arguments <- c(`dplyr::all_of("x")` = "c(x)")
  cause <- "\nCaused by warning:\n! boom"
  restated <- function(text) {
    conditionMessage(restate_condition_arguments(
      rlang::warning_cnd("test_warning", message = text),
      arguments
    ))
  }

  # The caller's diagnostic sits after `Caused by`, and stays as written even
  # where it spells dplyr's bullet over a matching label; the bullet before it
  # is dplyr's and is restated.
  bulletlike <- paste0(
    "i In argument: `dplyr::all_of(\"x\")`.\n",
    "Caused by warning:\n",
    "! boom\n",
    "In argument: `dplyr::all_of(\"x\")`."
  )
  expect_identical(
    restated(bulletlike),
    paste0(
      "i In argument: `c(x)`.\n",
      "Caused by warning:\n",
      "! boom\n",
      "In argument: `dplyr::all_of(\"x\")`."
    )
  )
  # A warning without a `Caused by` line is not one dplyr aggregated, so all
  # of it is the caller's and none of it is restated.
  expect_identical(
    restated("i In argument: `dplyr::all_of(\"x\")`."),
    "i In argument: `dplyr::all_of(\"x\")`."
  )
  # A message this restates nothing in comes back byte-identical, trailing
  # newlines included: rebuilding it from its lines silently dropped one.
  expect_identical(restated("no match here\n"), "no match here\n")
  expect_identical(restated("a\n\nb\n\n"), "a\n\nb\n\n")
  # A message this does restate keeps its trailing newline for the same
  # reason, and needs saying separately: splitting drops it, so the restated
  # path puts it back rather than inheriting the one the unchanged path never
  # removed.
  expect_identical(
    restated(paste0("i In argument: `dplyr::all_of(\"x\")`.", cause, "\n")),
    paste0("i In argument: `c(x)`.", cause, "\n")
  )
  # An empty message is neither restated nor rebuilt.
  expect_identical(restated(""), "")
})

# Two dots can hand dplyr one expression, and the span then says which
# expression raised the condition but not which argument did. The substitution
# is made only where it does not depend on that answer.
test_that("an ambiguous label is restored only where it is unique", {
  dots <- list(
    rlang::quo(dplyr::all_of("x")),
    rlang::quo(dplyr::all_of("x"))
  )

  expect_identical(
    branch_argument_map(dots, c("c(x)", "c(x)")),
    c(`dplyr::all_of("x")` = "c(x)")
  )
  expect_length(branch_argument_map(dots, c("c(x)", "any_of(\"x\")")), 0L)
  # A dot no rewrite touched has nothing to restate, which is also what a
  # caller reaching an adapter directly hands over: `new_summary_arguments()`
  # defaults the labels to the dots' own, so absence needs no second value.
  expect_length(
    branch_argument_map(
      dots,
      c("dplyr::all_of(\"x\")", "dplyr::all_of(\"x\")")
    ),
    0L
  )
  expect_length(
    branch_argument_map(dots, new_summary_arguments(dots)$labels),
    0L
  )
})

# The shared reading of a condition another package raised, asserted here
# because it is this module's, and directly because neither consumer's
# diagnostic would report a traversal that changed shape: the share-selection
# reader answers the same empty vector for a chain read in the wrong order as
# for one naming nothing, and the grouping-specification predicate the same
# `FALSE`. Unlike the Condition context above, it is reached without a verb --
# a chain is what a caller's own selection failure arrives as, and building one
# is what states the shape the reader is written for.

test_that("the condition-chain reader answers a chain outermost first", {
  innermost <- rlang::error_cnd(message = "innermost")
  middle <- rlang::error_cnd(message = "middle", parent = innermost)
  outermost <- rlang::error_cnd(message = "outermost", parent = middle)

  chain <- condition_chain(outermost)

  expect_length(chain, 3L)
  # Each condition's own message, since `conditionMessage()` of a chained rlang
  # error already reports the parents underneath it and would pass whatever
  # order the chain arrived in.
  expect_identical(
    vapply(chain, function(condition) condition$message, character(1)),
    c("outermost", "middle", "innermost")
  )
  expect_length(condition_chain(innermost), 1L)
})

test_that("the condition-chain reader stops where the chain does", {
  # rlang writes `parent = NULL` for an unchained condition, and refuses
  # anything else in the field -- but the chains this walks are raised by other
  # packages, so the field is read for what it holds rather than trusted to be
  # a condition. Built with `structure()` here for that reason: `error_cnd()`
  # cannot express the case.
  foreign <- structure(
    list(message = "x", call = NULL, parent = "not a condition"),
    class = c("error", "condition")
  )

  expect_length(condition_chain(foreign), 1L)
  expect_identical(condition_chain(NULL), list())
  expect_identical(condition_chain("not a condition"), list())
})

test_that("a tidyselect selection failure chains as the reader walks it", {
  # The two shapes the consumers exist to handle, taken from tidyselect rather
  # than built here: a selection helper raises the refusal inside its own call,
  # so what a `tryCatch()` handler catches is a wrapper holding no `i`, while a
  # bare subscript raises it at the top. A reader of the caught condition alone
  # would report the first as a failure that named nothing.
  proxy <- list(sales = 1L)
  refused <- "profit"

  wrapped <- expect_error(
    tidyselect::eval_select(
      rlang::quo(tidyselect::all_of(refused)),
      data = proxy,
      strict = TRUE
    )
  )
  direct <- expect_error(
    tidyselect::eval_select(rlang::quo(profit), data = proxy, strict = TRUE)
  )

  wrapped_chain <- condition_chain(wrapped)
  direct_chain <- condition_chain(direct)

  # Deeper than one rather than tidyselect's current two, and the subscript
  # found anywhere below the top rather than at a fixed depth: how many layers
  # tidyselect wraps a helper's failure in is not promised, and a layer added
  # upstream is not a defect here. What the consumers need is that the refusal
  # is reachable from a condition that does not carry it, which is what these
  # say. The order the chain is read in is pinned above, where it is this
  # reader's own.
  expect_gt(length(wrapped_chain), 1L)
  expect_null(wrapped_chain[[1L]]$i)
  expect_true(any(vapply(
    wrapped_chain,
    function(condition) identical(condition$i, refused),
    logical(1)
  )))
  expect_identical(direct_chain[[1L]]$i, refused)
})
