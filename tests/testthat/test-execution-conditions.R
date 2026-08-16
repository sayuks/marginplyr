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

# #108's reproduction: an error from the caller's own expression, which aborts
# the first branch that raises it.
summarize_failing_rollup <- function() {
  # `g` is a column of the frame above it; see the note in
  # `summarize_coercion_cube()`.
  # nolint start: object_usage_linter.
  summarize_with_margins(
    data.frame(g = c("a", "b"), v = c(1, 2)),
    x = stop("my error"),
    .grouping = rollup(g)
  )
  # nolint end
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

# `expr` is forced inside `collect_warnings()`, so it is rendered at the width
# set here.
collect_warnings_at_width <- function(width, expr) {
  original <- options(cli.width = width)
  on.exit(options(original), add = TRUE)
  collect_warnings(expr)
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

# cli wraps a bullet it cannot fit onto continuation lines, so how many lines a
# warning message holds is a function of the console width and of how long the
# grouping values are. Which grouping set raised a warning is no more part of
# its identity when the bullet naming it wrapped: at 60 columns this reproduced
# three reports of one condition, and at 40 it reproduced four.
test_that("a repeated warning is one report at any console width", {
  for (width in c(80L, 60L, 40L, 20L)) {
    warnings <- collect_warnings_at_width(width, summarize_coercion_cube())

    expect_length(warnings, 1L)
    expect_match(
      conditionMessage(warnings[[1L]]),
      "3 further grouping sets",
      fixed = TRUE,
      info = paste("width", width)
    )
  }
})

# A test that only asserted collapsing would pass if everything collapsed, so
# the plan whose branches raise genuinely different diagnostics is asserted
# beside it. `dplyr::n()` is 3 only in the grouping set that groups by nothing.
test_that("branches raising different diagnostics are reported separately", {
  warnings <- collect_warnings(summarize_with_margins(
    coercion_frame(),
    total = {
      if (dplyr::n() == 3L) warning("whole table") else warning("one region")
      0
    },
    .grouping = rollup(region)
  ))

  messages <- vapply(warnings, conditionMessage, character(1))
  expect_length(warnings, 2L)
  expect_true(any(grepl("whole table", messages, fixed = TRUE)))
  expect_true(any(grepl("one region", messages, fixed = TRUE)))
  expect_false(any(grepl("further grouping set", messages, fixed = TRUE)))
})

# Asserted on the seam rather than through a verb, because neither case can be
# produced through one: dplyr aggregates a branch's warnings into one condition
# of its own before signalling, so the class it carries is always
# `rlang_warning` and its message is never empty. The identity is stated over
# the class as well as the diagnostic (CONTEXT.md, *Repeated condition*), and a
# warning that reached this without dplyr's aggregation must get a cause rather
# than an error -- replacing an External condition with one of marginplyr's own
# is the one outcome the contract rules out.
test_that("a warning's cause covers its class and admits an empty message", {
  text <- "There was 1 warning in `dplyr::summarize()`.\n! NAs introduced"

  expect_false(identical(
    branch_warning_cause(rlang::warning_cnd("one_class", message = text)),
    branch_warning_cause(rlang::warning_cnd("other_class", message = text))
  ))
  expect_identical(
    branch_warning_cause(rlang::warning_cnd("one_class", message = text)),
    branch_warning_cause(rlang::warning_cnd("one_class", message = text))
  )
  expect_type(
    branch_warning_cause(rlang::warning_cnd("one_class", message = "")),
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
    "Summary results cannot overwrite grouping column `region`."
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
  skip_if_backend_absent("dtplyr")

  query <- summarize_with_margins(
    dtplyr::lazy_dt(coercion_frame()),
    total = sum(as.numeric(grade)),
    .grouping = cube(region, grade)
  )

  collected <- collect_warnings(dplyr::collect(query))
  expect_true(length(collected) > 1L)
  expect_true(all(vapply(
    collected,
    function(cnd) grepl("NAs introduced by coercion", conditionMessage(cnd)),
    logical(1)
  )))
})

test_that("the reported conditions read as they are written", {
  data <- data.frame(g = c("a", "b"), v = c(1, 2))
  warnings <- collect_warnings(summarize_with_margins(
    coercion_frame(),
    total = sum(as.numeric(grade)),
    .grouping = cube(region, grade)
  ))
  error <- expect_error(summarize_with_margins(
    data,
    x = stop("my error"),
    .grouping = rollup(g)
  ))

  # Targeted snapshots: a structural assertion would not catch a dplyr
  # formatting change, which is the whole reason the deduplication key is
  # computed from rendered text. Regenerated whenever the wording is
  # deliberately improved.
  expect_snapshot(cat(conditionMessage(warnings[[1L]])))
  expect_snapshot(cat(conditionMessage(error)))
})
