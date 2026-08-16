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

test_that("a warning repeated across grouping sets is reported once", {
  warnings <- collect_warnings(summarize_with_margins(
    coercion_frame(),
    total = sum(as.numeric(grade)),
    .grouping = cube(region, grade)
  ))

  expect_length(warnings, 1L)
  message <- conditionMessage(warnings[[1L]])
  expect_match(message, "NAs introduced by coercion", fixed = TRUE)
  # Four grouping sets raise it, so three further ones follow the one reported.
  expect_match(message, "3 further grouping sets", fixed = TRUE)
  # The condition itself is the caller's to receive unchanged.
  expect_s3_class(warnings[[1L]], "rlang_warning")
})

test_that("a reported warning names the caller's own grouping columns", {
  warnings <- collect_warnings(summarize_with_margins(
    coercion_frame(),
    total = sum(as.numeric(grade)),
    .grouping = cube(region, grade)
  ))

  message <- conditionMessage(warnings[[1L]])
  expect_match(message, "`region = \"East\"`", fixed = TRUE)
  expect_match(message, "`grade = \"a\"`", fixed = TRUE)
  expect_false(grepl("marginplyr_key", message, fixed = TRUE))
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

# #108's reproduction. An error aborts the first branch that raises it, so
# there is nothing to deduplicate; what it needs is the context.
test_that("a branch error reports the caller's column, group, and verb", {
  data <- data.frame(g = c("a", "b"), v = c(1, 2))

  error <- expect_error(summarize_with_margins(
    data,
    x = stop("my error"),
    .grouping = rollup(g)
  ))

  message <- conditionMessage(error)
  expect_match(message, "In group 1: `g = \"a\"`", fixed = TRUE)
  expect_false(grepl("marginplyr_key", message, fixed = TRUE))
  expect_identical(
    rlang::call_name(conditionCall(error)),
    "summarize_with_margins"
  )
})

test_that("a propagated error keeps its class, diagnostic, and cause", {
  data <- data.frame(g = c("a", "b"), v = c(1, 2))

  error <- expect_error(summarize_with_margins(
    data,
    x = stop("my error"),
    .grouping = rollup(g)
  ))

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

  message <- conditionMessage(error)
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
  skip_on_cran()

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
