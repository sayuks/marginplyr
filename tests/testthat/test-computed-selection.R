# A computed expression where `.by` or a constructor selects columns (#1703).
# tidyselect evaluates such a call without the columns in scope, so it failed
# with "object not found" for a column that exists, or -- for a column named
# like a function -- with the function's deparsed body as column names.

computed_selection_data <- function() {
  data.frame(
    region = c("a", "b"),
    day = as.Date(c("2025-01-01", "2026-02-01")),
    dt = 1:2,
    value = 1:2
  )
}

computed_selection_remedy <- paste0(
  "i Create the column with `dplyr::mutate()` first, then select it by name."
)

expect_computed_refusal <- function(call, main, computed) {
  error <- expect_error(call, class = "marginplyr_error")
  lines <- strsplit(conditionMessage(error), "\n", fixed = TRUE)[[1L]]
  expect_identical(lines, c(main, computed, computed_selection_remedy))
  expect_null(error$parent)
  invisible(error)
}

by_main <- "`.by` selects existing columns, not computed expressions."
grouping_main <- paste0(
  "Grouping specification arguments select existing columns, not computed ",
  "expressions."
)

test_that("a computed `.by` is sent to mutate() on every entry point", {
  data <- computed_selection_data()
  for (verb in names(forwarded_verbs)) {
    expect_computed_refusal(
      forwarded_verbs[[verb]](data, toupper(region), rollup(dt)),
      by_main,
      "i `toupper(region)` is computed from `region`."
    )
  }
})

test_that("a computed constructor argument is sent to mutate()", {
  data <- computed_selection_data()
  for (verb in names(forwarded_verbs)) {
    expect_computed_refusal(
      forwarded_verbs[[verb]](data, NULL, rollup(format(day, "%Y"))),
      grouping_main,
      "i `format(day, \"%Y\")` is computed from `day`."
    )
  }
  # Under a selection operator, and inside a nested constructor.
  expect_computed_refusal(
    inspect_grouping(data, .grouping = grouping_sets(c(region, toupper(dt)))),
    grouping_main,
    "i `toupper(dt)` is computed from `dt`."
  )
  expect_computed_refusal(
    inspect_grouping(
      data, .grouping = grouping_sets(rollup(region, format(day)))
    ),
    grouping_main,
    "i `format(day)` is computed from `day`."
  )
})

# `dt` is also `stats::dt()`, which tidyselect found and deparsed.
test_that("a column named like a function gets the same refusal", {
  data <- computed_selection_data()
  error <- expect_computed_refusal(
    inspect_grouping(data, .grouping = rollup(format(dt))),
    grouping_main,
    "i `format(dt)` is computed from `dt`."
  )
  expect_no_match(conditionMessage(error), "function", fixed = TRUE)
})

# `tbl_lazy()` has no connection, so a refusal that read data would fail
# differently.
test_that("a lazy input is refused from its metadata alone", {
  remote <- dbplyr::tbl_lazy(
    computed_selection_data(),
    con = dbplyr::simulate_postgres()
  )
  expect_computed_refusal(
    inspect_grouping(remote, .by = toupper(region), .grouping = rollup(dt)),
    by_main,
    "i `toupper(region)` is computed from `region`."
  )
  expect_computed_refusal(
    inspect_grouping(remote, .grouping = rollup(format(day, "%Y"))),
    grouping_main,
    "i `format(day, \"%Y\")` is computed from `day`."
  )
})

test_that("selection helpers and other failures are left as they were", {
  data <- computed_selection_data()
  expect_identical(
    inspect_grouping(data, .grouping = rollup(starts_with("reg")))$included,
    inspect_grouping(data, .grouping = rollup(region))$included
  )
  missing <- expect_error(
    inspect_grouping(data, .grouping = rollup(all_of("zzz")))
  )
  expect_false(inherits(missing, "marginplyr_error"))
  # A call naming no column of the input is not a computed column.
  unrelated <- expect_error(
    inspect_grouping(data, .grouping = rollup(toupper(nowhere)))
  )
  expect_false(inherits(unrelated, "marginplyr_error"))
})

# The refusal diagnoses a failed column lookup. A caller's own function that
# fails for its own reason has its condition reach the caller, as it does
# through `tidyselect::eval_select()` (#1802).
test_that("a caller's own function keeps its own condition", {
  data <- computed_selection_data()
  key_cols <- function(x) {
    rlang::abort("The key registry is offline.", class = "registry_offline")
  }
  caught <- function(call) {
    rlang::try_fetch(call, registry_offline = function(cnd) "caught")
  }
  for (verb in names(forwarded_verbs)) {
    expect_identical(
      caught(forwarded_verbs[[verb]](data, key_cols(region), rollup(dt))),
      "caught"
    )
    expect_identical(
      caught(forwarded_verbs[[verb]](data, NULL, rollup(key_cols(region)))),
      "caught"
    )
  }
  expect_identical(
    caught(inspect_grouping(
      data, .grouping = grouping_sets(c(dt, key_cols(region)))
    )),
    "caught"
  )
})

# The part a failure is decided for is the one whose column it names, or, for
# any other failure, a part the caller defined wherever it stands.
test_that("a later computed part is the one a failure is decided for", {
  data <- computed_selection_data()
  ok_cols <- function(x) "region"
  expect_computed_refusal(
    inspect_grouping(
      data, .by = c(ok_cols(dt), toupper(region)), .grouping = rollup(value)
    ),
    by_main,
    "i `toupper(region)` is computed from `region`."
  )
  expect_computed_refusal(
    inspect_grouping(data, .grouping = rollup(c(ok_cols(dt), toupper(region)))),
    grouping_main,
    "i `toupper(region)` is computed from `region`."
  )
  key_cols <- function(x) {
    rlang::abort("The key registry is offline.", class = "registry_offline")
  }
  caught <- function(call) {
    rlang::try_fetch(call, registry_offline = function(cnd) "caught")
  }
  expect_identical(caught(inspect_grouping(
    data, .by = c(rlang::as_name(quote(region)), key_cols(dt)),
    .grouping = rollup(value)
  )), "caught")
  expect_identical(caught(inspect_grouping(
    data, .grouping = rollup(c(rlang::as_name(quote(dt)), key_cols(region)))
  )), "caught")
})

test_that("a caller's function keeps a converted warning and a lookup", {
  data <- computed_selection_data()
  slow_registry <- function(x) {
    warning("The key registry is slow.")
    "region"
  }
  absent_registry <- function(x) registry_table
  converted <- local({
    withr::local_options(warn = 2)
    expect_error(
      inspect_grouping(
        data, .by = slow_registry(region), .grouping = rollup(dt)
      )
    )
  })
  expect_false(inherits(converted, "marginplyr_error"))
  expect_match(conditionMessage(converted), "The key registry is slow.",
               fixed = TRUE)
  absent <- expect_error(
    inspect_grouping(data, .grouping = rollup(absent_registry(region)))
  )
  expect_false(inherits(absent, "marginplyr_error"))
  expect_match(conditionMessage(absent), "registry_table", fixed = TRUE)
})

test_that("a time limit inside a caller's function ends the call", {
  data <- computed_selection_data()
  slow_cols <- function(x) {
    expire_time_limit()
    "region"
  }
  chain_end <- function(cnd) {
    chain <- condition_chain(cnd)
    conditionMessage(chain[[length(chain)]])
  }
  native <- catch_time_limited(
    tidyselect::eval_select(rlang::quo(slow_cols(region)), data)
  )
  error <- catch_time_limited(
    inspect_grouping(data, .by = slow_cols(region), .grouping = rollup(dt))
  )
  expect_s3_class(error, "error")
  expect_false(inherits(error, "marginplyr_error"))
  expect_identical(chain_end(error), chain_end(native))
})

test_that("a caller's function that only transforms a column is refused", {
  data <- computed_selection_data()
  my_upper <- function(x) toupper(x)
  expect_computed_refusal(
    inspect_grouping(data, .by = my_upper(region), .grouping = rollup(dt)),
    by_main,
    "i `my_upper(region)` is computed from `region`."
  )
  expect_computed_refusal(
    inspect_grouping(data, .grouping = rollup(my_upper(region))),
    grouping_main,
    "i `my_upper(region)` is computed from `region`."
  )
})

test_that("deciding the refusal runs and forces nothing of the caller's", {
  data <- computed_selection_data()
  calls <- 0L
  counted_cols <- function(x) {
    calls <<- calls + 1L
    rlang::abort("The key registry is offline.", class = "registry_offline")
  }
  expect_error(
    inspect_grouping(data, .by = counted_cols(region), .grouping = rollup(dt)),
    class = "registry_offline"
  )
  expect_identical(calls, 1L)
  forced <- 0L
  delayedAssign("lazy_cols", {
    forced <- forced + 1L
    function(x) "region"
  })
  error <- expect_error(
    inspect_grouping(data, .grouping = rollup(c(zzz, lazy_cols(region))))
  )
  expect_false(inherits(error, "marginplyr_error"))
  expect_identical(forced, 0L)
})

# A failure is decided for the part tidyselect names as having raised it, not
# for another part whose spelling it happens to match.
test_that("a failure is decided for the part that raised it", {
  data <- computed_selection_data()
  ok_cols <- function(x) "region"
  expect_computed_refusal(
    inspect_grouping(
      data, .by = c(format(dt), ok_cols(region)), .grouping = rollup(value)
    ),
    by_main,
    "i `format(dt)` is computed from `dt`."
  )
  key_cols <- function(x) {
    rlang::abort("The key registry is offline.", class = "registry_offline")
  }
  expect_computed_refusal(
    inspect_grouping(
      data, .grouping = rollup(c(toupper(dt), key_cols(region)))
    ),
    grouping_main,
    "i `toupper(dt)` is computed from `dt`."
  )
  # The helper's own missing object is named like a column another part names.
  free_read <- function(x) value_table
  data$value_table <- data$value
  for (call in list(
    quote(inspect_grouping(
      data, .by = c(free_read(region), toupper(value_table)),
      .grouping = rollup(dt)
    )),
    quote(inspect_grouping(
      data, .grouping = rollup(c(free_read(region), toupper(value_table)))
    ))
  )) {
    error <- expect_error(eval(call))
    expect_false(inherits(error, "marginplyr_error"))
    expect_match(conditionMessage(error), "free_read(region)", fixed = TRUE)
    expect_match(conditionMessage(error), "value_table", fixed = TRUE)
  }
})

# A computed part tidyselect never reached is a bystander: the failure was
# raised by a part that computes nothing, and reaches the caller as it does
# through `tidyselect::eval_select()` (#1802).
test_that("a failure before a computed part is left to the caller", {
  data <- computed_selection_data()
  data$store <- c("x", "y")
  key_cols <- function(x) {
    rlang::abort("The key registry is offline.", class = "registry_offline")
  }
  caught <- function(call) {
    rlang::try_fetch(call, registry_offline = function(cnd) "caught")
  }
  for (verb in names(forwarded_verbs)) {
    expect_identical(caught(forwarded_verbs[[verb]](
      data, c(all_of(key_cols(region)), toupper(store)), rollup(dt)
    )), "caught")
    expect_identical(caught(forwarded_verbs[[verb]](
      data, NULL, rollup(c(all_of(key_cols(region)), toupper(store)))
    )), "caught")
  }
  for (call in list(
    quote(inspect_grouping(
      data, .by = c(all_of(nope), toupper(store)), .grouping = rollup(dt)
    )),
    quote(inspect_grouping(
      data, .grouping = rollup(c(all_of(nope), toupper(store)))
    ))
  )) {
    error <- expect_error(eval(call))
    expect_false(inherits(error, "marginplyr_error"))
    expect_match(conditionMessage(error), "all_of(nope)", fixed = TRUE)
  }
})

# A function a namespace part returns, its head bare or qualified, is applied
# by tidyselect as a predicate, outside its "In argument" wrapper, so its error
# names no part (#1802).
test_that("a namespace part returning a function keeps the refusal", {
  data <- computed_selection_data()
  for (call in list(
    quote(inspect_grouping(
      data, .by = identity(dt), .grouping = rollup(region)
    )),
    quote(inspect_grouping(data, .grouping = rollup(identity(dt)))),
    quote(inspect_grouping(data, .grouping = rollup(base::identity(dt)))),
    quote(inspect_grouping(data, .grouping = rollup(c(value, identity(dt))))),
    quote(inspect_grouping(
      data, .by = c(value, force(dt)), .grouping = rollup(region)
    ))
  )) {
    expect_error(
      eval(call), "is computed from `dt`", class = "marginplyr_error"
    )
  }
})

# A `where()` predicate's condition reaches the reader unwrapped, and one whose
# class computes its message in a `conditionMessage()` method holds no
# `message` field to compare. The reader passes over it to the caller's own
# function, whose failure stays the caller's (#1802).
test_that("a predicate's condition with no message field stays its own", {
  rlang::local_bindings(
    conditionMessage.registry_offline = function(c) {
      "The key registry is offline."
    },
    .env = asNamespace("base")$.__S3MethodsTable__.
  )
  offline <- function(x) {
    stop(structure(
      class = c("registry_offline", "error", "condition"), list(call = NULL)
    ))
  }
  data <- computed_selection_data()
  my_upper <- function(x) toupper(x)
  expect_error(
    inspect_grouping(
      data, .by = c(where(offline), my_upper(region)), .grouping = rollup(dt)
    ),
    "The key registry is offline.", fixed = TRUE, class = "registry_offline"
  )
  expect_error(
    inspect_grouping(
      data, .grouping = rollup(c(where(offline), my_upper(region)))
    ),
    "The key registry is offline.", fixed = TRUE, class = "registry_offline"
  )
})

# Without a conditionMessage() method the condition has no message at all, and
# it still reaches the caller as tidyselect raises it (#1802).
test_that("a predicate's condition with no message stays its own", {
  offline <- function(x) {
    stop(structure(
      class = c("registry_offline", "error", "condition"), list(call = NULL)
    ))
  }
  data <- computed_selection_data()
  my_upper <- function(x) toupper(x)
  expect_error(
    inspect_grouping(
      data, .by = c(where(offline), my_upper(region)), .grouping = rollup(dt)
    ),
    class = "registry_offline"
  )
})
