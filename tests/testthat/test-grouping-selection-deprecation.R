# tidyselect deprecates an external vector written bare in a selection. Where
# the vector is a Grouping specification the advice is withheld (#1703), and
# deciding that reads no binding tidyselect resolves as a column without
# evaluating it: a wrapper's argument named like a column is evaluated as often
# as `dplyr::select()` evaluates it, which is never (#1803).

deprecation_data <- function() {
  data.frame(region = c("E", "E", "W"), store = c("A", "B", "C"), v = 1:3)
}

# The deprecation is signalled on every call, as testthat's default does, so
# that no test depends on whether lifecycle has shown it this session.
quietly_deprecated <- function(expr) {
  rlang::local_options(lifecycle_verbosity = "warning")
  suppressWarnings(expr, classes = "lifecycle_warning_deprecated")
}

# `sp` is an external character vector; `store` is both a column and the
# wrapper's own argument, never evaluated by a native selection.
external_vector_wrappers <- function() {
  sp <- "region"
  list(
    native = function(data, store) {
      names(dplyr::collect(dplyr::select(data, c(sp, store))))
    },
    grouping_set = function(data, store) {
      inspect_grouping(data, .grouping = grouping_set(c(sp, store)))$included
    },
    grouping_sets = function(data, store) {
      inspect_grouping(data, .grouping = grouping_sets(c(sp, store)))$included
    },
    rollup = function(data, store) {
      inspect_grouping(data, .grouping = rollup(c(sp, store)))$included
    },
    cube = function(data, store) {
      inspect_grouping(data, .grouping = cube(c(sp, store)))$included
    },
    grouping_spec = function(data, store) {
      inspect_grouping(data, .grouping = grouping_spec(c(sp, store)))$included
    }
  )
}

# Each constructor's control is the same selection written with the column
# names instead of the vector.
expect_argument_unevaluated <- function(data, wrappers) {
  for (name in names(wrappers)) {
    forced <- 0L
    result <- quietly_deprecated(wrappers[[name]](data, {
      forced <- forced + 1L
      "never needed"
    }))
    expect_identical(forced, 0L, info = name)
    failing <- quietly_deprecated(
      wrappers[[name]](data, stop("never needed"))
    )
    if (name == "native") {
      expect_identical(result, c("region", "store"))
    } else {
      spec <- eval(
        rlang::call2(name, quote(c(region, store)), .ns = "marginplyr")
      )
      expected <- inspect_grouping(data, .grouping = spec)$included
      expect_identical(result, expected, info = name)
      expect_identical(failing, expected, info = name)
    }
  }
}

test_that("a deprecated selection leaves a wrapper's argument unevaluated", {
  expect_argument_unevaluated(deprecation_data(), external_vector_wrappers())
})

test_that("a deprecated selection on dtplyr leaves the argument unevaluated", {
  skip_if_suggest_absent("dtplyr")
  expect_argument_unevaluated(
    dtplyr::lazy_dt(deprecation_data()),
    external_vector_wrappers()[c("native", "rollup")]
  )
})

test_that("a deprecated selection on Arrow leaves the argument unevaluated", {
  skip_if_suggest_absent("arrow")
  expect_argument_unevaluated(
    arrow::arrow_table(deprecation_data()),
    external_vector_wrappers()[c("native", "rollup")]
  )
})

test_that("a deprecated selection on SQLite leaves the argument unevaluated", {
  skip_if_suggest_absent("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  expect_argument_unevaluated(
    dplyr::copy_to(con, deprecation_data(), "deprecation_data"),
    external_vector_wrappers()[c("native", "rollup")]
  )
})

test_that("a deprecated selection on DuckDB leaves the argument unevaluated", {
  skip_if_suggest_absent("duckdb")
  con <- duckdb_test_connection()
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_argument_unevaluated(
    dplyr::copy_to(con, deprecation_data(), "deprecation_data"),
    external_vector_wrappers()[c("native", "rollup")]
  )
})

test_that("a deprecated selection naming no column keeps tidyselect's error", {
  sp <- "region"
  wrapper <- function(data, store) {
    inspect_grouping(data, .grouping = rollup(c(sp, bad, store)))
  }
  error <- expect_error(
    quietly_deprecated(wrapper(deprecation_data(), stop("never needed"))),
    class = "vctrs_error_subscript_oob"
  )
  expect_no_match(conditionMessage(error), "never needed", fixed = TRUE)
})

test_that("a bare specification in a selection is refused without advice", {
  data <- deprecation_data()
  warned_while <- function(expr) {
    warned <- character()
    error <- withCallingHandlers(
      expect_error(expr, class = "vctrs_error_subscript_type"),
      warning = function(cnd) {
        warned <<- c(warned, conditionMessage(cnd))
        invokeRestart("muffleWarning")
      }
    )
    expect_identical(warned, character())
    invisible(error)
  }
  rlang::local_options(lifecycle_verbosity = "warning")
  from_argument <- function(data, s) {
    inspect_grouping(data, .grouping = grouping_sets(c(s, region)))
  }
  warned_while(from_argument(data, grouping_set(store)))
  delayedAssign("s", grouping_set(store))
  warned_while(
    inspect_grouping(data, .grouping = grouping_sets(c(s, region)))
  )
})

test_that("a deprecated selection calls no active binding", {
  calls <- 0L
  env <- rlang::env(rlang::current_env(), sp = "region")
  makeActiveBinding("store", function() {
    calls <<- calls + 1L
    "store"
  }, env)
  result <- quietly_deprecated(eval(
    quote(inspect_grouping(
      deprecation_data(),
      .grouping = rollup(c(sp, store))
    )$included),
    env
  ))
  expect_identical(calls, 0L)
  expect_identical(length(result), 3L)
})
