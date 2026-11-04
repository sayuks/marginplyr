# Mixed selections in one set and across the registry's nested family forms.
leading_error_specs <- function(first, later) {
  list(
    grouping_set(!!first, !!later),
    grouping_sets(grouping_set(!!first), grouping_set(!!later)),
    rollup(grouping_set(!!first), !!later),
    cube(grouping_set(!!first), !!later),
    grouping_spec(grouping_sets(grouping_set(!!first)), rollup(!!later))
  )
}

test_that("known fixed-key overlaps precede unsafe dtplyr metadata", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame("fixed  key" = 1L, value = 2L, check.names = FALSE)
  step <- dtplyr::lazy_dt(data) |> dplyr::mutate(value = as.character(value))
  predicates <- 0L
  numeric_column <- function(x) {
    predicates <<- predicates + 1L
    is.numeric(x)
  }
  first <- rlang::sym("fixed  key")
  for (spec in leading_error_specs(first, rlang::quo(where(numeric_column)))) {
    for (verb in forwarded_verbs) {
      original <- expect_error(verb(data, !!first, grouping_set(!!first)))
      expect_match(conditionMessage(original), "Columns cannot appear in both")
      expect_match(conditionMessage(original), "`fixed  key`", fixed = TRUE)
      for (input in list(data, step, dplyr::group_by(step, !!first))) {
        by <- if (length(dplyr::group_vars(input)) > 0L) {
          NULL
        } else {
          first
        }
        mixed <- expect_error(
          verb(input, !!by, spec), class = "marginplyr_error"
        )
        expect_identical(conditionMessage(mixed), conditionMessage(original))
      }
    }
  }
  expect_identical(predicates, 0L)
})

test_that("leading selection errors precede unsafe dtplyr metadata", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(x = 1L, value = 2L)
  step <- dtplyr::lazy_dt(data) |> dplyr::mutate(x = as.character(x))
  predicates <- 0L
  numeric_column <- function(x) {
    predicates <<- predicates + 1L
    is.numeric(x)
  }

  for (first in list(quote(absent), quote(all_of("absent")), quote(x * x))) {
    for (spec in leading_error_specs(first, quote(where(numeric_column)))) {
      for (verb in forwarded_verbs) {
        original <- expect_error(verb(data, NULL, grouping_set(!!first, x)))
        mixed <- expect_error(verb(step, NULL, spec))
        expect_identical(class(mixed), class(original))
        expect_identical(conditionMessage(mixed), conditionMessage(original))
        expect_identical(conditionCall(mixed), conditionCall(original))
        expect_identical(class(mixed$parent), class(original$parent))
        if (!is.null(original$parent)) {
          expect_identical(
            conditionMessage(mixed$parent), conditionMessage(original$parent)
          )
        }
      }
    }
  }
  expect_identical(predicates, 0L)
  expect_error(
    inspect_grouping(
      step, .grouping = grouping_set(where(numeric_column), absent)
    ),
    "Can't safely determine column metadata", class = "marginplyr_error"
  )
  expect_identical(predicates, 0L)
})

test_that("leading overlaps retain fixed-key order and selection precedence", {
  data <- data.frame(x = 1L, value = 2L)
  predicate <- function(x) stop("typed selection evaluated")
  for (verb in forwarded_verbs) {
    original <- expect_error(verb(data, c(value, x), grouping_set(x, value)))
    mixed <- expect_error(verb(data, c(value, x), grouping_set(
      x, value, where(predicate)
    )), class = "marginplyr_error")
    expect_identical(conditionMessage(mixed), conditionMessage(original))
    expect_error(
      verb(data, x, grouping_set(x, absent, where(predicate))),
      "absent", class = "vctrs_error_subscript_oob"
    )
    expect_error(
      verb(data, x, grouping_set(where(predicate), x)),
      "typed selection evaluated"
    )
  }
})

test_that("mixed selections preserve evaluation order and counts", {
  data <- data.frame(x = 1L, value = 2L)
  for (verb in forwarded_verbs) {
    events <- character()
    choose_names <- function() {
      events <<- c(events, "names")
      "x"
    }
    numeric_column <- function(x) {
      events <<- c(events, "typed")
      is.numeric(x)
    }
    last_selection <- function() {
      events <<- c(events, "last")
      "value"
    }
    verb(data, NULL, grouping_set(
      all_of(choose_names()), where(numeric_column), last_selection()
    ))
    expect_identical(events, c("names", "typed", "typed", "last"))

    events <- character()
    fixed <- function(x) {
      events <<- c(events, "fixed")
      FALSE
    }
    verb(data, where(fixed), grouping_set(
      all_of(choose_names()), where(numeric_column), last_selection()
    ))
    expect_identical(
      events, c("fixed", "fixed", "names", "typed", "typed", "last")
    )

    events <- character()
    verb(data, where(fixed), grouping_set(
      all_of({
        events <<- c(events, "assignment")
        "x"
      }),
      where(numeric_column)
    ))
    expect_identical(
      events, c("fixed", "fixed", "assignment", "typed", "typed")
    )
  }
})

test_that("a discarded leading pass does not duplicate selection warnings", {
  data <- data.frame(x = 1L, value = 2L)
  for (verb in forwarded_verbs) {
    warnings <- character()
    withCallingHandlers(
      verb(data, NULL, grouping_set(one_of("absent"), where(is.numeric))),
      warning = function(cnd) {
        warnings <<- c(warnings, conditionMessage(cnd))
        invokeRestart("muffleWarning")
      }
    )
    expect_length(warnings, 1L)
    expect_match(warnings, "absent")
  }
})

test_that("warning-promoted selections precede unsafe dtplyr metadata", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(x = 1L, value = 2L)
  step <- dtplyr::lazy_dt(data) |> dplyr::mutate(x = as.character(x))
  rlang::local_options(warn = 2L, lifecycle_verbosity = "warning")
  predicates <- 0L
  numeric_column <- function(x) {
    predicates <<- predicates + 1L
    is.numeric(x)
  }
  for (first in list(quote(one_of("absent")), quote(.data$x))) {
    specs <- c(
      list(grouping_set(!!first)),
      leading_error_specs(first, quote(where(numeric_column)))
    )
    for (spec in specs) {
      for (verb in forwarded_verbs) {
        original <- expect_error(
          verb(data, NULL, grouping_set(!!first)), "converted from warning"
        )
        actual <- expect_error(verb(step, NULL, spec))
        expect_identical(class(actual), class(original))
        expect_identical(conditionMessage(actual), conditionMessage(original))
        expect_identical(conditionCall(actual), conditionCall(original))
        expect_identical(class(actual$parent), class(original$parent))
        if (!is.null(original$parent)) {
          expect_identical(
            conditionMessage(actual$parent), conditionMessage(original$parent)
          )
        }
      }
    }
  }
  expect_identical(predicates, 0L)
})

test_that("caller-muffled preflight warnings are reported once", {
  data <- data.frame(x = 1L, value = 2L)
  for (warn in c(-1L, 0L, 1L, 2L)) {
    rlang::local_options(warn = warn, lifecycle_verbosity = "warning")
    for (first in list(quote(one_of(c("x", "absent"))), quote(.data$x))) {
      for (spec in list(
        grouping_set(!!first), grouping_set(!!first, where(is.numeric))
      )) {
        for (verb in forwarded_verbs) {
          warnings <- list()
          withCallingHandlers(
            verb(data, NULL, spec),
            warning = function(cnd) {
              warnings[[length(warnings) + 1L]] <<- cnd
              invokeRestart("muffleWarning")
            }
          )
          expect_length(warnings, 1L)
        }
      }
    }
  }
})

test_that("caller-promoted selection warnings precede dtplyr metadata", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(x = 1L, value = 2L)
  step <- dtplyr::lazy_dt(data) |> dplyr::mutate(x = as.character(x))
  promoted <- structure(
    list(message = "caller rejected selection warning", call = quote(select()),
         token = new.env(parent = emptyenv()), payload = list(value = 42L)),
    class = c("caller_selection_error", "error", "condition")
  )
  capture <- function(expr) {
    tryCatch(withCallingHandlers(expr, warning = function(cnd) stop(promoted)),
             error = identity)
  }
  predicates <- 0L
  numeric_column <- function(x) {
    predicates <<- predicates + 1L
    is.numeric(x)
  }
  for (warn in c(0L, 1L, 2L)) {
    rlang::local_options(warn = warn, lifecycle_verbosity = "warning")
    for (first in list(quote(one_of("absent")), quote(.data$x))) {
      selection <- rlang::new_quosure(first)
      expect_identical(capture(tidyselect::eval_select(selection, data)),
                       promoted)
      specs <- c(list(grouping_set(!!first)), leading_error_specs(
        first, quote(where(numeric_column))
      ))
      for (verb in forwarded_verbs) {
        for (input in list(data, step)) {
          expect_identical(capture(verb(input, !!first, grouping_set(value))),
                           promoted)
          for (spec in specs) {
            expect_identical(capture(verb(input, NULL, spec)), promoted)
          }
        }
      }
    }
  }
  expect_identical(predicates, 0L)
})

test_that("preflight warning filtering retains later typed warnings", {
  data <- data.frame(x = 1L, value = 2L)
  for (warn in c(-1L, 0L, 1L, 2L)) {
    rlang::local_options(warn = warn, lifecycle_verbosity = "warning")
    for (muffle in c(FALSE, TRUE)) {
      for (verb in forwarded_verbs) {
        predicates <- 0L
        numeric_column <- function(x) {
          predicates <<- predicates + 1L
          warning("typed selection warning", call. = FALSE)
          is.numeric(x)
        }
        warnings <- character()
        withCallingHandlers(withCallingHandlers(
          verb(data, NULL, grouping_set(
            .data$x,
            one_of(c("x", "absent")), one_of(c("x", "absent")),
            where(numeric_column)
          )),
          warning = function(cnd) {
            warnings <<- c(warnings, conditionMessage(cnd))
            if (muffle) invokeRestart("muffleWarning")
          }
        ), warning = function(cnd) invokeRestart("muffleWarning"))
        expect_length(warnings, 5L)
        expect_match(warnings[[1L]], ".data")
        expect_match(warnings[[2L]], "absent")
        expect_identical(warnings[[3L]], warnings[[2L]])
        expect_identical(warnings[4:5], rep("typed selection warning", 2L))
        expect_identical(predicates, 2L)
      }
    }
  }
})

test_that("continuing handlers retain later selection warnings", {
  data <- data.frame(x = 1L, value = 2L)
  rlang::local_options(warn = -1L, lifecycle_verbosity = "warning")
  observe <- function(expr) {
    warnings <- list()
    withCallingHandlers(expr, warning = function(cnd) {
      warnings[[length(warnings) + 1L]] <<- list(
        class(cnd), conditionMessage(cnd), conditionCall(cnd)
      )
    })
    warnings
  }
  for (first in list(quote(one_of(c("x", "absent"))), quote(.data$x))) {
    selection <- rlang::quo(c(!!first, where(is.numeric), !!first))
    expected <- observe(tidyselect::eval_select(selection, data))
    for (verb in forwarded_verbs) {
      actual <- observe(verb(data, NULL, grouping_set(
        !!first, where(is.numeric), !!first
      )))
      expect_identical(actual, expected)
    }
  }
})

test_that("a leading failure does not force later helper bindings", {
  data <- data.frame(x = 1L, value = 2L)
  for (active in c(FALSE, TRUE)) {
    reads <- 0L
    env <- rlang::env()
    read <- function() {
      reads <<- reads + 1L
      stop("later binding forced")
    }
    if (active) {
      makeActiveBinding("selected", read, env)
    } else {
      delayedAssign("selected", read(), assign.env = env)
    }
    later <- rlang::new_quosure(quote(all_of(selected)), env)
    for (verb in forwarded_verbs) {
      expect_error(
        verb(data, NULL, grouping_set(absent, !!later, where(is.numeric))),
        "absent", class = "vctrs_error_subscript_oob"
      )
    }
    expect_identical(reads, 0L)
  }
})

test_that("preflight stops at the first selection requiring types", {
  data <- data.frame(x = 1L, value = 2L)
  first_error <- rlang::error_cnd("leading_predicate_error", message = "first")
  predicate <- function(x) stop(first_error)
  for (verb in forwarded_verbs) {
    cnd <- expect_error(
      verb(data, NULL, grouping_set(where(predicate), absent))
    )
    expect_identical(class(cnd), class(first_error))
    expect_identical(conditionMessage(cnd), "first")
    expect_error(
      verb(data, where(predicate), grouping_set(x, where(is.numeric))),
      "first"
    )
    expect_error(
      verb(data, where(predicate), grouping_set(absent, where(is.numeric))),
      "absent", class = "vctrs_error_subscript_oob"
    )
  }
})

test_that("completed nested family errors keep selection precedence", {
  data <- data.frame(x = 1L, value = 2L)
  for (verb in forwarded_verbs) {
    original <- expect_error(verb(data, NULL, grouping_spec(
      rollup(grouping_set(any_of("absent"))), absent
    )))
    mixed <- expect_error(verb(data, NULL, grouping_spec(
      rollup(grouping_set(any_of("absent"))), absent, where(is.numeric)
    )))
    expect_identical(class(mixed), class(original))
    expect_identical(conditionMessage(mixed), conditionMessage(original))
    expect_match(conditionMessage(mixed), "cannot be a composite dimension")
  }
})

test_that("DuckDB leading selection failures invoke no metadata query", {
  skip_if_suggest_absent("duckdb", "DBI")
  data <- data.frame(x = 1L, value = 2L)
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  remote <- dplyr::copy_to(con, data, "leading_selection_errors")
  # Observe the actual DBI method, beyond construction-time Sent query records.
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE)
  predicates <- 0L
  numeric_column <- function(x) {
    predicates <<- predicates + 1L
    is.numeric(x)
  }
  # SQL nesting is refused at input admission, before Grouping compilation.
  verbs <- forwarded_verbs[c(
    "inspect_grouping", "summarize_with_margins", "expand_with_margins"
  )]
  for (first in list(quote(absent), quote(all_of("absent")), quote(x * x))) {
    for (spec in leading_error_specs(first, quote(where(numeric_column)))) {
      for (verb in verbs) {
        original <- expect_error(verb(data, NULL, grouping_set(!!first, x)))
        mixed <- expect_error(verb(remote, NULL, spec))
        expect_identical(class(mixed), class(original))
        expect_identical(conditionMessage(mixed), conditionMessage(original))
        expect_identical(counter$queries, 0L)
      }
    }
  }
  expect_identical(predicates, 0L)

  specs <- leading_error_specs(quote(x), rlang::quo(where(numeric_column)))
  for (spec in specs) {
    for (verb in verbs) {
      original <- expect_error(verb(data, x, grouping_set(x)))
      mixed <- expect_error(verb(remote, x, spec), class = "marginplyr_error")
      expect_identical(conditionMessage(mixed), conditionMessage(original))
      expect_identical(counter$queries, 0L)
    }
  }
  expect_identical(predicates, 0L)

  typed <- rlang::quo(where(numeric_column) & value)
  duplicate_specs <- list(
    grouping_sets(x, x, !!typed),
    grouping_sets(grouping_sets(x, x, !!typed), value),
    grouping_sets(x, grouping_sets(x, !!typed)),
    grouping_spec(grouping_set(), grouping_sets(x, x, !!typed)),
    grouping_spec(grouping_sets(x, x), !!typed),
    grouping_spec(
      grouping_sets(x, x),
      grouping_sets(grouping_set(!!typed)),
      grouping_sets(grouping_spec(value, grouping_set()))
    ),
    grouping_spec(grouping_spec(grouping_sets(x, x), !!typed), value)
  )
  for (verb in verbs) {
    original <- expect_error(verb(data, NULL, grouping_sets(x, x, value)),
                             class = "marginplyr_error")
    for (spec in duplicate_specs) {
      mixed <- expect_error(
        verb(remote, NULL, spec), class = "marginplyr_error"
      )
      expect_identical(conditionMessage(mixed), conditionMessage(original))
    }
  }
  expect_identical(counter$queries, 0L)
  expect_identical(predicates, 0L)

  expect_error(
    inspect_grouping(remote, .grouping = grouping_spec(
      grouping_sets(x, x), grouping_set(absent, !!typed)
    )),
    "absent", class = "vctrs_error_subscript_oob"
  )
  expect_error(
    inspect_grouping(remote, .by = x, .grouping = grouping_spec(
      grouping_sets(x, x), !!typed
    )),
    "Columns cannot appear in both", class = "marginplyr_error"
  )
  expect_identical(counter$queries, 0L)
  expect_identical(predicates, 0L)

  expect_error(
    inspect_grouping(remote, .grouping = grouping_spec(
      grouping_set(),
      grouping_sets(x, grouping_spec(grouping_sets(x, x), !!typed))
    )),
    "2, 3.", fixed = TRUE, class = "marginplyr_error"
  )
  expect_identical(counter$queries, 0L)
  expect_identical(predicates, 0L)

  expect_error(
    inspect_grouping(remote, .grouping = grouping_sets(!!typed, x, x)),
    "2, 3", class = "marginplyr_error"
  )
  expect_identical(counter$queries, 1L)
  expect_identical(predicates, 2L)
  expect_error(
    inspect_grouping(remote, .grouping = grouping_spec(
      grouping_sets(value, grouping_set()), !!typed
    )),
    "1, 2.", fixed = TRUE, class = "marginplyr_error"
  )
  expect_identical(counter$queries, 2L)
  expect_identical(predicates, 4L)
  for (policy in c("keep", "drop")) {
    before <- counter$queries
    before_predicates <- predicates
    plan <- inspect_grouping(remote, .grouping = grouping_spec(
      grouping_sets(x, value, x), !!typed
    ), .duplicates = policy, .format = "list")
    expected <- list(c("x", "value"), "value")
    if (identical(policy, "keep")) {
      expected <- c(expected, expected[1L])
    }
    expect_identical(plan$included, expected)
    expect_identical(plan$set_id, seq_along(expected))
    expect_identical(counter$queries - before, 1L)
    expect_identical(predicates - before_predicates, 2L)
  }
  counter$queries <- 0L
  predicates <- 0L

  for (warn in c(0L, 1L)) {
    rlang::local_options(warn = warn, lifecycle_verbosity = "warning")
    promoted <- structure(
      list(message = "caller rejected selection warning",
           call = quote(select()),
           token = new.env(parent = emptyenv()), payload = list(value = 42L)),
      class = c("caller_selection_error", "error", "condition")
    )
    capture <- function(expr) {
      tryCatch(
        withCallingHandlers(expr, warning = function(cnd) stop(promoted)),
        error = identity
      )
    }
    for (first in list(quote(one_of("absent")), quote(.data$x))) {
      specs <- c(list(grouping_set(!!first)), leading_error_specs(
        first, quote(where(numeric_column))
      ))
      for (verb in verbs) {
        for (spec in specs) {
          expect_identical(capture(verb(remote, NULL, spec)), promoted)
          expect_identical(counter$queries, 0L)
        }
        expect_identical(capture(verb(remote, !!first, grouping_set(value))),
                         promoted)
        expect_identical(counter$queries, 0L)
      }
    }
  }
  expect_identical(predicates, 0L)

  rlang::local_options(warn = 2L, lifecycle_verbosity = "warning")
  for (first in list(quote(one_of("absent")), quote(.data$x))) {
    specs <- c(
      list(grouping_set(!!first)),
      leading_error_specs(first, quote(where(numeric_column)))
    )
    for (spec in specs) {
      for (verb in verbs) {
        original <- expect_error(
          verb(data, NULL, grouping_set(!!first)), "converted from warning"
        )
        actual <- expect_error(verb(remote, NULL, spec))
        expect_identical(class(actual), class(original))
        expect_identical(conditionMessage(actual), conditionMessage(original))
        expect_identical(counter$queries, 0L)
      }
    }
    for (verb in verbs) {
      expect_error(verb(remote, !!first, grouping_set(x)),
                   "converted from warning")
      expect_identical(counter$queries, 0L)
    }
  }
  expect_identical(predicates, 0L)

  for (warn in c(0L, 2L)) {
    rlang::local_options(warn = warn, lifecycle_verbosity = "error")
    for (verb in verbs) {
      expect_error(
        verb(remote, NULL, grouping_set(.data$x, where(numeric_column))),
        class = "lifecycle_error_deprecated"
      )
      expect_identical(counter$queries, 0L)
    }
  }
  rlang::local_options(warn = 2L, lifecycle_verbosity = "quiet")

  plan <- inspect_grouping(
    remote, .grouping = grouping_set(where(numeric_column))
  )
  expect_identical(plan$included, "(x, value)")
  expect_identical(counter$queries, 1L)
  expect_identical(predicates, 2L)
  for (verb in verbs[c("summarize_with_margins", "expand_with_margins")]) {
    before <- counter$queries
    query <- verb(remote, NULL, grouping_set(x, where(is.numeric)))
    expect_s3_class(query, "tbl_lazy")
    expect_identical(counter$queries - before, 1L)
    expected <- verb(data, NULL, grouping_set(x, where(is.numeric)))
    expect_equal(dplyr::collect(query), tibble::as_tibble(expected))
  }

  before <- counter$queries
  plan <- inspect_grouping(remote, .grouping = grouping_set(.data$x))
  expect_identical(plan$included, "(x)")
  expect_identical(counter$queries - before, 1L)

  for (warn in c(0L, 1L)) {
    rlang::local_options(warn = warn, lifecycle_verbosity = "warning")
    for (first in list(quote(one_of(c("x", "absent"))), quote(.data$x))) {
      before <- counter$queries
      warnings <- character()
      plan <- withCallingHandlers(
        inspect_grouping(remote, .grouping = grouping_set(!!first)),
        warning = function(cnd) {
          warnings <<- c(warnings, conditionMessage(cnd))
          invokeRestart("muffleWarning")
        }
      )
      expect_identical(plan$included, "(x)")
      expect_length(warnings, 1L)
      expect_identical(counter$queries - before, 1L)
    }
  }
})

test_that("known duplicate occurrences precede unsafe dtplyr metadata", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(x = 1L, value = 2L)
  step <- dtplyr::lazy_dt(data) |> dplyr::mutate(x = as.character(x))
  predicates <- 0L
  numeric_column <- function(x) {
    predicates <<- predicates + 1L
    is.numeric(x)
  }
  specs <- list(
    grouping_sets(x, x, where(numeric_column)),
    grouping_sets(x, grouping_sets(x, where(numeric_column))),
    grouping_spec(
      grouping_set(value), grouping_sets(x, x, where(numeric_column))
    ),
    grouping_spec(grouping_sets(x, x), where(numeric_column))
  )
  for (verb in forwarded_verbs) {
    original <- expect_error(verb(data, NULL, grouping_sets(x, x, value)),
                             class = "marginplyr_error")
    for (spec in specs) {
      mixed <- expect_error(verb(step, NULL, spec), class = "marginplyr_error")
      expect_identical(conditionMessage(mixed), conditionMessage(original))
    }
  }
  expect_error(
    inspect_grouping(step, .grouping = grouping_spec(
      grouping_sets(x, x), grouping_sets(x, where(numeric_column))
    )),
    "Can.t safely determine column metadata", class = "marginplyr_error"
  )
  expect_identical(predicates, 0L)
})
