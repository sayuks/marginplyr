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

# A pass stopped at a typed selection names one proved group as an example
# (#1616). `positions` is that whole group, as the message spells it.
duplicate_example_lines <- function(positions) {
  c(paste0("Duplicate grouping sets were produced. For example, ",
           "these positions in the grouping plan have the same set:"),
    paste0("i ", positions, "."))
}

expect_duplicate_example <- function(cnd, positions, original = NULL) {
  lines <- function(cnd) {
    strsplit(conditionMessage(cnd), "\n", fixed = TRUE)[[1L]]
  }
  expect_identical(lines(cnd)[1:2], duplicate_example_lines(positions))
  if (!is.null(original)) {
    expect_identical(lines(cnd)[-(1:2)], lines(original)[-(1:2)])
  }
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
    grouping_spec(grouping_sets(x, x, !!typed), value),
    grouping_spec(
      grouping_sets(x, x),
      grouping_sets(grouping_set(!!typed)),
      grouping_sets(grouping_spec(value, grouping_set()))
    ),
    grouping_spec(grouping_spec(grouping_sets(x, x), !!typed), value),
    grouping_spec(
      grouping_sets(grouping_spec(grouping_sets(x, x), !!typed)), value
    ),
    grouping_spec(
      grouping_sets(grouping_spec(grouping_sets(x, x, !!typed), value)), value
    ),
    grouping_spec(
      grouping_sets(x, grouping_set()), grouping_set(x, !!typed)
    )
  )
  for (verb in verbs) {
    original <- expect_error(verb(data, NULL, grouping_sets(x, x, value)),
                             class = "marginplyr_error")
    for (spec in duplicate_specs) {
      mixed <- expect_error(
        verb(remote, NULL, spec), class = "marginplyr_error"
      )
      expect_duplicate_example(mixed, "1, 2", original)
    }
  }
  expect_identical(counter$queries, 0L)
  expect_identical(predicates, 0L)

  for (verb in verbs) {
    expect_error(verb(remote, NULL, grouping_spec(
      grouping_sets(value, grouping_spec(grouping_sets(x, x), !!typed)), value
    )), "2, 3.", fixed = TRUE, class = "marginplyr_error")
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
  expect_identical(counter$queries, 2L)
  expect_identical(predicates, 2L)
  expect_error(
    inspect_grouping(remote, .grouping = grouping_spec(
      grouping_sets(value, grouping_set()), !!typed
    )),
    "1, 2.", fixed = TRUE, class = "marginplyr_error"
  )
  expect_identical(counter$queries, 4L)
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
    expect_identical(counter$queries - before, 2L)
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
  expect_identical(counter$queries, 2L)
  expect_identical(predicates, 2L)
  for (verb in verbs[c("summarize_with_margins", "expand_with_margins")]) {
    before <- counter$queries
    query <- verb(remote, NULL, grouping_set(x, where(is.numeric)))
    expect_s3_class(query, "tbl_lazy")
    expect_identical(counter$queries - before, 2L)
    expected <- verb(data, NULL, grouping_set(x, where(is.numeric)))
    expect_equal(dplyr::collect(query), tibble::as_tibble(expected))
  }

  before <- counter$queries
  plan <- inspect_grouping(remote, .grouping = grouping_set(.data$x))
  expect_identical(plan$included, "(x)")
  expect_identical(counter$queries - before, 0L)

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
      expect_identical(counter$queries - before, 0L)
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
    grouping_spec(grouping_sets(x, x), where(numeric_column)),
    grouping_spec(grouping_sets(x, x, where(numeric_column)), value)
  )
  for (verb in forwarded_verbs) {
    original <- expect_error(verb(data, NULL, grouping_sets(x, x, value)),
                             class = "marginplyr_error")
    for (spec in specs) {
      mixed <- expect_error(verb(step, NULL, spec), class = "marginplyr_error")
      expect_duplicate_example(mixed, "1, 2", original)
    }
  }
  expect_duplicate_example(expect_error(
    inspect_grouping(step, .grouping = grouping_spec(
      grouping_sets(x, x), grouping_sets(x, where(numeric_column))
    )),
    class = "marginplyr_error"
  ), "1, 3")
  expect_identical(predicates, 0L)
})

test_that("known columns in a pending singleton settle product duplicates", {
  data <- data.frame(x = 1L, value = 2L)
  predicate <- function(x) stop("typed selection evaluated")
  spec <- grouping_spec(
    grouping_sets(x, grouping_set()), grouping_set(x, where(predicate))
  )

  for (verb in forwarded_verbs) {
    expect_error(
      verb(data, NULL, spec),
      "1, 2.", fixed = TRUE, class = "marginplyr_error"
    )
  }
})

test_that("duplicate product prefixes precede multi-set typed suffixes", {
  data <- data.frame(a = "x", b = 1L)
  predicates <- 0L
  predicate <- function(x) {
    predicates <<- predicates + 1L
    is.integer(x)
  }
  spec <- grouping_spec(
    grouping_sets(a, a),
    grouping_sets(grouping_set(where(predicate)), grouping_set())
  )
  for (verb in forwarded_verbs[c(
    "inspect_grouping", "nest_with_margins", "nest_by_with_margins"
  )]) {
    cnd <- expect_error(verb(data, NULL, spec), "Duplicate grouping sets",
                        class = "marginplyr_error")
    expect_duplicate_example(cnd, "1, 3")
  }
  expect_identical(predicates, 0L)
})

test_that("multi-set product duplicate positions survive outer families", {
  data <- data.frame(a = "x", b = 1L, c = "y", d = "z")
  predicate <- function(x) stop("typed selection evaluated")
  partial <- grouping_spec(
    grouping_sets(a, a), grouping_sets(b, where(predicate))
  )
  spec <- grouping_spec(!!partial, grouping_sets(c, grouping_set()))
  cnd <- expect_error(inspect_grouping(data, .grouping = spec),
                      "Duplicate grouping sets", class = "marginplyr_error")
  expect_duplicate_example(cnd, "1, 5")
  cnd <- expect_error(inspect_grouping(
    data, .grouping = grouping_sets(c, !!spec)
  ), "Duplicate grouping sets", class = "marginplyr_error")
  expect_duplicate_example(cnd, "2, 6")
  cnd <- expect_error(inspect_grouping(
    data, .grouping = grouping_spec(
      grouping_sets(c, !!partial), grouping_sets(d, grouping_set())
    )
  ), "Duplicate grouping sets", class = "marginplyr_error")
  expect_duplicate_example(cnd, "3, 7")
})

test_that("duplicate product prefixes avoid unsafe dtplyr suffix metadata", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = "x", b = 1L)
  callbacks <- 0L
  derive <- function(x) {
    callbacks <<- callbacks + 1L
    paste0(x, "!")
  }
  step <- dtplyr::lazy_dt(data) |> dplyr::mutate(added = derive(a))
  counter <- local_dtplyr_proxy_counter()
  predicate <- function(x) stop("typed selection evaluated")
  spec <- grouping_spec(
    grouping_sets(a, a),
    grouping_sets(grouping_set(where(predicate)), grouping_set())
  )
  for (verb in forwarded_verbs[c(
    "inspect_grouping", "nest_with_margins", "nest_by_with_margins"
  )]) {
    cnd <- expect_error(verb(step, NULL, spec), "Duplicate grouping sets",
                        class = "marginplyr_error")
    expect_duplicate_example(cnd, "1, 3")
  }
  expect_identical(callbacks, 0L)
  expect_identical(counter$n, 0L)
  expect_identical(counter$source_evaluations, 0L)
})

test_that("duplicate product prefixes avoid DuckDB multi-set suffix queries", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- data.frame(a = "x", b = 1L)
  remote <- dplyr::copy_to(con, data, "duplicate_product_suffix")
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE, after = FALSE)
  predicates <- 0L
  predicate <- function(x) {
    predicates <<- predicates + 1L
    is.integer(x)
  }
  typed <- grouping_set(where(predicate))
  specs <- list(
    grouping_spec(grouping_sets(a, a), !!typed),
    grouping_spec(grouping_sets(a, a), grouping_sets(b, grouping_set())),
    grouping_spec(grouping_sets(a, a), grouping_sets(!!typed, grouping_set()))
  )
  for (spec in specs) {
    expect_error(inspect_grouping(remote, .grouping = spec),
                 "Duplicate grouping sets", class = "marginplyr_error")
  }
  expect_identical(counter$queries, 0L)
  expect_identical(predicates, 0L)
  plan <- inspect_grouping(remote, .grouping = typed)
  expect_identical(plan$included, "(b)")
  expect_identical(counter$queries, 2L)
  expect_identical(predicates, 2L)
})

test_that("multi-set typed suffixes retain valid and allowed duplicate plans", {
  data <- data.frame(a = c("x", "y"), b = 1:2)
  suffix <- grouping_sets(grouping_set(where(is.integer)), grouping_set())
  valid <- grouping_spec(grouping_sets(a, grouping_set()), !!suffix)
  duplicate <- grouping_spec(grouping_sets(a, a), !!suffix)
  plan <- inspect_grouping(data, .grouping = valid)
  expect_identical(plan$included, c("(a, b)", "(a)", "(b)", "()"))
  expect_identical(plan$set_id, 1:4)
  expect_identical(plan$grouping_id, 0:3)

  plan <- inspect_grouping(data, .grouping = duplicate, .duplicates = "keep")
  expect_identical(plan$included, c("(a, b)", "(a)", "(a, b)", "(a)"))
  expect_identical(plan$set_id, 1:4)
  expect_identical(plan$grouping_id, c(0L, 1L, 0L, 1L))
  plan <- inspect_grouping(data, .grouping = duplicate, .duplicates = "drop")
  expect_identical(plan$included, c("(a, b)", "(a)"))
  expect_identical(plan$set_id, 1:2)
  expect_identical(plan$grouping_id, 0:1)

  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    expect_identical(
      verb(data, .grouping = valid),
      verb(data, .grouping = grouping_sets(
        grouping_set(a, b), a, b, grouping_set()
      ))
    )
    expect_identical(
      verb(data, .grouping = duplicate, .duplicates = "drop"),
      verb(data, .grouping = grouping_sets(grouping_set(a, b), a))
    )
    expect_error(verb(data, .grouping = duplicate, .duplicates = "keep"),
                 "must be one of", class = "marginplyr_error")
  }
})

test_that("completed duplicate groups precede incomplete product groups", {
  data <- data.frame(x = 1L, y = 2L, z = "value")
  spec <- grouping_sets(
    x, y, y, grouping_spec(grouping_sets(x, x), where(is.character))
  )
  expect_duplicate_example(expect_error(
    inspect_grouping(data, .grouping = spec), class = "marginplyr_error"
  ), "2, 3")
})

test_that("partial product factors refuse known duplicates before predicates", {
  data <- data.frame(x = 1L, value = 2L)
  predicates <- 0L
  predicate <- function(x) {
    predicates <<- predicates + 1L
    stop("typed selection evaluated")
  }
  specs <- list(
    grouping_spec(grouping_sets(x, x, where(predicate)), value),
    grouping_spec(
      grouping_sets(x, x, where(predicate)),
      grouping_sets(grouping_spec(value, grouping_set()))
    ),
    grouping_spec(
      grouping_sets(x, value),
      grouping_sets(x, grouping_set(), where(predicate)), value
    )
  )
  for (verb in forwarded_verbs[c(
    "inspect_grouping", "nest_with_margins", "nest_by_with_margins"
  )]) {
    for (spec in specs) {
      expect_error(verb(data, NULL, spec), "1, 2.", fixed = TRUE,
                   class = "marginplyr_error")
    }
    expect_error(verb(data, NULL, grouping_sets(value, !!specs[[1L]])),
                 "2, 3.", fixed = TRUE, class = "marginplyr_error")
    expect_error(verb(data, NULL, grouping_spec(
      grouping_sets(x, x, absent, where(predicate)), value
    )), "absent", class = "vctrs_error_subscript_oob")
  }
  expect_identical(predicates, 0L)
})

test_that("singleton wrappers retain leading partial product duplicates", {
  data <- data.frame(a = 1L, b = 2L, c = 3L)
  predicates <- 0L
  predicate <- function(x) {
    predicates <<- predicates + 1L
    stop("typed selection evaluated")
  }
  spec <- grouping_spec(
    grouping_sets(grouping_spec(grouping_sets(a, a, where(predicate)), b)), c
  )
  for (verb in forwarded_verbs[c(
    "inspect_grouping", "nest_with_margins", "nest_by_with_margins"
  )]) {
    expect_error(verb(data, NULL, spec), "1, 2.", fixed = TRUE,
                 class = "marginplyr_error")
    expect_error(verb(data, NULL, grouping_sets(b, !!spec)), "2, 3.",
                 fixed = TRUE, class = "marginplyr_error")
    expect_error(verb(data, NULL, grouping_spec(
      grouping_sets(grouping_spec(
        grouping_sets(a, a, absent, where(predicate)), b
      )), c
    )), "absent", class = "vctrs_error_subscript_oob")
    expect_error(verb(data, NULL, grouping_spec(
      grouping_sets(b, grouping_spec(grouping_sets(a, a), where(predicate))), b
    )), "2, 3.", fixed = TRUE, class = "marginplyr_error")
  }
  expect_identical(predicates, 0L)
})

test_that("partial product positions include preceding branches", {
  data <- data.frame(a = 1L, b = 2L, c = 3L)
  predicates <- 0L
  predicate <- function(x) {
    predicates <<- predicates + 1L
    stop("typed selection evaluated")
  }
  partial <- grouping_sets(
    b, c, grouping_spec(grouping_sets(a, a), where(predicate))
  )
  spec <- grouping_spec(!!partial, b)
  nested <- grouping_spec(grouping_sets(b, grouping_spec(!!partial, b)), c)
  for (verb in forwarded_verbs[c(
    "inspect_grouping", "nest_with_margins", "nest_by_with_margins"
  )]) {
    expect_error(verb(data, NULL, spec), "3, 4.", fixed = TRUE,
                 class = "marginplyr_error")
    expect_error(verb(data, NULL, grouping_sets(c, !!spec)), "4, 5.",
                 fixed = TRUE, class = "marginplyr_error")
    expect_error(verb(data, NULL, grouping_spec(
      grouping_sets(grouping_set(), c), !!partial, b, c
    )), "3, 4.", fixed = TRUE, class = "marginplyr_error")
    expect_error(verb(data, NULL, grouping_sets(c, grouping_spec(
      grouping_sets(grouping_set(), c), !!partial, b, c
    ))), "4, 5.", fixed = TRUE, class = "marginplyr_error")
    expect_error(verb(data, NULL, nested), "4, 5.", fixed = TRUE,
                 class = "marginplyr_error")
    expect_error(verb(data, NULL, grouping_sets(c, !!nested)), "5, 6.",
                 fixed = TRUE, class = "marginplyr_error")
  }
  expect_identical(predicates, 0L)
})

test_that("partial product offsets do not settle typed membership", {
  data <- data.frame(a = 1L, b = 2L, c = 3L)
  predicates <- 0L
  predicate <- function(x) {
    predicates <<- predicates + 1L
    stop("typed selection evaluated")
  }
  spec <- grouping_spec(
    grouping_sets(b, grouping_spec(grouping_sets(a, c), where(predicate))), b
  )
  for (verb in forwarded_verbs[c(
    "inspect_grouping", "nest_with_margins", "nest_by_with_margins"
  )]) {
    expect_error(verb(data, NULL, spec), "typed selection evaluated")
  }
  expect_identical(predicates, 3L)
})

test_that("partial products preserve typed decisions and duplicate policies", {
  data <- data.frame(x = 1L, value = 2L)
  for (policy in c("error", "keep", "drop")) {
    events <- character()
    fixed <- function(x) {
      events <<- c(events, "by")
      FALSE
    }
    predicate <- function(x) {
      events <<- c(events, "dimension")
      is.numeric(x)
    }
    spec <- grouping_spec(
      grouping_sets(x, value, where(predicate) & value), grouping_set()
    )
    if (identical(policy, "error")) {
      expect_error(inspect_grouping(
        data, .by = where(fixed), .grouping = spec, .duplicates = policy
      ), "2, 3.", fixed = TRUE, class = "marginplyr_error")
    } else {
      plan <- inspect_grouping(
        data, .by = where(fixed), .grouping = spec, .duplicates = policy,
        .format = "list"
      )
      expected <- if (identical(policy, "keep")) {
        list("x", "value", "value")
      } else {
        list("x", "value")
      }
      expect_identical(plan$included, expected)
      expect_identical(plan$set_id, seq_along(expected))
    }
    expect_identical(events, c("by", "by", "dimension", "dimension"))
  }
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    expected <- verb(data, .grouping = grouping_sets(
      grouping_set(x, value), value
    ))
    actual <- verb(data, .grouping = grouping_spec(
      grouping_sets(x, x, where(is.numeric) & value), value
    ), .duplicates = "drop")
    expect_identical(actual, expected)
  }
})

# Before typed metadata, the sets a typed selection will produce are unknown,
# so they can join any listed group or form a new one. Such a refusal names
# one group it has proved and says it is an example (#1616); the name
# spellings, resolved in full, keep the complete partition.
test_that("duplicates proved before typed selection are an example", {
  data <- data.frame(a = "x", n = 1L, value = 1)
  cases <- list(
    list(
      names = grouping_sets(a, n, a, n),
      typed = grouping_sets(a, n, a, where(is.integer)),
      partition = "2, 4; 1, 3",
      example = "1, 3"
    ),
    list(
      names = grouping_spec(cube(a, n), cube(a), n),
      typed = grouping_spec(cube(a, n), cube(a), where(is.integer)),
      partition = "6, 8; 1, 2, 3, 4, 5, 7",
      example = "1, 2, 5"
    )
  )
  inputs <- list(local = data)
  if (suggest_available("dtplyr")) {
    inputs$dtplyr <- dtplyr::lazy_dt(data)
  }
  diagnostic <- function(verb, input, spec) {
    cnd <- expect_error(verb(input, NULL, !!spec), class = "marginplyr_error")
    strsplit(conditionMessage(cnd), "\n", fixed = TRUE)[[1L]][1:2]
  }
  for (input in inputs) {
    for (verb in forwarded_verbs) {
      for (case in cases) {
        expect_identical(
          diagnostic(verb, input, case$names),
          c("Duplicate grouping sets were produced at position groups:",
            paste0("i ", case$partition, "."))
        )
        expect_identical(
          diagnostic(verb, input, case$typed),
          duplicate_example_lines(case$example)
        )
      }
    }
  }
})
