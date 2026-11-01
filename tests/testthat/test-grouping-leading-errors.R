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
  counter <- new.env(parent = emptyenv())
  counter$queries <- 0L
  # Observe the actual DBI method, beyond construction-time Sent query records.
  # do.call retains the tracer's closure when S4 tracing wraps .local lazily.
  suppressMessages(do.call(trace, list(
    what = "dbSendQuery", signature = c("duckdb_connection", "character"),
    where = asNamespace("DBI"), print = FALSE,
    tracer = function() {
      counter$queries <- counter$queries + 1L
    }
  )))
  on.exit(suppressMessages(untrace(
    "dbSendQuery", signature = c("duckdb_connection", "character"),
    where = asNamespace("DBI")
  )), add = TRUE)
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
})
