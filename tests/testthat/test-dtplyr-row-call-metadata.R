# dtplyr represents these as `dtplyr_step_call`s that only remove rows.
row_call_steps <- function(root) {
  list(
    head = utils::head(root, 2L),
    distinct = dplyr::distinct(root),
    distinct_keep_all = dplyr::distinct(
      root, !!rlang::sym("g"), .keep_all = TRUE
    ),
    drop_na = tidyr::drop_na(root),
    drop_na_cols = tidyr::drop_na(root, tidyselect::all_of("day"))
  )
}

# Nested cells compare as tibbles whichever frame class the input carried.
row_call_result <- function(x) {
  x <- dplyr::ungroup(tibble::as_tibble(dplyr::collect(x)))
  if ("data" %in% names(x)) {
    x$data <- lapply(x$data, tibble::as_tibble)
  }
  x
}

row_call_source <- function() {
  data.table::data.table(
    g = factor(c("a", "a", "b", "b"), levels = c("a", "b", "unused")),
    day = as.Date(c("2026-01-01", "2026-01-01", NA, "2026-01-02")),
    value = c(1L, 1L, 2L, 3L)
  )
}

test_that("row-only dtplyr calls keep typed metadata without reading rows", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("tidyr")
  source <- row_call_source()
  before <- serialize(source, NULL)
  calls <- 0L
  probe <- function(x) {
    calls <<- calls + 1L
    x > 0L
  }
  root <- dtplyr::lazy_dt(source)
  filtered <- dplyr::filter(root, probe(value))
  steps <- c(row_call_steps(root), list(
    filtered_head = utils::head(filtered, 2L),
    filtered_distinct = dplyr::distinct(filtered)
  ))
  for (name in names(steps)) {
    step <- steps[[name]]
    expect_identical(inspect_grouping(
      step, .grouping = rollup(where(is.factor), where(is.integer))
    )$included, c("(g, value)", "(g)", "()"), info = name)
    expect_identical(inspect_grouping(
      step, .by = where(function(x) inherits(x, "Date"))
    )$fixed, "(day)", info = name)
    expect_identical(calls, 0L, info = name)
    for (verb in names(forwarded_margin_verbs)) {
      info <- paste(name, verb)
      forward <- forwarded_margin_verbs[[verb]]
      result <- forward(step, NULL, rollup(where(is.factor)))
      # nest_by_with_margins() collects a lazy input at its documented boundary.
      if (!identical(verb, "nest_by_with_margins")) {
        expect_identical(calls, 0L, info = info)
      }
      expect_identical(row_call_result(result), row_call_result(
        forward(dplyr::collect(step), NULL, rollup(where(is.factor)))
      ), info = info)
      calls <- 0L
    }
    expect_identical(serialize(source, NULL), before, info = name)
  }
})

# Each translation's generic, the namespace whose S3 table registers its
# data.table method, and a step that generates it.
row_call_dispatch <- list(
  head = list(owner = asNamespace("utils"), step = function(root) {
    utils::head(root, 2L)
  }),
  unique = list(owner = baseenv(), step = dplyr::distinct),
  na.omit = list(owner = asNamespace("stats"), step = function(root) {
    tidyr::drop_na(root)
  })
)

# A caller binding for `name` that records each call and returns its input
# with `g` recast, so a metadata proxy built through it would be unfaithful.
row_call_caller <- function(name, calls) {
  binding_env <- new.env(parent = globalenv())
  binding_env[[name]] <- function(x, ...) {
    calls$n <- calls$n + 1L
    data.table::set(x, j = "g", value = as.character(x$g))
    x
  }
  binding_env
}

row_call_refused <- function(step, calls, source, before, info) {
  expect_error(inspect_grouping(
    step, .grouping = grouping_set(tidyselect::where(is.factor))
  ), "Can't safely determine column metadata", fixed = TRUE,
  class = "marginplyr_error", info = info)
  for (verb in forwarded_margin_verbs) {
    expect_error(verb(step, NULL, rollup(tidyselect::where(is.factor))),
                 "Can't safely determine column metadata", fixed = TRUE,
                 class = "marginplyr_error", info = info)
  }
  expect_identical(calls$n, 0L, info = info)
  expect_identical(serialize(source, NULL), before, info = info)
}

test_that("row-only calls refuse a caller generic or method before it runs", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("tidyr")
  for (generic in names(row_call_dispatch)) {
    for (name in paste0(generic, c("", ".data.table"))) {
      source <- row_call_source()
      before <- serialize(source, NULL)
      calls <- new.env(parent = emptyenv())
      calls$n <- 0L
      caller <- row_call_caller(name, calls)
      root <- eval(rlang::expr(dtplyr::lazy_dt(!!source)), caller)
      step <- row_call_dispatch[[generic]]$step(root)
      row_call_refused(step, calls, source, before, info = name)
      dplyr::collect(step)
      expect_gt(calls$n, 0L)
    }
  }
})

test_that("row-only calls leave uncertain lexical bindings unread", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("tidyr")
  for (generic in names(row_call_dispatch)) {
    for (binding in c("active", "lazy")) {
      source <- row_call_source()
      before <- serialize(source, NULL)
      calls <- new.env(parent = emptyenv())
      calls$n <- 0L
      read_binding <- function() {
        calls$n <- calls$n + 1L
        get(generic, envir = row_call_dispatch[[generic]]$owner)
      }
      caller <- new.env(parent = globalenv())
      if (binding == "active") {
        makeActiveBinding(generic, read_binding, caller)
      } else {
        delayedAssign(generic, read_binding(), assign.env = caller)
      }
      root <- eval(rlang::expr(dtplyr::lazy_dt(!!source)), caller)
      step <- row_call_dispatch[[generic]]$step(root)
      row_call_refused(step, calls, source, before,
                       info = paste(generic, binding))
    }
  }
})

test_that("row-only calls refuse a replaced data.table registration", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("tidyr")
  for (generic in names(row_call_dispatch)) local({
    owner <- row_call_dispatch[[generic]]$owner
    original <- utils::getS3method(generic, "data.table")
    on.exit(registerS3method(generic, "data.table", original, envir = owner),
            add = TRUE)
    source <- row_call_source()
    before <- serialize(source, NULL)
    calls <- new.env(parent = emptyenv())
    calls$n <- 0L
    replacement <- row_call_caller(generic, calls)[[generic]]
    registerS3method(generic, "data.table", replacement, envir = owner)
    step <- row_call_dispatch[[generic]]$step(dtplyr::lazy_dt(source))
    row_call_refused(step, calls, source, before, info = generic)
  })
})

test_that("row-only calls refuse argument shapes dtplyr does not generate", {
  skip_if_suggest_absent("dtplyr")
  source <- row_call_source()
  before <- serialize(source, NULL)
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  root <- dtplyr::lazy_dt(source)
  head <- utils::head(root, 2L)
  distinct <- dplyr::distinct(root)
  args <- list(
    list(head, list(n = structure(2L, class = "marginplyr_test_n"))),
    list(head, list(n = c(1L, 2L))),
    list(head, list(n = NA_integer_)),
    list(head, list(n = 2L, extra = 1L)),
    list(distinct, list(by = "missing")),
    list(distinct, list(fromLast = TRUE))
  )
  for (case in args) {
    step <- case[[1L]]
    step$args <- case[[2L]]
    row_call_refused(step, calls, source, before,
                     info = deparse(case[[2L]]))
  }
})

test_that("row-only calls do not admit a refused parent", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("tidyr")
  source <- row_call_source()
  before <- serialize(source, NULL)
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  probe <- function(x) {
    calls$n <- calls$n + 1L
    x
  }
  root <- dtplyr::lazy_dt(source)
  for (parent in list(dplyr::mutate(root, g = factor(probe(value))),
                      dplyr::mutate(root, day = probe(day)))) {
    for (step in row_call_steps(parent)) {
      row_call_refused(step, calls, source, before, info = class(step)[[1L]])
    }
  }
})

test_that("Mutable row-only calls are inspected in isolation and refused", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("tidyr")
  source <- row_call_source()
  before <- serialize(source, NULL)
  for (step in row_call_steps(dtplyr::lazy_dt(source, immutable = FALSE))) {
    expect_identical(inspect_grouping(
      step, .grouping = grouping_set(where(is.factor))
    )$included, "(g)")
    for (verb in forwarded_margin_verbs) {
      expect_error(verb(step, NULL, rollup(where(is.factor))),
                   "immutable = FALSE", fixed = TRUE,
                   class = "marginplyr_error")
    }
    expect_identical(serialize(source, NULL), before)
  }
})
