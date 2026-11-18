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

# A source holding a factor with an unused level, a Date with a missing value,
# and an integer; its serialized original; and a counter for the bindings a
# test installs to record.
row_call_fixture <- function() {
  fixture <- new.env(parent = emptyenv())
  fixture$source <- data.table::data.table(
    g = factor(c("a", "a", "b", "b"), levels = c("a", "b", "unused")),
    day = as.Date(c("2026-01-01", "2026-01-01", NA, "2026-01-02")),
    value = c(1L, 1L, 2L, 3L)
  )
  fixture$before <- serialize(fixture$source, NULL)
  fixture$calls <- 0L
  fixture
}

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

# A function that records its call and returns its input with `g` recast, so a
# metadata proxy built through it would be unfaithful.
row_call_replacement <- function(fixture) {
  function(x, ...) {
    fixture$calls <- fixture$calls + 1L
    data.table::set(x, j = "g", value = as.character(x$g))
    x
  }
}

row_call_refused <- function(step, fixture, info) {
  expect_error(inspect_grouping(
    step, .grouping = grouping_set(tidyselect::where(is.factor))
  ), "Can't safely determine column metadata", fixed = TRUE,
  class = "marginplyr_error", info = info)
  for (verb in forwarded_margin_verbs) {
    expect_error(verb(step, NULL, rollup(tidyselect::where(is.factor))),
                 "Can't safely determine column metadata", fixed = TRUE,
                 class = "marginplyr_error", info = info)
  }
  expect_identical(fixture$calls, 0L, info = info)
  expect_identical(serialize(fixture$source, NULL), fixture$before,
                   info = info)
}

test_that("row-only dtplyr calls keep typed metadata without reading rows", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("tidyr")
  fixture <- row_call_fixture()
  probe <- function(x) {
    fixture$calls <- fixture$calls + 1L
    x > 0L
  }
  root <- dtplyr::lazy_dt(fixture$source)
  filtered <- dplyr::filter(root, probe(value))
  steps <- c(row_call_steps(root), list(
    filtered_head = utils::head(filtered, 2L),
    filtered_distinct = dplyr::distinct(filtered),
    filtered_drop_na = tidyr::drop_na(filtered)
  ))
  for (name in names(steps)) {
    step <- steps[[name]]
    expect_identical(inspect_grouping(
      step, .grouping = rollup(where(is.factor), where(is.integer))
    )$included, c("(g, value)", "(g)", "()"), info = name)
    expect_identical(inspect_grouping(
      step, .by = where(function(x) inherits(x, "Date"))
    )$fixed, "(day)", info = name)
    expect_identical(fixture$calls, 0L, info = name)
    for (verb in names(forwarded_margin_verbs)) {
      info <- paste(name, verb)
      forward <- forwarded_margin_verbs[[verb]]
      result <- forward(step, NULL, rollup(where(is.factor)))
      # nest_by_with_margins() collects a lazy input at its documented boundary.
      if (!identical(verb, "nest_by_with_margins")) {
        expect_identical(fixture$calls, 0L, info = info)
      }
      expect_identical(row_call_result(result), row_call_result(
        forward(dplyr::collect(step), NULL, rollup(where(is.factor)))
      ), info = info)
      fixture$calls <- 0L
    }
    expect_identical(serialize(fixture$source, NULL), fixture$before,
                     info = name)
  }
})

test_that("row-only calls refuse a caller generic or method before it runs", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("tidyr")
  for (generic in names(row_call_dispatch)) {
    for (name in paste0(generic, c("", ".data.table"))) {
      fixture <- row_call_fixture()
      caller <- new.env(parent = globalenv())
      caller[[name]] <- row_call_replacement(fixture)
      root <- eval(rlang::expr(dtplyr::lazy_dt(!!fixture$source)), caller)
      step <- row_call_dispatch[[generic]]$step(root)
      row_call_refused(step, fixture, info = name)
      dplyr::collect(step)
      expect_gt(fixture$calls, 0L)
    }
  }
})

test_that("row-only calls leave uncertain lexical bindings unread", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("tidyr")
  for (generic in names(row_call_dispatch)) {
    owner <- row_call_dispatch[[generic]]$owner
    bound <- list(get(generic, envir = owner),
                  utils::getS3method(generic, "data.table"))
    names(bound) <- paste0(generic, c("", ".data.table"))
    for (name in names(bound)) {
      for (binding in c("active", "lazy")) {
        fixture <- row_call_fixture()
        read_binding <- function() {
          fixture$calls <- fixture$calls + 1L
          bound[[name]]
        }
        caller <- new.env(parent = globalenv())
        if (binding == "active") {
          makeActiveBinding(name, read_binding, caller)
        } else {
          delayedAssign(name, read_binding(), assign.env = caller)
        }
        root <- eval(rlang::expr(dtplyr::lazy_dt(!!fixture$source)), caller)
        step <- row_call_dispatch[[generic]]$step(root)
        row_call_refused(step, fixture, info = paste(name, binding))
      }
    }
  }
})

test_that("row-only calls refuse a replaced data.table registration", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("tidyr")
  for (generic in names(row_call_dispatch)) {
    for (attached in c(FALSE, TRUE)) local({
      owner <- row_call_dispatch[[generic]]$owner
      original <- utils::getS3method(generic, "data.table")
      on.exit(registerS3method(generic, "data.table", original, envir = owner),
              add = TRUE)
      fixture <- row_call_fixture()
      registerS3method(generic, "data.table", row_call_replacement(fixture),
                       envir = owner)
      if (attached) {
        # Dispatch reaches the registry before the search path, so an attached
        # copy of data.table's own method does not decide what runs.
        methods <- list(original)
        names(methods) <- paste0(generic, ".data.table")
        attach(methods, name = "marginplyr_row_call_methods")
        on.exit(detach("marginplyr_row_call_methods"), add = TRUE)
      }
      step <- row_call_dispatch[[generic]]$step(
        dtplyr::lazy_dt(fixture$source)
      )
      row_call_refused(step, fixture, info = paste(generic, attached))
      dplyr::collect(step)
      expect_gt(fixture$calls, 0L)
    })
  }
})

test_that("row-only calls refuse argument shapes dtplyr does not generate", {
  skip_if_suggest_absent("dtplyr")
  fixture <- row_call_fixture()
  root <- dtplyr::lazy_dt(fixture$source)
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
    row_call_refused(step, fixture, info = deparse(case[[2L]]))
  }
})

test_that("row-only calls do not admit a refused parent", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("tidyr")
  fixture <- row_call_fixture()
  probe <- function(x) {
    fixture$calls <- fixture$calls + 1L
    x
  }
  root <- dtplyr::lazy_dt(fixture$source)
  for (parent in list(dplyr::mutate(root, g = factor(probe(value))),
                      dplyr::mutate(root, day = probe(day)))) {
    for (step in row_call_steps(parent)) {
      row_call_refused(step, fixture, info = class(step)[[1L]])
    }
  }
})

test_that("Mutable row-only calls are inspected in isolation and refused", {
  skip_if_suggest_absent("dtplyr")
  skip_if_suggest_absent("tidyr")
  fixture <- row_call_fixture()
  root <- dtplyr::lazy_dt(fixture$source, immutable = FALSE)
  for (step in row_call_steps(root)) {
    expect_identical(inspect_grouping(
      step, .grouping = grouping_set(where(is.factor))
    )$included, "(g)")
    for (verb in forwarded_margin_verbs) {
      expect_error(verb(step, NULL, rollup(where(is.factor))),
                   "immutable = FALSE", fixed = TRUE,
                   class = "marginplyr_error")
    }
    expect_identical(serialize(fixture$source, NULL), fixture$before)
  }
})
