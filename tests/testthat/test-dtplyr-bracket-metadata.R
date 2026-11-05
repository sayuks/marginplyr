test_that("typed derived plans refuse caller brackets without running them", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only dtplyr is an optional backend here.
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  calls <- integer()
  step <- local({
    `[` <- function(x, ...) {
      calls <<- c(calls, nrow(x))
      result <- base::`[`(x, ...)
      if (nrow(result) == 0L) {
        data.table::set(result, j = "g", value = integer())
      }
      result
    }
    dplyr::mutate(dtplyr::lazy_dt(source), derived = value + 1L)
  })
  explicit <- dplyr::collect(step)
  expect_identical(explicit, tibble::tibble(
    g = c("a", "b"), value = 1:2, derived = 2:3
  ))
  expect_identical(calls, 2L)
  calls <- integer()
  expect_identical(
    inspect_grouping(step, .grouping = rollup(g)),
    inspect_grouping(explicit, .grouping = rollup(g))
  )
  expect_identical(calls, integer())
  expect_error(
    inspect_grouping(step, .grouping = rollup(where(is.character))),
    "Can't safely determine column metadata", fixed = TRUE,
    class = "marginplyr_error"
  )
  expect_identical(calls, integer())
  expect_identical(source, before)
})

test_that("fixed keys and both Nest verbs refuse a caller bracket before use", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  calls <- integer()
  step <- local({
    `[` <- function(x, ...) {
      calls <<- c(calls, nrow(x))
      result <- base::`[`(x, ...)
      if (nrow(result) == 0L) {
        data.table::set(result, j = "g", value = integer())
      }
      result
    }
    dplyr::mutate(dtplyr::lazy_dt(source), derived = value + 1L)
  })
  expect_error(inspect_grouping(step, .by = where(is.character)),
               "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    expect_error(verb(step, .by = where(is.character)),
                 "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
    expect_error(verb(step, .grouping = rollup(where(is.character))),
                 "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
  }
  expect_identical(calls, integer())
  expect_identical(source, before)
})

test_that("typed derived metadata does not look up active or lazy brackets", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  for (inherited in c(FALSE, TRUE)) {
    for (lazy in c(FALSE, TRUE)) {
      lookups <- 0L
      binding_env <- new.env(parent = environment())
      if (lazy) {
        delayedAssign("[", {
          lookups <<- lookups + 1L
          base::`[`
        }, assign.env = binding_env)
      } else {
        makeActiveBinding("[", function() {
          lookups <<- lookups + 1L
          base::`[`
        }, binding_env)
      }
      caller <- if (inherited) new.env(parent = binding_env) else binding_env
      step <- eval(quote(dplyr::mutate(
        dtplyr::lazy_dt(source), derived = value + 1L
      )), caller)
      expect_identical(inspect_grouping(step, .grouping = rollup(g))$included,
                       c("(g)", "()"))
      expect_error(inspect_grouping(step, .by = where(is.character)),
                   class = "marginplyr_error")
      expect_error(inspect_grouping(
        step, .grouping = rollup(where(is.character))
      ), class = "marginplyr_error")
      for (verb in list(nest_with_margins, nest_by_with_margins)) {
        expect_error(verb(step, .by = where(is.character)),
                     class = "marginplyr_error")
      }
      expect_identical(lookups, 0L)
      expect_identical(source, before)
    }
  }
})

test_that("subset metadata respects bracket dispatch even without evaluation", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  calls <- integer()
  root <- local({
    `[` <- function(x, ...) {
      calls <<- c(calls, nrow(x))
      result <- data.table::copy(base::`[`(x, ...))
      data.table::set(result, j = "g", value = seq_len(nrow(result)))
      result
    }
    dtplyr::lazy_dt(source)
  })
  for (step in list(dplyr::filter(root, value > 0L),
                    dplyr::select(root, g))) {
    explicit <- dplyr::collect(step)
    expect_identical(explicit$g, 1:2)
    expect_identical(calls, 2L)
    calls <- integer()
    expect_identical(inspect_grouping(step, .grouping = rollup(g))$included,
                     c("(g)", "()"))
    expect_error(inspect_grouping(step, .by = where(is.character)),
                 "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
    expect_identical(calls, integer())
    expect_identical(source, before)
  }
})

test_that("join metadata checks the effective outer bracket environment", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  calls <- integer()
  binding_env <- new.env(parent = environment())
  makeActiveBinding("[", function() {
    calls <<- c(calls, 1L)
    base::`[`
  }, binding_env)
  caller <- new.env(parent = binding_env)
  masked <- eval(quote(dtplyr::lazy_dt(source)), caller)
  ordinary <- dtplyr::lazy_dt(source)
  left <- dplyr::mutate(masked, derived = value + 1L)
  safe <- dplyr::left_join(left, ordinary, by = c("g", "value"))
  explicit <- dplyr::collect(safe)
  expect_identical(explicit$g, c("a", "b"))
  expect_identical(calls, integer())
  expect_identical(
    inspect_grouping(safe, .grouping = rollup(where(is.character))),
    inspect_grouping(explicit, .grouping = rollup(g))
  )
  unsafe <- dplyr::left_join(ordinary, masked, by = c("g", "value"))
  expect_error(inspect_grouping(unsafe, .by = where(is.character)),
               "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
  expect_identical(calls, integer())
  expect_identical(source, before)
})

test_that("base brackets preserve typed derived plans and nesting cells", {
  skip_if_suggest_absent("dtplyr")
  normalize <- function(x) {
    x <- dplyr::ungroup(dplyr::collect(x))
    x$data <- lapply(x$data, tibble::as_tibble)
    x
  }
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  step <- local({
    `[` <- base::`[`
    dplyr::mutate(dtplyr::lazy_dt(source), derived = value + 1L)
  })
  explicit <- dplyr::collect(step)
  expect_identical(explicit$derived, 2:3)
  expect_identical(
    inspect_grouping(step, .grouping = rollup(where(is.character))),
    inspect_grouping(explicit, .grouping = rollup(g))
  )
  expect_identical(
    inspect_grouping(step, .by = where(is.character)),
    inspect_grouping(explicit, .by = g)
  )
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    expect_identical(
      normalize(verb(step, .grouping = rollup(where(is.character)))),
      normalize(verb(explicit, .grouping = rollup(g)))
    )
    expect_identical(normalize(verb(step, .by = where(is.character))),
                     normalize(verb(explicit, .by = g)))
  }
  expect_identical(source, before)
})

test_that("root metadata needs no caller bracket binding", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  lookups <- 0L
  caller <- new.env(parent = environment())
  makeActiveBinding("[", function() {
    lookups <<- lookups + 1L
    stop("The caller bracket must not be looked up")
  }, caller)
  root <- eval(quote(dtplyr::lazy_dt(source)), caller)
  explicit <- dplyr::collect(root)
  expect_identical(explicit, tibble::tibble(g = c("a", "b"), value = 1:2))
  expect_identical(
    inspect_grouping(root, .grouping = rollup(where(is.character))),
    inspect_grouping(explicit, .grouping = rollup(g))
  )
  expect_identical(inspect_grouping(root, .by = where(is.character)),
                   inspect_grouping(explicit, .by = g))
  expect_identical(lookups, 0L)
  expect_identical(source, before)
})
