test_that("active arithmetic cannot establish typed dtplyr metadata", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(value = 1:2)
  before <- data.table::copy(source)
  lookups <- 0L
  seen <- integer()
  operator_env <- new.env(parent = environment())
  custom <- function(a, b) {
    seen <<- c(seen, length(a))
    if (length(a) == 0L) character() else base::`+`(a, b)
  }
  makeActiveBinding("+", function() {
    lookups <<- lookups + 1L
    if (lookups == 3L) custom else base::`+`
  }, operator_env)
  step <- eval(quote(dplyr::mutate(
    dtplyr::lazy_dt(source), derived = value + 1L
  )), operator_env)

  for (i in seq_len(5L)) {
    collected <- dplyr::collect(step)
    expect_identical(collected$derived, 2:3)
  }
  expect_identical(inspect_grouping(
    collected, .by = where(is.character), .format = "list"
  )$fixed, list(character()))
  lookups <- 0L
  seen <- integer()
  error <- expect_error(
    inspect_grouping(step, .by = where(is.character)),
    class = "marginplyr_error"
  )
  expect_match(conditionMessage(error), "dplyr::collect()", fixed = TRUE)
  expect_identical(lookups, 0L)
  expect_identical(seen, integer())
  expect_identical(source, before)
})

test_that("inherited and nested active arithmetic is refused before lookup", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(value = 1:2, g = c("a", "a"))
  before <- data.table::copy(source)
  for (operator in c("+", "-", "*", "/")) {
    for (inherited in c(FALSE, TRUE)) {
      for (nested in c(FALSE, TRUE)) {
        lookups <- 0L
        operator_env <- new.env(parent = environment())
        makeActiveBinding(operator, function() {
          lookups <<- lookups + 1L
          get(operator, envir = baseenv())
        }, operator_env)
        caller <- operator_env
        if (inherited) {
          caller <- new.env(parent = operator_env)
        }
        arithmetic <- as.call(list(as.name(operator), quote(value), 1L))
        if (nested) {
          arithmetic <- as.call(list(as.name("*"), arithmetic, 2L))
        }
        step <- eval(rlang::expr(dplyr::mutate(
          dtplyr::lazy_dt(source), derived = !!arithmetic
        )), caller)
        collected <- dplyr::collect(step)
        expect_true(is.numeric(collected$derived))
        lookups <- 0L
        expect_error(inspect_grouping(step, .by = where(is.character)),
                     "dplyr::collect()", fixed = TRUE,
                     class = "marginplyr_error")
        expect_error(inspect_grouping(
          step, .grouping = rollup(where(is.character))
        ), class = "marginplyr_error")
        for (verb in list(summarize_with_margins, expand_with_margins,
                          nest_with_margins, nest_by_with_margins)) {
          expect_error(verb(step, .by = where(is.character)),
                       "dplyr::collect()", fixed = TRUE,
                       class = "marginplyr_error")
        }
        expect_identical(lookups, 0L)
        expect_identical(source, before)
      }
    }
  }
})

test_that("ordinary base arithmetic retains typed plans and nesting cells", {
  skip_if_suggest_absent("dtplyr")
  normalize <- function(x) {
    x <- dplyr::ungroup(dplyr::collect(x))
    x$data <- lapply(x$data, tibble::as_tibble)
    x
  }
  for (rows in c(0L, 1L, 3L)) {
    source <- data.table::data.table(value = seq_len(rows))
    before <- data.table::copy(source)
    for (operator in c("+", "-", "*", "/")) {
      caller <- new.env(parent = environment())
      assign(operator, get(operator, envir = baseenv()), envir = caller)
      arithmetic <- as.call(list(as.name(operator), quote(value), 1L))
      step <- eval(rlang::expr(dplyr::mutate(
        dtplyr::lazy_dt(source), derived = !!arithmetic
      )), caller)
      collected <- dplyr::collect(step)
      for (predicate in list(is.integer, is.double, is.character)) {
        expect_identical(
          inspect_grouping(step, .by = where(predicate)),
          inspect_grouping(collected, .by = where(predicate))
        )
        expect_identical(
          inspect_grouping(step, .grouping = grouping_set(where(predicate))),
          inspect_grouping(collected,
                           .grouping = grouping_set(where(predicate)))
        )
      }
      for (verb in list(nest_with_margins, nest_by_with_margins)) {
        actual <- normalize(verb(step, .by = where(is.character)))
        expected <- normalize(verb(collected, .by = where(is.character)))
        expect_identical(actual, expected)
        if (rows > 0L) {
          expect_identical(nrow(actual), 1L)
          expect_identical(actual$data[[1L]], collected)
        }
      }
      expect_identical(source, before)
    }
  }
})

test_that("ordinary value-dependent arithmetic stays refused without running", {
  skip_if_suggest_absent("dtplyr")
  seen <- integer()
  `+` <- function(a, b) {
    seen <<- c(seen, length(a))
    if (length(a) == 0L) character() else base::`+`(a, b)
  }
  source <- data.table::data.table(value = 1:2)
  before <- data.table::copy(source)
  step <- dplyr::mutate(dtplyr::lazy_dt(source), derived = value + 1L)
  collected <- dplyr::collect(step)
  expect_identical(collected$derived, 2:3)
  seen <- integer()
  expect_error(inspect_grouping(step, .by = where(is.character)),
               class = "marginplyr_error")
  for (verb in list(summarize_with_margins, expand_with_margins,
                    nest_with_margins, nest_by_with_margins)) {
    expect_error(verb(step, .by = where(is.character)),
                 class = "marginplyr_error")
  }
  expect_identical(seen, integer())
  expect_identical(source, before)
})

test_that("unbound arithmetic cannot establish typed metadata", {
  skip_if_suggest_absent("dtplyr")
  caller <- new.env(parent = emptyenv())
  caller$mutate <- dplyr::mutate
  caller$lazy <- dtplyr::lazy_dt
  caller$source <- data.table::data.table(value = 1:2)
  before <- data.table::copy(caller$source)
  step <- eval(quote(mutate(lazy(source), derived = value + 1L)), caller)
  expect_error(inspect_grouping(step, .by = where(is.numeric)),
               "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
  expect_identical(caller$source, before)
})

test_that("typed join metadata refuses arithmetic masked at execution", {
  skip_if_suggest_absent("dtplyr")
  left_source <- data.table::data.table(id = 1L, g = "a", v = 10L)
  right_source <- data.table::data.table(id = 1L)
  left_before <- data.table::copy(left_source)
  right_before <- data.table::copy(right_source)

  left <- dplyr::mutate(dtplyr::lazy_dt(left_source), w = v + 1L)
  sizes <- integer()
  right <- local({
    `+` <- function(x, y) {
      sizes <<- c(sizes, length(x))
      if (length(x) == 0L) character() else base::`+`(x, y)
    }
    dtplyr::lazy_dt(right_source)
  })
  joined <- dplyr::left_join(left, right, by = "id")
  expect_identical(dplyr::collect(joined)$w, 11L)
  expect_identical(sizes, 1L)

  sizes <- integer()
  expect_error(
    inspect_grouping(joined, .grouping = grouping_set(where(is.character))),
    "Can't safely determine column metadata", fixed = TRUE,
    class = "marginplyr_error"
  )
  expect_error(
    expand_with_margins(joined, .grouping = rollup(where(is.character))),
    "Can't safely determine column metadata", fixed = TRUE,
    class = "marginplyr_error"
  )
  expect_identical(sizes, integer())
  expect_identical(left_source, left_before)
  expect_identical(right_source, right_before)
})

test_that("typed join metadata leaves an unused operator promise unforced", {
  skip_if_suggest_absent("dtplyr")
  left_source <- data.table::data.table(id = 1L, g = "a", v = 10L)
  right_source <- data.table::data.table(id = 1L)
  left_before <- data.table::copy(left_source)
  right_before <- data.table::copy(right_source)
  reads <- integer()
  track_rows <- function(x) {
    reads <<- c(reads, length(x))
    x > 0L
  }
  make_left <- function(`+`) {
    dplyr::mutate(dtplyr::lazy_dt(left_source), w = v + 1L) |>
      dplyr::filter(track_rows(v))
  }
  forced <- FALSE
  left_inputs <- list(
    make_left(),
    make_left({
      forced <<- TRUE
      stop("The join does not use this operator promise")
    })
  )
  right <- dtplyr::lazy_dt(right_source)
  expected <- tibble::tibble(
    g = c("a", "Total"), sid = 1:2, id = c(1L, 1L),
    v = c(10L, 10L), w = c(11L, 11L)
  )

  for (left in left_inputs) {
    joined <- dplyr::left_join(left, right, by = "id")
    selection <- grouping_set(where(is.character))
    explicit <- dplyr::collect(joined)
    expect_identical(explicit$w, 11L)
    expect_false(forced)
    expect_identical(reads, 1L)

    reads <- integer()
    plan <- inspect_grouping(joined, .grouping = selection)
    expect_identical(plan$included, "(g)")
    expect_identical(plan, inspect_grouping(explicit, .grouping = selection))
    expanded <- expand_with_margins(
      joined, .grouping = rollup(where(is.character)),
      .id = "sid", .sort = "last"
    )
    expect_s3_class(expanded, "dtplyr_step")
    expect_identical(reads, integer())
    expect_false(forced)

    expect_identical(dplyr::collect(expanded), expected)
    expect_identical(reads, 1L)
    expect_false(forced)
    expect_identical(left_source, left_before)
    expect_identical(right_source, right_before)
    reads <- integer()
  }
})

test_that("typed join metadata refuses unavailable arithmetic at execution", {
  skip_if_suggest_absent("dtplyr")
  left <- dtplyr::lazy_dt(data.frame(id = 1L, v = 10L)) |>
    dplyr::mutate(w = v + 1L)
  make_right <- function(`+`) dtplyr::lazy_dt(data.frame(id = 1L))
  forced <- FALSE
  right <- make_right({
    forced <<- TRUE
    base::`+`
  })
  for (operand in list(right, make_right())) {
    joined <- dplyr::left_join(left, operand, by = "id")
    expect_error(
      inspect_grouping(joined, .grouping = grouping_set(where(is.character))),
      "Can't safely determine column metadata", fixed = TRUE,
      class = "marginplyr_error"
    )
  }
  expect_false(forced)
})

test_that("join metadata prepares zero rows without calling a lexical head", {
  skip_if_suggest_absent("dtplyr")
  left_source <- data.table::data.table(id = 1L, g = "a", v = 10L)
  left <- dplyr::mutate(dtplyr::lazy_dt(left_source), w = v + 1L)
  reads <- integer()
  right <- local({
    head <- function(x, n) {
      if ("w" %in% names(x) && !"r" %in% names(x)) {
        reads <<- left_source$v
      }
      utils::head(x, n = n)
    }
    dtplyr::lazy_dt(data.frame(id = 1L, r = 9L))
  })
  joined <- dplyr::left_join(left, right, by = "id")
  explicit <- dplyr::collect(joined)
  expect_identical(explicit$w, 11L)
  expect_identical(reads, integer())

  plan <- inspect_grouping(
    joined, .grouping = grouping_set(where(is.character))
  )
  expect_identical(plan$included, "(g)")
  expanded <- expand_with_margins(
    joined, .grouping = rollup(where(is.character))
  )
  expect_s3_class(expanded, "dtplyr_step")
  expect_identical(reads, integer())
})
