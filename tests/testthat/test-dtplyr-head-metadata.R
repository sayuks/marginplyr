# Replace the external S3 dispatch boundary and restore it even on failure.
# The caller has made dtplyr (which imports data.table) available.
local_dtplyr_head <- function(mutating = FALSE, .env = parent.frame()) {
  fixture <- new.env(parent = emptyenv())
  original <- utils::getS3method("head", "data.table")
  fixture$restore <- function() {
    registerS3method("head", "data.table", original,
                     envir = asNamespace("utils"))
  }
  withr::defer(fixture$restore(), envir = .env)
  fixture$rows <- integer()
  replacement <- function(x, n = 6L, ...) {
    fixture$rows <- c(fixture$rows, nrow(x))
    if (mutating) {
      data.table::set(x, j = "g", value = rep("changed", nrow(x)))
    }
    out <- original(x, n = n, ...)
    if (!mutating && nrow(out) == 0L) {
      data.table::set(out, j = "g", value = integer())
    }
    out
  }
  registerS3method("head", "data.table", replacement,
                   envir = asNamespace("utils"))
  fixture
}

test_that("typed inspection refuses registered head replacement", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only dtplyr is an optional backend here.
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- serialize(source, NULL)
  root <- dtplyr::lazy_dt(source)
  fixture <- local_dtplyr_head()
  expect_error(inspect_grouping(
    root, .grouping = grouping_set(where(is.character))
  ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
  expect_identical(fixture$rows, integer())
  expect_identical(serialize(source, NULL), before)
  fixture$restore()
  expect_identical(inspect_grouping(
    root, .grouping = grouping_set(where(is.character))
  )$included, "(g)")
})

test_that("registered head cannot alter derived dimensions or fixed keys", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- serialize(source, NULL)
  root <- dtplyr::lazy_dt(source)
  for (step in list(root, dplyr::mutate(root, derived = value + 1L),
                    dplyr::filter(root, value > 0L))) local({
    fixture <- local_dtplyr_head()
    expect_identical(dplyr::collect(step)$g, c("a", "b"))
    expect_identical(inspect_grouping(
      step, .grouping = grouping_set(g)
    )$included, "(g)")
    expect_identical(inspect_grouping(step, .by = g)$fixed, "(g)")
    for (verb in list(inspect_grouping, nest_with_margins,
                      nest_by_with_margins)) {
      expect_error(verb(step, .by = where(is.character)),
                   "Can't safely determine column metadata", fixed = TRUE,
                   class = "marginplyr_error")
      expect_error(verb(
        step, .grouping = grouping_set(where(is.character))
      ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
    }
    expect_identical(fixture$rows, integer())
    expect_identical(serialize(source, NULL), before)

    fixture$restore()
    expect_identical(inspect_grouping(
      step, .grouping = grouping_set(where(is.character))
    )$included, "(g)")
    expect_identical(inspect_grouping(
      step, .by = where(is.character)
    )$fixed, "(g)")
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      for (result in list(verb(step, .by = where(is.character)),
                          verb(step, .grouping = grouping_set(
                            where(is.character)
                          )))) {
        result <- dtplyr_metadata_nest_result(result)
        expect_identical(result$g, c("a", "b"))
        expect_identical(lapply(result$data, function(cell) cell$value),
                         list(1L, 2L))
      }
    }
    expect_identical(serialize(source, NULL), before)
  })
})

test_that("head refusal protects immutable and Mutable inspection sources", {
  skip_if_suggest_absent("dtplyr")
  fixture <- local_dtplyr_head(mutating = TRUE)
  for (immutable in c(TRUE, FALSE)) {
    source <- data.table::data.table(g = c("a", "b"), value = 1:2)
    before <- serialize(source, NULL)
    root <- dtplyr::lazy_dt(source, immutable = immutable)
    predicate_calls <- 0L
    predicate <- function(x) {
      predicate_calls <<- predicate_calls + 1L
      x > 0L
    }
    for (step in list(root, dplyr::mutate(root, derived = value + 1L),
                      dplyr::filter(root, predicate(value)))) {
      expect_identical(inspect_grouping(
        step, .grouping = grouping_set(g)
      )$included, "(g)")
      for (verb in list(inspect_grouping, nest_with_margins,
                        nest_by_with_margins)) {
        message <- if (!immutable && !identical(verb, inspect_grouping)) {
          "immutable = FALSE"
        } else {
          "Can't safely determine column metadata"
        }
        expect_error(verb(step, .by = where(is.character)),
                     message, fixed = TRUE, class = "marginplyr_error")
        expect_error(verb(
          step, .grouping = grouping_set(where(is.character))
        ), message, fixed = TRUE, class = "marginplyr_error")
      }
      expect_identical(fixture$rows, integer())
      expect_identical(predicate_calls, 0L)
      expect_identical(serialize(source, NULL), before)
    }
  }
})

test_that("head refusal protects every contributing graph root", {
  skip_if_suggest_absent("dtplyr")
  left_source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  right_source <- data.table::data.table(g = c("a", "b"), value = 3:4)
  before <- lapply(list(left_source, right_source), serialize,
                   connection = NULL)
  for (immutable in c(TRUE, FALSE)) {
    left <- dtplyr::lazy_dt(left_source)
    right <- dtplyr::lazy_dt(right_source, immutable = immutable)
    for (inputs in list(list(left, right), list(right, left))) {
      for (step in list(dplyr::left_join(inputs[[1L]], inputs[[2L]], by = "g"),
                        dplyr::union_all(inputs[[1L]], inputs[[2L]]))) local({
        native <- dplyr::collect(step)
        fixture <- local_dtplyr_head(mutating = TRUE)
        expect_identical(dplyr::collect(step), native)
        expect_identical(inspect_grouping(
          step, .grouping = grouping_set(g)
        )$included, "(g)")
        for (verb in list(inspect_grouping, nest_with_margins,
                          nest_by_with_margins)) {
          message <- if (!immutable && !identical(verb, inspect_grouping)) {
            "immutable = FALSE"
          } else {
            "Can't safely determine column metadata"
          }
          expect_error(verb(step, .by = where(is.character)),
                       message, fixed = TRUE, class = "marginplyr_error")
          expect_error(verb(
            step, .grouping = grouping_set(where(is.character))
          ), message, fixed = TRUE, class = "marginplyr_error")
        }
        expect_identical(fixture$rows, integer())
        expect_identical(lapply(list(left_source, right_source), serialize,
                                connection = NULL), before)
        fixture$restore()
        expect_identical(inspect_grouping(
          step, .by = where(is.character)
        )$fixed, "(g)")
        if (immutable) {
          for (verb in list(nest_with_margins, nest_by_with_margins)) {
            result <- dtplyr_metadata_nest_result(verb(
              step, .by = where(is.character)
            ))
            expect_identical(result$g, c("a", "b"))
            for (i in seq_along(result$data)) {
              expect_identical(result$data[[i]], tibble::as_tibble(
                native[native$g == result$g[[i]], setdiff(names(native), "g")]
              ))
            }
          }
        }
        expect_identical(lapply(list(left_source, right_source), serialize,
                                connection = NULL), before)
      })
    }
  }
})

test_that("typed metadata leaves uncertain head registrations unread", {
  skip_if_suggest_absent("dtplyr")
  for (binding in c("absent", "active", "lazy")) local({
    source <- data.table::data.table(g = c("a", "b"), value = 1:2)
    before <- serialize(source, NULL)
    root <- dtplyr::lazy_dt(source)
    registry <- get(".__S3MethodsTable__.", envir = asNamespace("utils"))
    original <- utils::getS3method("head", "data.table")
    on.exit({
      if (exists("head.data.table", envir = registry, inherits = FALSE)) {
        rm("head.data.table", envir = registry)
      }
      registerS3method("head", "data.table", original,
                       envir = asNamespace("utils"))
    }, add = TRUE)
    lookups <- 0L
    read_method <- function() {
      lookups <<- lookups + 1L
      data.table::set(source, j = "g", value = c("changed", "changed"))
      original
    }
    rm("head.data.table", envir = registry)
    if (binding == "active") {
      makeActiveBinding("head.data.table", read_method, registry)
    } else if (binding == "lazy") {
      delayedAssign("head.data.table", read_method(), assign.env = registry)
    }
    expect_identical(inspect_grouping(
      root, .grouping = grouping_set(g)
    )$included, "(g)")
    for (verb in list(inspect_grouping, nest_with_margins,
                      nest_by_with_margins)) {
      expect_error(verb(root, .by = where(is.character)),
                   "dplyr::collect()", fixed = TRUE,
                   class = "marginplyr_error")
    }
    expect_identical(lookups, 0L)
    expect_identical(serialize(source, NULL), before)
  })
})

test_that("standard deferred head registration preserves typed metadata", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- serialize(source, NULL)
  registry <- get(".__S3MethodsTable__.", envir = asNamespace("utils"))
  original <- utils::getS3method("head", "data.table")
  ns <- asNamespace("data.table")
  methods <- getNamespaceInfo(ns, "S3methods")
  withr::defer({
    registerS3method("head", "data.table", original,
                     envir = asNamespace("utils"))
    setNamespaceInfo(ns, "S3methods", methods)
  })
  registerS3method("head", "data.table", "head.data.table", envir = ns)
  expect_true(rlang::env_binding_are_lazy(registry, "head.data.table")[[1L]])
  root <- dtplyr::lazy_dt(source)
  expect_identical(inspect_grouping(
    root, .grouping = grouping_set(where(is.character))
  )$included, "(g)")
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    result <- dtplyr_metadata_nest_result(verb(root, .by = where(is.character)))
    expect_identical(result$g, c("a", "b"))
    expect_identical(lapply(result$data, function(cell) cell$value),
                     list(1L, 2L))
  }
  expect_identical(serialize(source, NULL), before)
})

test_that("head fixture restores registration when its scope fails", {
  skip_if_suggest_absent("dtplyr")
  original <- utils::getS3method("head", "data.table")
  expect_error(local({
    local_dtplyr_head()
    stop("fixture failure")
  }), "fixture failure", fixed = TRUE)
  expect_identical(utils::getS3method("head", "data.table"), original)
})
