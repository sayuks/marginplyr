test_that("registered converters cannot change dimensions or fixed keys", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only dtplyr is an optional backend here.
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- serialize(source, NULL)
  root <- dtplyr::lazy_dt(source)
  predicate_calls <- 0L
  predicate <- function(x) {
    predicate_calls <<- predicate_calls + 1L
    x > 0L
  }
  steps <- list(root, dplyr::compute(root),
                dplyr::filter(root, predicate(value)),
                dplyr::mutate(root, derived = value + 1L))
  for (step in steps) local({
    ns <- asNamespace("data.table")
    ordinary <- utils::getS3method("as.data.table", "dtplyr_step", envir = ns)
    on.exit(registerS3method("as.data.table", "dtplyr_step", ordinary,
                             envir = ns), add = TRUE)
    calls <- 0L
    replacement <- function(x, ...) {
      calls <<- calls + 1L
      out <- ordinary(x, ...)
      if (nrow(out) == 0L) {
        data.table::set(out, j = "g", value = integer())
      }
      out
    }
    registerS3method("as.data.table", "dtplyr_step", replacement, envir = ns)
    native <- dplyr::collect(step)
    expect_identical(native$g, c("a", "b"))
    expect_identical(native$value, 1:2)
    expect_identical(calls, 0L)
    predicate_calls <<- 0L
    expect_identical(inspect_grouping(
      step, .grouping = grouping_set(g)
    )$included, "(g)")
    expect_identical(inspect_grouping(step, .by = g)$fixed, "(g)")
    for (verb in list(inspect_grouping, nest_with_margins,
                      nest_by_with_margins)) {
      expect_error(verb(step, .by = where(is.character)),
                   "dplyr::collect()", fixed = TRUE,
                   class = "marginplyr_error")
      expect_error(verb(
        step, .grouping = grouping_set(where(is.character))
      ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
    }
    expect_identical(calls, 0L)
    expect_identical(predicate_calls, 0L)
    expect_identical(serialize(source, NULL), before)

    registerS3method("as.data.table", "dtplyr_step", ordinary, envir = ns)
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

test_that("uncertain converter registrations stay unread before refusal", {
  skip_if_suggest_absent("dtplyr")
  for (binding in c("active", "lazy")) local({
    for (immutable in c(TRUE, FALSE)) local({
      source <- data.table::data.table(g = c("a", "b"), value = 1:2)
      before <- serialize(source, NULL)
      root <- dtplyr::lazy_dt(source, immutable = immutable)
      ns <- asNamespace("data.table")
      registry <- get(".__S3MethodsTable__.", envir = ns)
      name <- "as.data.table.dtplyr_step"
      ordinary <- utils::getS3method("as.data.table", "dtplyr_step", envir = ns)
      on.exit({
        rm(list = name, envir = registry)
        registerS3method("as.data.table", "dtplyr_step", ordinary, envir = ns)
      }, add = TRUE)
      lookups <- 0L
      read_method <- function() {
        lookups <<- lookups + 1L
        data.table::set(source, j = "g", value = c("changed", "changed"))
        ordinary
      }
      rm(list = name, envir = registry)
      if (binding == "active") {
        makeActiveBinding(name, read_method, registry)
      } else {
        delayedAssign(name, read_method(), assign.env = registry)
      }
      predicate_calls <- 0L
      predicate <- function(x) {
        predicate_calls <<- predicate_calls + 1L
        x > 0L
      }
      for (step in list(root, dplyr::filter(root, predicate(value)),
                        dplyr::mutate(root, derived = value + 1L))) {
        expect_identical(inspect_grouping(
          step, .grouping = grouping_set(g)
        )$included, "(g)")
        expect_identical(inspect_grouping(step, .by = g)$fixed, "(g)")
        for (verb in list(inspect_grouping, nest_with_margins,
                          nest_by_with_margins)) {
          message <- if (!immutable && !identical(verb, inspect_grouping)) {
            "immutable = FALSE"
          } else {
            "dplyr::collect()"
          }
          expect_error(verb(step, .by = where(is.character)),
                       message, fixed = TRUE, class = "marginplyr_error")
          expect_error(verb(
            step, .grouping = grouping_set(where(is.character))
          ), message, fixed = TRUE, class = "marginplyr_error")
        }
      }
      expect_identical(lookups, 0L)
      expect_identical(predicate_calls, 0L)
      expect_identical(serialize(source, NULL), before)
      if (binding == "lazy") {
        expect_true(rlang::env_binding_are_lazy(registry, name)[[1L]])
      }
    })
  })
})

test_that("absent and standard deferred converters preserve typed metadata", {
  skip_if_suggest_absent("dtplyr")
  for (registration in c("absent", "deferred")) local({
    source <- data.table::data.table(g = c("a", "b"), value = 1:2)
    before <- serialize(source, NULL)
    ns <- asNamespace("data.table")
    home <- asNamespace("dtplyr")
    registry <- get(".__S3MethodsTable__.", envir = ns)
    name <- "as.data.table.dtplyr_step"
    ordinary <- utils::getS3method("as.data.table", "dtplyr_step", envir = ns)
    methods <- getNamespaceInfo(home, "S3methods")
    on.exit({
      registerS3method("as.data.table", "dtplyr_step", ordinary, envir = ns)
      setNamespaceInfo(home, "S3methods", methods)
    }, add = TRUE)
    if (registration == "absent") {
      rm(list = name, envir = registry)
    } else {
      registerS3method("as.data.table", "dtplyr_step", name, envir = home)
      expect_true(rlang::env_binding_are_lazy(registry, name)[[1L]])
    }
    root <- dtplyr::lazy_dt(source)
    for (step in list(root, dplyr::mutate(root, derived = value + 1L))) {
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
    }
    expect_identical(serialize(source, NULL), before)
  })
})
