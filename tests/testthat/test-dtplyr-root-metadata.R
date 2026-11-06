test_that("immutable root typed plans ignore a caller head binding", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only dtplyr is an optional backend here.
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  calls <- integer()
  root <- local({
    head <- function(x, n) {
      calls <<- c(calls, nrow(x))
      data.table::data.table(g = integer(), value = integer())
    }
    dtplyr::lazy_dt(source, immutable = TRUE)
  })
  explicit <- dplyr::collect(root)
  expect_identical(explicit, tibble::tibble(g = c("a", "b"), value = 1:2))
  expect_identical(calls, integer())
  expect_identical(
    inspect_grouping(root, .grouping = rollup(g)),
    inspect_grouping(explicit, .grouping = rollup(g))
  )
  expect_identical(calls, integer())
  expect_identical(
    inspect_grouping(root, .grouping = rollup(where(is.character))),
    inspect_grouping(explicit, .grouping = rollup(where(is.character)))
  )
  expect_identical(
    inspect_grouping(root, .by = where(is.character)),
    inspect_grouping(explicit, .by = where(is.character))
  )
  expect_identical(calls, integer())
  expect_identical(source, before)
})

test_that("immutable root nesting preserves partitions despite a caller head", {
  skip_if_suggest_absent("dtplyr")
  for (mutating in c(FALSE, TRUE)) {
    source <- data.table::data.table(g = c("a", "b"), value = 1:2)
    before <- data.table::copy(source)
    calls <- integer()
    root <- local({
      head <- function(x, n) {
        calls <<- c(calls, nrow(x))
        if (mutating) {
          data.table::set(x, j = "g", value = rep("changed", nrow(x)))
          return(utils::head(x, n = n))
        }
        data.table::data.table(g = integer(), value = integer())
      }
      dtplyr::lazy_dt(source, immutable = TRUE)
    })
    explicit <- dplyr::collect(root)
    expect_identical(calls, integer())
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      fixed <- dtplyr_metadata_nest_result(verb(
        root, .by = where(is.character), .sort = "last"
      ))
      expect_identical(
        fixed, dtplyr_metadata_nest_result(verb(
          explicit, .by = g, .sort = "last"
        ))
      )
      expect_identical(fixed$g, c("a", "b"))
      expect_identical(
        lapply(fixed$data, function(cell) cell$value), list(1L, 2L)
      )
      expect_identical(calls, integer())
      expect_identical(source, before)

      margins <- dtplyr_metadata_nest_result(verb(
        root, .grouping = rollup(where(is.character)), .sort = "last"
      ))
      expect_identical(margins, dtplyr_metadata_nest_result(verb(
        explicit, .grouping = rollup(g), .sort = "last"
      )))
      expect_identical(margins$g, c("a", "b", "Total"))
      expect_identical(lapply(margins$data, function(cell) cell$value),
                       list(1L, 2L, 1:2))
      expect_identical(calls, integer())
      expect_identical(source, before)
    }
  }
})

test_that("immutable root inspection cannot mutate the source through head", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  calls <- integer()
  root <- local({
    head <- function(x, n) {
      calls <<- c(calls, nrow(x))
      data.table::set(x, j = "g", value = rep("changed", nrow(x)))
      utils::head(x, n = n)
    }
    dtplyr::lazy_dt(source, immutable = TRUE)
  })
  explicit <- dplyr::collect(root)
  expect_identical(
    inspect_grouping(root, .grouping = rollup(where(is.character))),
    inspect_grouping(explicit, .grouping = rollup(g))
  )
  expect_identical(
    inspect_grouping(root, .by = where(is.character)),
    inspect_grouping(explicit, .by = g)
  )
  expect_identical(calls, integer())
  expect_identical(source, before)
})
test_that("typed inspection refuses root subclass truncation before dispatch", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only dtplyr is an optional backend here.
  for (dispatch in c("head", "[")) {
    local({
      fixture <- local_dtplyr_root_subclass(dispatch)
      root <- dtplyr::lazy_dt(fixture$source)
      for (step in list(root, dplyr::mutate(root, derived = value + 1L),
                        dplyr::filter(root, value > 0L))) {
        expect_identical(dplyr::collect(step)$g, c("a", "b"))
        fixture$calls <- integer()
        expect_identical(inspect_grouping(
          step, .grouping = grouping_set(g)
        )$included, "(g)")
        expect_error(inspect_grouping(
          step, .grouping = grouping_set(where(is.character))
        ), "Can't safely determine column metadata", fixed = TRUE,
        class = "marginplyr_error")
        expect_error(inspect_grouping(step, .by = where(is.character)),
                     "dplyr::collect()", fixed = TRUE,
                     class = "marginplyr_error")
        expect_identical(fixture$calls, integer())
        expect_identical(serialize(fixture$source, NULL), fixture$before)
      }
    })
  }
})

test_that("both Nest verbs refuse false root subclass partitions", {
  skip_if_suggest_absent("dtplyr")
  for (dispatch in c("head", "[")) {
    local({
      fixture <- local_dtplyr_root_subclass(dispatch)
      root <- dtplyr::lazy_dt(fixture$source)
      for (step in list(root, dplyr::mutate(root, derived = value + 1L),
                        dplyr::filter(root, value > 0L))) {
        explicit <- dplyr::collect(step)
        expect_identical(dplyr::summarise(
          dplyr::group_by(step, g), n = dplyr::n()
        ) |> dplyr::collect(), tibble::tibble(g = c("a", "b"), n = c(1L, 1L)))
        fixture$calls <- integer()
        for (verb in list(nest_with_margins, nest_by_with_margins)) {
          expect_error(verb(step, .by = where(is.character)),
                       "Can't safely determine column metadata", fixed = TRUE,
                       class = "marginplyr_error")
          expect_error(verb(
            step, .grouping = grouping_set(where(is.character))
          ), "Can't safely determine column metadata", fixed = TRUE,
          class = "marginplyr_error")
          expect_identical(fixture$calls, integer())
          for (result in list(verb(explicit, .by = g),
                              verb(explicit, .grouping = grouping_set(g)))) {
            result <- dtplyr_metadata_nest_result(result)
            expect_identical(result$g, c("a", "b"))
            expect_identical(lapply(result$data, function(cell) cell$value),
                             list(1L, 2L))
          }
          expect_identical(serialize(fixture$source, NULL), fixture$before)
        }
      }
    })
  }
})

test_that("subclass metadata refusal protects immutable and Mutable roots", {
  skip_if_suggest_absent("dtplyr")
  for (dispatch in c("head", "[")) {
    local({
      fixture <- local_dtplyr_root_subclass(dispatch, mutating = TRUE)
      for (immutable in c(TRUE, FALSE)) {
        root <- dtplyr::lazy_dt(fixture$source, immutable = immutable)
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
          expect_error(inspect_grouping(step, .by = where(is.character)),
                       "Can't safely determine column metadata", fixed = TRUE,
                       class = "marginplyr_error")
          expect_error(inspect_grouping(
            step, .grouping = grouping_set(where(is.character))
          ), "Can't safely determine column metadata", fixed = TRUE,
          class = "marginplyr_error")
          for (verb in list(nest_with_margins, nest_by_with_margins)) {
            message <- if (immutable) {
              "Can't safely determine column metadata"
            } else {
              "immutable = FALSE"
            }
            expect_error(verb(step, .by = where(is.character)),
                         message, fixed = TRUE, class = "marginplyr_error")
            expect_error(verb(
              step, .grouping = grouping_set(where(is.character))
            ), message, fixed = TRUE, class = "marginplyr_error")
          }
          expect_identical(fixture$calls, integer())
          expect_identical(predicate_calls, 0L)
          expect_identical(serialize(fixture$source, NULL), fixture$before)
        }
      }
    })
  }
})

test_that("ordinary roots retain typed schema and both Nest partitions", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  for (immutable in c(TRUE, FALSE)) {
    root <- dtplyr::lazy_dt(source, immutable = immutable)
    for (step in list(root, dplyr::mutate(root, derived = value + 1L),
                      dplyr::filter(root, value > 0L))) {
      explicit <- dplyr::collect(step)
      before <- serialize(source, NULL)
      expect_identical(inspect_grouping(
        step, .grouping = grouping_set(where(is.character))
      )$included, "(g)")
      expect_identical(inspect_grouping(step, .by = where(is.character)),
                       inspect_grouping(explicit, .by = g))
      for (verb in list(nest_with_margins, nest_by_with_margins)) {
        if (immutable) {
          for (result in list(verb(step, .by = where(is.character)),
                              verb(step, .grouping = grouping_set(
                                where(is.character)
                              )))) {
            result <- dtplyr_metadata_nest_result(result)
            expect_identical(result$g, c("a", "b"))
            expect_identical(lapply(result$data, function(cell) cell$value),
                             list(1L, 2L))
          }
        } else {
          expect_error(verb(step, .by = where(is.character)),
                       "immutable = FALSE", fixed = TRUE,
                       class = "marginplyr_error")
        }
      }
      expect_identical(serialize(source, NULL), before)
    }
  }
})

test_that("typed metadata protects subclass roots on either graph input", {
  skip_if_suggest_absent("dtplyr")
  fixture <- local_dtplyr_root_subclass("head", mutating = TRUE)
  plain_source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- serialize(plain_source, NULL)
  plain <- dtplyr::lazy_dt(plain_source)
  for (immutable in c(TRUE, FALSE)) {
    subclass <- dtplyr::lazy_dt(fixture$source, immutable = immutable)
    for (inputs in list(list(plain, subclass), list(subclass, plain))) {
      for (step in list(
        dplyr::left_join(inputs[[1L]], inputs[[2L]], by = "g"),
        dplyr::union_all(inputs[[1L]], inputs[[2L]])
      )) {
        expect_error(inspect_grouping(step, .by = where(is.character)),
                     "Can't safely determine column metadata", fixed = TRUE,
                     class = "marginplyr_error")
        for (verb in list(nest_with_margins, nest_by_with_margins)) {
          message <- if (immutable) {
            "Can't safely determine column metadata"
          } else {
            "immutable = FALSE"
          }
          expect_error(verb(step, .by = where(is.character)),
                       message, fixed = TRUE, class = "marginplyr_error")
        }
        expect_identical(fixture$calls, integer())
        expect_identical(serialize(fixture$source, NULL), fixture$before)
        expect_identical(serialize(plain_source, NULL), before)
      }
    }
  }
})
