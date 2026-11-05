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
  normalize <- function(x) {
    x <- dplyr::ungroup(dplyr::collect(x))
    x$data <- lapply(x$data, tibble::as_tibble)
    x
  }
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
      fixed <- normalize(verb(root, .by = where(is.character), .sort = "last"))
      expect_identical(
        fixed, normalize(verb(explicit, .by = g, .sort = "last"))
      )
      expect_identical(fixed$g, c("a", "b"))
      expect_identical(
        lapply(fixed$data, function(cell) cell$value), list(1L, 2L)
      )
      expect_identical(calls, integer())
      expect_identical(source, before)

      margins <- normalize(verb(
        root, .grouping = rollup(where(is.character)), .sort = "last"
      ))
      expect_identical(margins, normalize(verb(
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
