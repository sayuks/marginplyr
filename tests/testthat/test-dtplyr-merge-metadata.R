# A full join whose caller merge changes the key type only on empty inputs.
# The caller has made dtplyr available; calls record the backend boundary.
full_join_merge_fixture <- function(binding = "merge") {
  fixture <- new.env(parent = emptyenv())
  fixture$source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  fixture$before <- data.table::copy(fixture$source)
  fixture$calls <- integer()
  binding_env <- new.env(parent = environment())
  binding_env[[binding]] <- function(x, y, ...) {
    fixture$calls <- c(fixture$calls, nrow(x), nrow(y))
    method <- get("merge.data.table", envir = asNamespace("data.table"))
    result <- method(x, y, ...)
    if (nrow(result) == 0L) {
      data.table::set(result, j = "g", value = integer())
    }
    result
  }
  caller <- new.env(parent = binding_env)
  caller[[binding]] <- NULL
  root <- eval(quote(dtplyr::lazy_dt(fixture$source)), caller)
  fixture$root <- root
  fixture$step <- dplyr::full_join(root, root, by = "g")
  fixture
}

test_that("typed full-join plans refuse a caller merge before invoking it", {
  skip_if_suggest_absent("dtplyr")
  # dtplyr imports data.table; only dtplyr is an optional backend here.
  fixture <- full_join_merge_fixture()
  explicit <- dplyr::collect(fixture$step)
  expect_identical(explicit, tibble::tibble(
    g = c("a", "b"), value.x = 1:2, value.y = 1:2
  ))
  expect_identical(fixture$calls, c(2L, 2L))
  fixture$calls <- integer()
  expect_identical(
    inspect_grouping(fixture$step, .grouping = grouping_set(g)),
    inspect_grouping(explicit, .grouping = grouping_set(g))
  )
  expect_error(inspect_grouping(
    fixture$step, .grouping = grouping_set(where(is.character))
  ), "Can't safely determine column metadata", fixed = TRUE,
  class = "marginplyr_error")
  expect_identical(fixture$calls, integer())
  expect_identical(fixture$source, fixture$before)
})

test_that("typed full joins refuse a caller merge method before dispatch", {
  skip_if_suggest_absent("dtplyr")
  fixture <- full_join_merge_fixture("merge.data.table")
  explicit <- dplyr::collect(fixture$step)
  expect_identical(explicit$g, c("a", "b"))
  expect_identical(fixture$calls, c(2L, 2L))
  fixture$calls <- integer()
  expect_error(inspect_grouping(
    fixture$step, .grouping = grouping_set(where(is.character))
  ), "Can't safely determine column metadata", fixed = TRUE,
  class = "marginplyr_error")
  expect_identical(fixture$calls, integer())
  expect_identical(fixture$source, fixture$before)
})

test_that("fixed keys and both Nest verbs refuse caller full-join dispatch", {
  skip_if_suggest_absent("dtplyr")
  for (binding in c("merge", "merge.data.table")) {
    fixture <- full_join_merge_fixture(binding)
    expect_error(inspect_grouping(fixture$step, .by = where(is.character)),
                 "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      expect_error(verb(fixture$step, .by = where(is.character)),
                   "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
      expect_error(verb(
        fixture$step, .grouping = grouping_set(where(is.character))
      ), "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
    }
    expect_identical(fixture$calls, integer())
    expect_identical(fixture$source, fixture$before)
  }
})

test_that("full-join metadata leaves active and lazy merge bindings unread", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  for (binding in c("merge", "merge.data.table")) {
    for (inherited in c(FALSE, TRUE)) {
      for (lazy in c(FALSE, TRUE)) {
        lookups <- 0L
        binding_env <- new.env(parent = environment())
        if (lazy) {
          delayedAssign(binding, {
            lookups <<- lookups + 1L
            base::merge
          }, assign.env = binding_env)
        } else {
          makeActiveBinding(binding, function() {
            lookups <<- lookups + 1L
            base::merge
          }, binding_env)
        }
        caller <- if (inherited) new.env(parent = binding_env) else binding_env
        if (inherited) {
          caller[[binding]] <- NULL
        }
        root <- eval(quote(dtplyr::lazy_dt(source)), caller)
        step <- dplyr::full_join(root, root, by = "g")
        expect_identical(inspect_grouping(
          step, .grouping = grouping_set(g)
        )$included, "(g)")
        for (verb in list(inspect_grouping, nest_with_margins,
                          nest_by_with_margins)) {
          expect_error(verb(step, .by = where(is.character)),
                       class = "marginplyr_error")
          expect_error(verb(
            step, .grouping = grouping_set(where(is.character))
          ), class = "marginplyr_error")
        }
        expect_identical(lookups, 0L)
        expect_identical(source, before)
      }
    }
  }
})

test_that("ordinary full-join dispatch preserves typed plans and nested rows", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  for (binding in c("unmasked", "merge", "merge.data.table", "nonfunctions")) {
    caller <- new.env(parent = environment())
    if (binding == "merge") {
      caller$merge <- base::merge
    } else if (binding == "merge.data.table") {
      caller$merge.data.table <- get(
        "merge.data.table", envir = asNamespace("data.table")
      )
    } else if (binding == "nonfunctions") {
      caller$merge <- NULL
      caller$merge.data.table <- 42L
    }
    root <- eval(quote(dtplyr::lazy_dt(source)), caller)
    step <- dplyr::full_join(root, root, by = "g")
    explicit <- dplyr::collect(step)
    expect_identical(explicit, tibble::tibble(
      g = c("a", "b"), value.x = 1:2, value.y = 1:2
    ))
    expect_identical(
      inspect_grouping(step, .by = where(is.character)),
      inspect_grouping(explicit, .by = g)
    )
    expect_identical(
      inspect_grouping(step, .grouping = grouping_set(where(is.character))),
      inspect_grouping(explicit, .grouping = grouping_set(g))
    )
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      expect_identical(
        dtplyr_metadata_nest_result(verb(step, .by = where(is.character))),
        dtplyr_metadata_nest_result(verb(explicit, .by = g))
      )
      expect_identical(
        dtplyr_metadata_nest_result(verb(
          step, .grouping = grouping_set(where(is.character))
        )),
        dtplyr_metadata_nest_result(verb(explicit, .grouping = grouping_set(g)))
      )
    }
    expect_identical(source, before)
  }
})

test_that("other joins do not acquire full-join dispatch restrictions", {
  skip_if_suggest_absent("dtplyr")
  fixture <- full_join_merge_fixture()
  for (join in list(dplyr::left_join, dplyr::right_join, dplyr::inner_join,
                    dplyr::semi_join, dplyr::anti_join)) {
    step <- join(fixture$root, fixture$root, by = "g")
    explicit <- dplyr::collect(step)
    expect_identical(
      inspect_grouping(step, .by = where(is.character)),
      inspect_grouping(explicit, .by = g)
    )
    expect_identical(
      inspect_grouping(step, .grouping = grouping_set(where(is.character))),
      inspect_grouping(explicit, .grouping = grouping_set(g))
    )
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      expect_identical(
        dtplyr_metadata_nest_result(verb(step, .by = where(is.character))),
        dtplyr_metadata_nest_result(verb(explicit, .by = g))
      )
      expect_identical(
        dtplyr_metadata_nest_result(verb(
          step, .grouping = grouping_set(where(is.character))
        )),
        dtplyr_metadata_nest_result(verb(explicit, .grouping = grouping_set(g)))
      )
    }
  }
  expect_identical(fixture$calls, integer())
  expect_identical(fixture$source, fixture$before)
})

test_that("full-join typed inspection does not process upstream source rows", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  reads <- integer()
  keep <- function(x) {
    reads <<- c(reads, length(x))
    x > 0L
  }
  root <- dtplyr::lazy_dt(source)
  filtered <- dplyr::filter(root, keep(value))
  step <- dplyr::full_join(filtered, root, by = "g")
  explicit <- dplyr::collect(step)
  expect_identical(explicit$g, c("a", "b"))
  expect_identical(reads, 2L)
  reads <- integer()
  expect_identical(
    inspect_grouping(step, .by = where(is.character)),
    inspect_grouping(explicit, .by = g)
  )
  expect_identical(
    inspect_grouping(step, .grouping = grouping_set(where(is.character))),
    inspect_grouping(explicit, .grouping = grouping_set(g))
  )
  expect_identical(reads, integer())
  expect_identical(source, before)
})

test_that("full joins check dispatch in the effective outer environment", {
  skip_if_suggest_absent("dtplyr")
  fixture <- full_join_merge_fixture()
  ordinary <- dtplyr::lazy_dt(fixture$source)
  safe <- dplyr::left_join(fixture$step, ordinary, by = "g")
  explicit <- dplyr::collect(safe)
  expect_identical(explicit$g, c("a", "b"))
  expect_identical(
    inspect_grouping(safe, .grouping = grouping_set(where(is.character))),
    inspect_grouping(explicit, .grouping = grouping_set(g))
  )
  expect_identical(fixture$calls, integer())
  unsafe <- dplyr::full_join(fixture$root, ordinary, by = "g")
  expect_identical(dplyr::collect(unsafe)$g, c("a", "b"))
  expect_identical(fixture$calls, c(2L, 2L))
  fixture$calls <- integer()
  expect_error(inspect_grouping(unsafe, .by = where(is.character)),
               "dplyr::collect()", fixed = TRUE, class = "marginplyr_error")
  expect_identical(fixture$calls, integer())
  expect_identical(fixture$source, fixture$before)
})
