test_that("Nest key collisions precede unsafe dtplyr metadata", {
  skip_if_suggest_absent("dtplyr")
  seen <- integer()
  probe <- function(x) {
    seen <<- c(seen, length(x))
    x
  }
  source <- data.table::data.table(g = c("a", "b"), value = 1:2)
  before <- data.table::copy(source)
  step <- dtplyr::lazy_dt(source) |>
    dplyr::mutate(value = probe(value))

  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    for (role in c("fixed", "dimension", "grouped")) {
      input <- if (role == "grouped") dplyr::group_by(step, g) else step
      by <- if (role == "fixed") rlang::quo(g) else rlang::quo(NULL)
      error <- expect_error(
        verb(
          input,
          .by = !!by,
          .grouping = if (role == "dimension") rollup(g),
          .key = "g"
        ),
        "must not be a grouping column", class = "marginplyr_error"
      )
      expect_match(conditionMessage(error), "`g`", fixed = TRUE)
      expect_identical(rlang::call_name(conditionCall(error)), "verb")
    }
  }
  expect_identical(seen, integer())
  expect_identical(source, before)
})

test_that("row-wise outer .rows names precede unsafe dtplyr metadata", {
  skip_if_suggest_absent("dtplyr")
  probe <- function(x) stop("payload evaluated")
  step <- dtplyr::lazy_dt(data.frame(.rows = c("a", "b"), value = 1:2)) |>
    dplyr::mutate(value = probe(value))
  calls <- list(
    quote(nest_by_with_margins(step, .by = .rows)),
    quote(nest_by_with_margins(step, .grouping = rollup(.rows))),
    quote(nest_by_with_margins(
      step, .grouping = rollup(.rows, where(is.integer))
    )),
    quote(nest_by_with_margins(dplyr::group_by(step, .rows))),
    quote(nest_by_with_margins(
      dplyr::rename(step, g = .rows), .id = ".rows"
    )),
    quote(nest_by_with_margins(
      dplyr::rename(step, g = .rows),
      .grouping = rollup(where(is.character)), .id = ".rows"
    ))
  )
  for (call in calls) {
    error <- expect_error(
      eval(call), "row-wise identifier", class = "marginplyr_error"
    )
    expect_identical(conditionCall(error), call)
    expect_match(conditionMessage(error), "nest_with_margins", fixed = TRUE)
  }
})

test_that("known Nest names do not wait for unrelated typed selections", {
  skip_if_suggest_absent("dtplyr")
  probe <- function(x) stop("payload evaluated")
  step <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), value = 1:2)) |>
    dplyr::mutate(value = probe(value))
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    expect_error(
      verb(step, .by = g, .grouping = rollup(where(is.integer)), .key = "g"),
      "must not be a grouping column", class = "marginplyr_error"
    )
    expect_error(
      verb(step, .by = where(is.integer), .grouping = rollup(g), .key = "g"),
      "must not be a grouping column", class = "marginplyr_error"
    )
  }
})

test_that("leading Nest dimensions settle collisions in mixed selections", {
  skip_if_suggest_absent("dtplyr")
  probe <- function(x) stop("payload evaluated")
  predicate <- function(x) stop("typed selection evaluated")
  step <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), value = 1:2)) |>
    dplyr::mutate(value = probe(value))
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    expect_error(
      verb(step, .grouping = rollup(g, where(predicate)), .key = "g"),
      "must not be a grouping column", class = "marginplyr_error"
    )
  }
})

test_that("valid Nest names still require safe typed metadata", {
  skip_if_suggest_absent("dtplyr")
  probe <- function(x) stop("payload evaluated")
  step <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), value = 1:2)) |>
    dplyr::mutate(value = probe(value))
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    expect_error(
      verb(step, .grouping = rollup(g), .key = "payload"),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
    expect_error(
      verb(step, .grouping = rollup(where(is.character)), .key = "g"),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
    expect_error(
      verb(step, .grouping = rollup(g, where(is.integer)), .key = "payload"),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
  }
})

test_that("typed Nest selections retain their output-name diagnostics", {
  skip_if_suggest_absent("dtplyr")
  source <- dtplyr::lazy_dt(data.frame(g = c("a", "b"), value = 1:2))
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    expect_error(
      verb(source, .grouping = rollup(where(is.character)), .key = "g"),
      "must not be a grouping column", class = "marginplyr_error"
    )
    expect_error(
      verb(source, .by = where(is.character), .key = "g"),
      "must not be a grouping column", class = "marginplyr_error"
    )
  }
  rows <- dtplyr::lazy_dt(data.frame(.rows = c("a", "b"), value = 1:2))
  expect_error(
    nest_by_with_margins(rows, .grouping = rollup(where(is.character))),
    "row-wise identifier", class = "marginplyr_error"
  )
})
