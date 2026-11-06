rebound_pattern <- function(target = quote(paste0),
                            .local_envir = parent.frame()) {
  state <- new.env(parent = emptyenv())
  state$seen <- character()
  bindings <- rlang::env(paste0 = "nonfunction shadow")
  source <- basename(tempfile("marginplyr_rebound_pattern_"))
  makeActiveBinding(source, function() {
    kind <- typeof(tidyselect::peek_data()$a)
    state$seen <- c(state$seen, kind)
    if (kind == "character") "^a$" else "^b$"
  }, globalenv())
  withr::defer(rm(list = source, envir = globalenv()), envir = .local_envir)
  assignment <- rlang::call2("<-", target, rlang::expr(
    as.vector(as.vector(!!source, "symbol"), "function")
  ))
  bindings$pattern <- rlang::new_function(pairlist(), rlang::expr({
    !!assignment
    paste0()
  }), bindings)
  state$selection <- rlang::new_quosure(quote(matches(pattern())), bindings)
  state$inline <- rlang::new_quosure(rlang::expr(tidyselect::matches({
    paste0 <- as.vector(as.vector(!!source, "symbol"), "function")
    paste0()
  })), bindings)
  state
}

test_that("locally rebound Grouping callees use typed selection metadata", {
  data <- tibble::tibble(a = c("east", "west"), b = c(1L, 1L))
  fixture <- rebound_pattern()
  selection <- fixture$selection
  expect_identical(names(tidyselect::eval_select(selection, data)), "a")
  fixture$seen <- character()

  actual <- summarize_with_margins(
    data, n = dplyr::n(), .by = b, .grouping = grouping_set(!!selection)
  )
  expected <- summarize_with_margins(
    data, n = dplyr::n(), .by = b, .grouping = grouping_set(a)
  )
  expect_identical(actual, expected)
  expect_identical(fixture$seen, "character")
})

expect_rebound_grouping <- function(data) {
  original <- serialize(data, NULL)
  a <- rlang::sym("a")
  b <- rlang::sym("b")
  constructors <- c("grouping_set", "grouping_sets", "rollup", "cube",
                    "grouping_spec")
  for (constructor in constructors) {
    construct <- getExportedValue("marginplyr", constructor)
    for (position in c("dimension", "fixed", "unfixed")) {
      for (consumer in c("inspect", "summary")) {
        fixture <- rebound_pattern()
        selection <- fixture$selection
        direct <- names(tidyselect::eval_select(
          selection, tibble::tibble(a = c("east", "west"), b = c(1L, 1L))
        ))
        expect_identical(direct, "a")
        fixture$seen <- character()
        spec <- if (position == "fixed") {
          construct(!!b)
        } else {
          construct(!!selection)
        }
        literal <- if (position == "fixed") construct(!!b) else construct(!!a)
        by <- if (position == "fixed") selection else if (
          position == "dimension"
        ) rlang::quo(!!b) else NULL
        literal_by <- if (position == "fixed") rlang::quo(!!a) else by
        if (consumer == "inspect") {
          actual <- inspect_grouping(
            data, .by = !!by, .grouping = spec, .format = "list"
          )
          expected <- inspect_grouping(
            data, .by = !!literal_by, .grouping = literal, .format = "list"
          )
        } else {
          actual <- summarize_with_margins(
            data, n = dplyr::n(), .by = !!by, .grouping = spec,
            .margin_label = NULL, .id = "set"
          )
          expected <- summarize_with_margins(
            data, n = dplyr::n(), .by = !!literal_by, .grouping = literal,
            .margin_label = NULL, .id = "set"
          )
          actual <- tibble::as_tibble(actual)
          expected <- tibble::as_tibble(expected)
        }
        expect_identical(actual, expected, info = paste(
          constructor, position, consumer
        ))
        expect_identical(fixture$seen, "character")
        expect_identical(rlang::quo_get_env(selection)$paste0,
                         "nonfunction shadow")
      }
    }
  }
  expect_identical(serialize(data, NULL), original)
}

test_that("local rebound selections agree across Grouping consumers", {
  data <- tibble::tibble(a = c("east", "west"), b = c(1L, 1L))
  expect_rebound_grouping(data)
  expect_rebound_grouping(as.data.frame(data))
})

test_that("data.table rebound selections agree across Grouping consumers", {
  skip_if_suggest_absent("data.table")
  data <- data.table::data.table(a = c("east", "west"), b = c(1L, 1L))
  expect_rebound_grouping(data)
})

test_that("immutable dtplyr rebound selections agree across consumers", {
  skip_if_suggest_absent("dtplyr")
  data <- tibble::tibble(a = c("east", "west"), b = c(1L, 1L))
  expect_rebound_grouping(dtplyr::lazy_dt(data, immutable = TRUE))
})

test_that("proven callbacks retain name-only dtplyr inspection budgets", {
  skip_if_suggest_absent("dtplyr")
  data <- tibble::tibble(a = c("east", "west"), b = c(1L, 1L))
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "x"))
  calls <- 0L
  pattern <- function() {
    calls <<- calls + 1L
    paste0("^", tidyselect::peek_vars()[[1L]], "$")
  }
  actual <- inspect_grouping(
    step, .grouping = grouping_set(matches(pattern())), .format = "list"
  )
  expect_identical(actual$included, list("a"))
  expect_identical(calls, 2L)
})

test_that("unproven rebound callbacks do not evaluate dtplyr source rows", {
  skip_if_suggest_absent("dtplyr")
  data <- tibble::tibble(a = c("east", "west"), b = c(1L, 1L))
  rows <- 0L
  touch <- function(x) {
    rows <<- rows + length(x)
    x
  }
  step <- dplyr::mutate(dtplyr::lazy_dt(data), a = touch(a))
  fixture <- rebound_pattern()
  selection <- fixture$selection
  expect_error(
    inspect_grouping(step, .grouping = grouping_set(!!selection)),
    "Can't safely determine column metadata", class = "marginplyr_error"
  )
  expect_identical(rows, 0L)
  expect_identical(fixture$seen, character())
})

test_that("computed heads and unused formals keep ordinary evaluation", {
  data <- tibble::tibble(a = "east", b = 1L)
  pattern <- function(unused) {
    (function(x) "^a$")(unused)
  }
  actual <- inspect_grouping(
    data, .by = b, .grouping = grouping_set(matches(pattern())),
    .format = "list"
  )
  expect_identical(actual$included, list("a"))
})

test_that("inline helper blocks retain rebound callee evaluation", {
  data <- tibble::tibble(a = "east", b = 1L)
  fixture <- rebound_pattern()
  selection <- fixture$inline
  expect_identical(names(tidyselect::eval_select(selection, data)), "a")
  fixture$seen <- character()
  actual <- inspect_grouping(
    data, .by = b, .grouping = grouping_set(!!selection), .format = "list"
  )
  expect_identical(actual$included, list("a"))
  expect_identical(fixture$seen, "character")
})

test_that("string assignment targets cannot retain an older callee proof", {
  data <- tibble::tibble(a = "east", b = 1L)
  fixture <- rebound_pattern("paste0")
  selection <- fixture$selection
  expect_identical(names(tidyselect::eval_select(selection, data)), "a")
  fixture$seen <- character()
  actual <- inspect_grouping(
    data, .by = b, .grouping = grouping_set(!!selection), .format = "list"
  )
  expect_identical(actual$included, list("a"))
  expect_identical(fixture$seen, "character")
})
