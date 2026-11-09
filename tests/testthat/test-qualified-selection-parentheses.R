test_that("qualified parentheses inside helpers defer mixed selections", {
  data <- data.frame(region = c("east", "west"), amount = 1:2)
  resolutions <- 0L
  env <- rlang::env(`::` = function(package, name) {
    resolutions <<- resolutions + 1L
    function(x) {
      proxy <- tidyselect::peek_data()
      if (is.character(proxy$region)) "region" else "amount"
    }
  })
  selection <- rlang::new_quosure(
    quote(all_of(base::`(`("amount"))), env
  )
  selected <- names(tidyselect::eval_select(selection, data))
  expect_identical(selected, "region")

  resolutions <- 0L
  expect_identical(
    inspect_grouping(data, .by = !!selection, .format = "list")$fixed,
    list(selected)
  )
  expect_identical(resolutions, 1L)

  mixed <- grouping_sets(
    grouping_set(!!selection), grouping_set(amount),
    grouping_set(where(is.logical))
  )
  expected <- grouping_sets(
    grouping_set(all_of(selected)), grouping_set(amount),
    grouping_set(where(is.logical))
  )
  verbs <- list(inspect_grouping, nest_with_margins, nest_by_with_margins)
  for (verb in verbs) {
    resolutions <- 0L
    expect_identical(
      verb(data, .grouping = mixed), verb(data, .grouping = expected)
    )
    expect_identical(resolutions, 1L)
  }
})

test_that("caller-qualified parentheses use typed dtplyr inspection", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(region = c("east", "west"), amount = 1:2)
  step <- dtplyr::lazy_dt(data)
  resolutions <- 0L
  env <- rlang::env(`:::` = function(package, name) {
    resolutions <<- resolutions + 1L
    function(x) {
      proxy <- tidyselect::peek_data()
      if (is.character(proxy$region)) "region" else "amount"
    }
  })
  head <- rlang::call2(":::", quote(base), quote(`(`))
  expressions <- rlang::exprs(
    all_of(!!rlang::call2(head, "amount")),
    all_of((!!rlang::call2(head, quote(("amount")))))
  )
  for (expr in expressions) {
    selection <- rlang::new_quosure(expr, env)
    selected <- names(tidyselect::eval_select(selection, data))
    expect_identical(selected, "region")

    resolutions <- 0L
    expect_identical(
      inspect_grouping(
        step, .grouping = grouping_set(!!selection), .format = "list"
      )$included,
      list(selected)
    )
    expect_identical(resolutions, 1L)
  }
})

test_that("ordinary qualified parentheses keep DuckDB leading validation", {
  skip_if_suggest_absent("duckdb", "DBI")
  data <- data.frame(region = c("east", "west"), amount = 1:2)
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  remote <- dplyr::copy_to(con, data, "qualified_selection_parentheses")
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE, after = FALSE)
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS n")$n, 1L)
  expect_identical(counter$queries, 1L)
  counter$queries <- 0L

  calls <- 0L
  env <- rlang::env(`::` = function(package, name) {
    calls <<- calls + 1L
    function(x) {
      proxy <- tidyselect::peek_data()
      if (is.character(proxy$region)) "region" else "amount"
    }
  })
  typed <- rlang::new_quosure(quote(all_of(base::`(`("amount"))), env)
  empty <- rlang::call2("(", rlang::missing_arg())
  ordinary <- list(
    quote(all_of(base::`(`("absent"))),
    quote(base::`(`(all_of("absent"))),
    rlang::call2("all_of", empty)
  )
  for (selector in ordinary) {
    original <- expect_error(tidyselect::eval_select(selector, data))
    mixed <- grouping_sets(grouping_set(!!selector), grouping_set(!!typed))
    leading <- expect_error(inspect_grouping(remote, .grouping = mixed))
    expect_identical(class(leading), class(original))
    expect_identical(conditionMessage(leading), conditionMessage(original))
  }
  expect_identical(calls, 0L)
  expect_identical(counter$queries, 0L)

  selected <- names(tidyselect::eval_select(typed, data))
  expect_identical(selected, "region")
  calls <- 0L
  valid <- grouping_sets(grouping_set(!!typed), grouping_set(amount))
  expect_identical(
    inspect_grouping(remote, .grouping = valid, .format = "list")$included,
    list(selected, "amount")
  )
  expect_identical(calls, 1L)
  expect_identical(counter$queries, 2L)
})

test_that("qualified parentheses use lexical c and typed fixed keys", {
  data <- data.frame(region = c("east", "west"), amount = 1:2)
  seen <- character()
  predicate <- function(x) {
    seen <<- base::c(seen, typeof(x))
    is.character(x)
  }
  c <- function(...) predicate
  selector <- rlang::quo(base::`(`(c("region")))
  selected <- names(tidyselect::eval_select(selector, data))
  expect_identical(selected, "region")
  expect_identical(seen, base::c("character", "integer"))

  seen <- character()
  expect_identical(
    inspect_grouping(data, .by = !!selector, .format = "list")$fixed,
    list(selected)
  )
  expect_identical(seen, base::c("character", "integer"))
})

test_that("qualified grouping selectors resolve before duplicate checks", {
  data <- data.frame(region = c("east", "west"), amount = 1:2)
  predicate <- is.character
  c <- function(...) predicate
  selector <- rlang::quo(base::`(`(c("region")))
  grouping <- grouping_sets(!!selector, grouping_set())
  expect_identical(
    inspect_grouping(data, .grouping = grouping, .format = "list")$included,
    list("region", character())
  )
  expect_identical(
    expand_with_margins(data, .grouping = grouping),
    expand_with_margins(
      data, .grouping = grouping_sets(grouping_set(region), grouping_set())
    )
  )
  expect_error(
    inspect_grouping(
      data, .grouping = grouping_sets(!!selector, grouping_set(region))
    ),
    "Duplicate grouping sets"
  )
})

test_that("qualified selections request dtplyr types only when needed", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(region = c("east", "west"), amount = 1:2)
  input <- dtplyr::lazy_dt(data)
  derived <- dplyr::mutate(input, label = paste0(region, "!"))
  expect_identical(
    inspect_grouping(
      derived, .by = base::`(`(c("label")), .format = "list"
    )$fixed,
    list("label")
  )
  expect_identical(
    inspect_grouping(
      derived, .grouping = grouping_set(base::`(`(all_of("label"))),
      .format = "list"
    )$included,
    list("label")
  )
  expect_identical(
    inspect_grouping(
      derived, .grouping = grouping_sets(base::`(`(rollup(label))),
      .format = "list"
    )$included,
    list("label", character())
  )

  predicate <- is.character
  c <- function(...) predicate
  selector <- rlang::quo((base::`(`(c("region"))))
  expect_identical(
    inspect_grouping(
      input, .grouping = grouping_set(!!selector), .format = "list"
    )$included,
    list("region")
  )
})

test_that("qualified parentheses retain forwarded lexical selections", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = c("east", "west"), b = 1:2)
  derived <- dplyr::mutate(dtplyr::lazy_dt(data), label = paste0(a, "!"))
  make_selection <- function(col, pick = "b") {
    rlang::quo(c(base::`(`({{ col }}), b))
  }
  pick <- "a"
  selections <- list(
    make_selection("a"), make_selection(.env$pick), make_selection(all_of("a"))
  )
  for (selection in selections) {
    selected <- names(tidyselect::eval_select(selection, data))
    expect_identical(selected, c("a", "b"))
    expect_identical(
      inspect_grouping(derived, .by = !!selection, .format = "list")$fixed,
      list(selected)
    )
    expect_identical(
      inspect_grouping(
        derived, .grouping = grouping_set(!!selection), .format = "list"
      )$included,
      list(selected)
    )
  }
})

test_that("forwarded ordinary caller code keeps mixed-selection evaluation", {
  data <- data.frame(a = c("east", "west"), b = 1:2)
  calls <- 0L
  choose <- function() {
    calls <<- calls + 1L
    "a"
  }
  make_selection <- function(col) rlang::quo(c(base::`(`({{ col }}), b))
  selection <- make_selection(choose())
  selected <- names(tidyselect::eval_select(selection, data))
  expect_identical(selected, c("a", "b"))
  expect_identical(calls, 1L)

  calls <- 0L
  grouping <- grouping_sets(grouping_set(!!selection), where(is.logical))
  expect_identical(
    inspect_grouping(data, .grouping = grouping, .format = "list")$included,
    list(selected, character())
  )
  expect_identical(calls, 1L)
})

test_that("forwarded ordinary predicates retain typed dtplyr refusal", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = c("east", "west"), b = 1:2)
  derived <- dplyr::mutate(dtplyr::lazy_dt(data), label = paste0(a, "!"))
  make_selection <- function(col) rlang::quo(c(base::`(`({{ col }}), b))
  through_closure <- function(col) {
    rlang::quo(c(base::`(`((function(pick) {{ col }})("b")), b))
  }
  pick <- is.character
  selections <- list(
    make_selection(is.character), make_selection(where(is.character)),
    through_closure(pick)
  )
  for (selection in selections) {
    selected <- names(tidyselect::eval_select(selection, data))
    expect_identical(selected, c("a", "b"))
    expect_error(
      inspect_grouping(derived, .by = !!selection),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
    expect_error(
      inspect_grouping(derived, .grouping = grouping_set(!!selection)),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
  }
})

test_that("missing forwarded ordinary names fail before DuckDB sends", {
  skip_if_suggest_absent("duckdb", "DBI")
  data <- data.frame(a = c("east", "west"), b = 1:2)
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  remote <- dplyr::copy_to(con, data, "forwarded_qualified_parentheses")
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE, after = FALSE)
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS n")$n, 1L)
  expect_identical(counter$queries, 1L)
  counter$queries <- 0L

  make_selection <- function(col, pick = "b") {
    rlang::quo(c(base::`(`({{ col }}), b))
  }
  pick <- "absent"
  for (selection in list(make_selection("absent"), make_selection(.env$pick))) {
    original <- expect_error(tidyselect::eval_select(selection, data), "absent")
    fixed <- expect_error(inspect_grouping(remote, .by = !!selection), "absent")
    dimensions <- expect_error(inspect_grouping(
      remote, .grouping = grouping_set(!!selection)
    ), "absent")
    for (actual in list(fixed, dimensions)) {
      expect_identical(class(actual), class(original))
      expect_identical(conditionMessage(actual), conditionMessage(original))
    }
    expect_identical(counter$queries, 0L)
  }
})
