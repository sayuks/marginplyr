# Each source runs separately so no test requires two optional backends.
check_forwarded_grouping <- function(input, data) {
  constructors <- c(
    "grouping_set", "grouping_sets", "rollup", "cube", "grouping_spec"
  )
  # Compare the private registry so a constructor added without a matrix case
  # fails this coverage assertion.
  expect_setequal(constructors, grouping_constructor_names())
  templates <- list(
    quote(function(column) {
      a <- "a"
      CONSTRUCTOR(base::`(`({{ column }}))
    }),
    quote(function(column) {
      a <- "a"
      CONSTRUCTOR(base::`(`(!!rlang::enquo(column)))
    }),
    quote(function(...) {
      CONSTRUCTOR(base::`(`(..1))
    }),
    quote(function(...) {
      CONSTRUCTOR(base::`(`((..1)))
    })
  )
  consumers <- list(
    function(s) inspect_grouping(input, .grouping = s, .format = "list"),
    function(s) {
      summarize_with_margins(
        input, n = dplyr::n(), .grouping = s, .margin_label = NULL
      )
    },
    function(s) expand_with_margins(input, .grouping = s, .margin_label = NULL),
    function(s) nest_with_margins(input, .grouping = s, .margin_label = NULL),
    function(s) nest_by_with_margins(input, .grouping = s, .margin_label = NULL)
  )
  if (!is.data.frame(input) && !inherits(input, "dtplyr_step")) {
    consumers <- consumers[1:3]
  }
  materialize <- function(x) {
    if (is.data.frame(x)) return(x)
    dplyr::arrange(dplyr::collect(x), dplyr::across(dplyr::everything()))
  }
  for (template in templates) {
    native <- eval(do.call(substitute, list(
      template, list(CONSTRUCTOR = quote(rlang::quo))
    )))
    for (a in list("b", 2L)) {
      for (selection in list(quote(a), quote(c(a)))) {
        ordinary <- eval(rlang::call2(native, selection))
        selected <- names(tidyselect::eval_select(ordinary, data))
        expect_identical(selected, "b")
        for (constructor in constructors) {
          forward <- eval(do.call(substitute, list(
            template, list(CONSTRUCTOR = as.name(constructor))
          )))
          actual <- eval(rlang::call2(forward, selection))
          expected <- eval(rlang::call2(constructor, selected))
          for (consume in consumers) {
            expect_identical(
              materialize(consume(actual)), materialize(consume(expected))
            )
          }
        }
      }
    }
  }
}

test_that("forwarded Grouping selections agree with typed local selection", {
  data <- data.frame(a = c("east", "west"), b = 1:2)
  check_forwarded_grouping(data, data)
})

test_that("forwarded Grouping selections agree with typed dtplyr selection", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = c("east", "west"), b = 1:2)
  check_forwarded_grouping(dtplyr::lazy_dt(data), data)
})

test_that("forwarded Grouping selections agree with typed SQLite selection", {
  skip_if_suggest_absent("RSQLite", "DBI")
  data <- data.frame(a = c("east", "west"), b = 1:2)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  input <- dplyr::copy_to(con, data, "forwarded_grouping_boundaries")
  check_forwarded_grouping(input, data)
})

test_that("forwarded Grouping selections agree with typed DuckDB selection", {
  skip_if_suggest_absent("duckdb", "DBI")
  data <- data.frame(a = c("east", "west"), b = 1:2)
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  input <- dplyr::copy_to(con, data, "forwarded_grouping_boundaries")
  check_forwarded_grouping(input, data)
})

# jarl-ignore unnecessary_parentheses: Selection-walk parentheses are a control.
test_that("qualified forwarding preserves distinct syntactic selection", {
  data <- data.frame(a = c("east", "west"), b = 1:2)
  qualified <- function(column) {
    grouping_sets((base::`(`(base::`(`(base::`(`({{ column }})))))
  }
  syntactic <- function(column) grouping_sets((({{ column }})))
  identity_wrapper <- function(column) grouping_sets(identity({{ column }}))
  a <- "b"
  expect_identical(
    inspect_grouping(data, .grouping = qualified(a), .format = "list")$included,
    list("b")
  )
  expect_identical(
    inspect_grouping(data, .grouping = syntactic(a), .format = "list")$included,
    list(names(tidyselect::eval_select(quote(((a))), data)))
  )
  expect_identical(
    inspect_grouping(data, .grouping = syntactic(a), .format = "list")$included,
    list("a")
  )
  expect_identical(
    inspect_grouping(data, .grouping = identity_wrapper(a)),
    inspect_grouping(data, .grouping = qualified(a))
  )
})

test_that("forwarded dimensions permit distinct unions and fixed keys", {
  data <- data.frame(a = c("east", "west"), b = 1:2)
  forward <- function(column) grouping_set(base::`(`({{ column }}))
  union <- function(column) {
    grouping_sets(base::`(`({{ column }}), grouping_set(a))
  }
  a <- "b"
  expect_identical(
    inspect_grouping(data, .grouping = union(a), .format = "list")$included,
    list("b", "a")
  )
  expect_identical(
    inspect_grouping(data, .by = a, .grouping = forward(a)),
    inspect_grouping(data, .by = a, .grouping = grouping_set(b))
  )
  expect_identical(
    expand_with_margins(
      data, .by = a, .grouping = forward(a), .margin_label = NULL
    ), data
  )
})

test_that("qualified nested specifications retain their captured scope", {
  data <- data.frame(a = c("east", "west"), b = 1:2)
  forward <- function(column) grouping_sets(base::`(`({{ column }}))
  relay <- function(column) forward(base::`(`({{ column }}))
  bound <- grouping_set(b)
  spec <- relay(bound)
  original <- rlang::duplicate(spec)
  expected <- grouping_sets(grouping_set(b))
  for (consume in list(
    function(s) inspect_grouping(data, .grouping = s),
    function(s) summarize_with_margins(data, n = dplyr::n(), .grouping = s),
    function(s) expand_with_margins(data, .grouping = s),
    function(s) nest_with_margins(data, .grouping = s),
    function(s) nest_by_with_margins(data, .grouping = s)
  )) {
    expect_identical(consume(spec), consume(expected))
  }
  expect_identical(spec, original)
})

test_that("qualified forwarded caller selections run once in a mixed plan", {
  data <- data.frame(a = c("east", "west"), b = 1:2)
  calls <- 0L
  choose <- function() {
    calls <<- calls + 1L
    if (calls > 1L) stop("second selection evaluation")
    "b"
  }
  forward <- function(column) {
    grouping_sets(base::`(`({{ column }}), where(is.logical))
  }
  expect_identical(
    inspect_grouping(
      data, .grouping = forward(choose()), .format = "list"
    )$included,
    list("b", character())
  )
  expect_identical(calls, 1L)
})

test_that("forwarded typed predicates retain dtplyr metadata safety", {
  skip_if_suggest_absent("dtplyr")
  forward <- function(column) grouping_sets(base::`(`({{ column }}))
  for (dispatch in c("head", "[")) {
    local({
      fixture <- local_dtplyr_root_subclass(dispatch, mutating = TRUE)
      root <- dtplyr::lazy_dt(fixture$source)
      # The qualified call evaluates c lexically, yielding a typed predicate.
      c <- function(...) is.character
      selection <- rlang::quo(base::`(`(c("value")))
      selected <- names(tidyselect::eval_select(selection, fixture$source))
      expect_identical(selected, "g")
      for (verb in list(inspect_grouping, nest_with_margins,
                        nest_by_with_margins)) {
        expect_error(
          verb(root, .grouping = forward(c("value"))),
          "Can't safely determine column metadata", fixed = TRUE,
          class = "marginplyr_error"
        )
      }
      expect_identical(fixture$calls, integer())
      expect_identical(serialize(fixture$source, NULL), fixture$before)
    })
  }
})

test_that("forwarded missing selections precede DuckDB metadata reads", {
  skip_if_suggest_absent("duckdb", "DBI")
  data <- data.frame(a = c("east", "west"), b = 1:2)
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  input <- dplyr::copy_to(con, data, "forwarded_grouping_error")
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE, after = FALSE)
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS n")$n, 1L)
  expect_identical(counter$queries, 1L)
  counter$queries <- 0L
  forward <- function(column) {
    grouping_sets(base::`(`({{ column }}), where(is.logical))
  }
  original <- expect_error(
    tidyselect::eval_select(quote(base::`(`("absent")), data), "absent"
  )
  actual <- expect_error(inspect_grouping(
    input, .grouping = forward("absent")
  ), "absent")
  expect_identical(class(actual), class(original))
  expect_identical(conditionMessage(actual), conditionMessage(original))
  expect_identical(counter$queries, 0L)
})

test_that("forwarded Grouping selections agree with typed Arrow selection", {
  skip_if_suggest_absent("arrow")
  data <- data.frame(a = c("east", "west"), b = 1:2)
  check_forwarded_grouping(arrow::Table$create(data), data)
})

test_that("qualified forwarding retains each preflight selection value", {
  data <- data.frame(a = c("east", "west"), b = 1:2)
  forward <- function(column) grouping_sets(base::`(`({{ column }}))
  calls <- 0L
  caller <- rlang::env(rlang::current_env())
  makeActiveBinding("pick", function() {
    calls <<- calls + 1L
    if (calls > 1L) stop("second binding evaluation")
    "b"
  }, caller)
  caller$grouping_set <- function(...) {
    calls <<- calls + 1L
    if (calls > 1L) stop("second constructor evaluation")
    "b"
  }
  for (expr in list(quote(pick), quote(grouping_set()))) {
    calls <- 0L
    spec <- eval(rlang::call2(forward, expr), caller)
    expect_identical(
      inspect_grouping(data, .grouping = spec, .format = "list")$included,
      list("b")
    )
    expect_identical(calls, 1L)
  }
  caller$grouping_set <- function(...) {
    calls <<- calls + 1L
    marginplyr::grouping_set(...)
  }
  calls <- 0L
  spec <- eval(quote(forward(grouping_set(b))), caller)
  expect_identical(
    inspect_grouping(data, .grouping = spec, .format = "list")$included,
    list("b")
  )
  expect_identical(calls, 1L)
})
