test_that("direct context lookups need only the derived column names", {
  skip_if_suggest_absent("dtplyr")
  data <- tibble::tibble(a = c("x", "y"), b = 1:2)
  evaluations <- 0L
  derive <- function(x) {
    evaluations <<- evaluations + 1L
    paste0(x, "!")
  }
  step <- dtplyr::lazy_dt(data) |>
    dplyr::mutate(added = derive(a))
  typed <- dplyr::mutate(data, added = paste0(a, "!"))
  data_alias <- tidyselect::peek_data
  names_alias <- base::names
  names <- "nonfunction shadow"
  idx <- c(3L, 1L)
  selectors <- rlang::exprs(
    tidyselect::peek_vars(),
    names(tidyselect::peek_data()),
    tidyselect::peek_vars()[c(3L, 1L)],
    tidyselect::peek_vars()[.env$idx],
    tidyselect::peek_vars()[[.env[["idx"]][[1L]]]],
    base::`[`(x = tidyselect::peek_vars(), i = c(3L, 1L)),
    names_alias(data_alias())[[2L]]
  )
  constructors <- list(grouping_set, grouping_sets, rollup, cube, grouping_spec)

  for (selector in selectors) {
    selected <- names(tidyselect::eval_select(selector, typed))
    wrapped <- rlang::expr(tidyselect::all_of(!!selector))
    for (selection in list(selector, wrapped)) {
      expect_identical(
        inspect_grouping(step, .by = !!selection, .format = "list")$fixed,
        list(selected)
      )
      for (constructor in constructors) {
        expect_identical(
          inspect_grouping(step, .grouping = constructor(!!selection)),
          inspect_grouping(
            typed, .grouping = constructor(tidyselect::all_of(selected))
          )
        )
      }
    }
  }
  expect_identical(evaluations, 0L)
})

test_that("NULL and direct lexical selectors need only the derived names", {
  skip_if_suggest_absent("dtplyr")
  data <- tibble::tibble(a = c("x", "y"), b = 1:2)
  evaluations <- 0L
  derive <- function(x) {
    evaluations <<- evaluations + 1L
    paste0(x, "!")
  }
  step <- dtplyr::lazy_dt(data) |>
    dplyr::mutate(added = derive(a))
  pick <- "a"
  empty <- NULL
  selectors <- rlang::exprs(
    c(a, NULL), c(a, all_of(NULL)), .env$pick, .env[["pick"]],
    c(a, .env$empty), c(a, all_of(.env[["empty"]]))
  )

  for (selector in selectors) {
    expect_identical(
      inspect_grouping(step, .by = !!selector, .format = "list")$fixed,
      list("a")
    )
    expect_identical(
      inspect_grouping(
        step, .grouping = grouping_set(!!selector), .format = "list"
      )$included,
      list("a")
    )
  }
  for (selector in rlang::exprs(NULL, all_of(NULL), .env$empty)) {
    expect_identical(
      inspect_grouping(step, .by = !!selector, .format = "list")$fixed,
      list(character())
    )
    expect_identical(
      inspect_grouping(
        step, .grouping = grouping_set(!!selector), .format = "list"
      )$included,
      list(character())
    )
  }
  # jarl-ignore missing_argument: The empty constructor argument is under test.
  expect_error(
    inspect_grouping(step, .grouping = grouping_set(, a)),
    "Argument 1", class = "marginplyr_error"
  )
  expect_identical(evaluations, 0L)
})

test_that("missing selection-context names fail before a DuckDB query", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  data <- tibble::tibble(a = "x", b = 1L)
  remote <- dplyr::copy_to(con, data, "selection_context_names")
  # Trace the DBI method to observe query sends before the condition.
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE, after = FALSE)
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS n")$n, 1L)
  expect_identical(counter$queries, 1L)

  names <- "nonfunction shadow"
  idx <- 99L
  pick <- "absent"

  selectors <- rlang::exprs(
    tidyselect::peek_vars()[99L],
    tidyselect::all_of(tidyselect::peek_vars()[99L]),
    tidyselect::peek_vars()[.env$idx],
    tidyselect::all_of(tidyselect::peek_vars()[.env[["idx"]]]),
    names(tidyselect::peek_data())[99L],
    tidyselect::peek_vars()[.env$absent_idx],
    tidyselect::all_of(tidyselect::peek_vars()[.env[["absent_idx"]]]),
    c(NULL, a, absent),
    c(all_of(NULL), a, absent),
    .env$pick,
    .env[["pick"]],
    .env$absent_idx
  )
  for (selector in selectors) {
    counter$queries <- 0L
    expected <- expect_error(tidyselect::eval_select(selector, data))
    fixed <- expect_error(inspect_grouping(remote, .by = !!selector))
    expect_identical(counter$queries, 0L)
    dimension <- expect_error(
      inspect_grouping(remote, .grouping = grouping_set(!!selector))
    )
    for (error in list(fixed, dimension)) {
      expect_identical(class(error), class(expected))
      expect_identical(conditionMessage(error), conditionMessage(expected))
    }
    expect_identical(counter$queries, 0L)
  }
})

test_that("typed and opaque selections retain the dtplyr metadata boundary", {
  skip_if_suggest_absent("dtplyr")
  data <- tibble::tibble(a = c("x", "y"), b = 1:2)
  calls <- new.env()
  calls$upstream <- 0L
  derive <- function(x) {
    calls$upstream <- calls$upstream + 1L
    paste0(x, "!")
  }
  step <- dtplyr::lazy_dt(data) |>
    dplyr::mutate(added = derive(a))

  calls$selector <- 0L
  read_index <- function() {
    calls$selector <- calls$selector + 1L
    1L
  }
  makeActiveBinding("active_idx", read_index, rlang::current_env())
  delayedAssign("delayed_idx", read_index())
  classed_idx <- factor(1L)
  predicate <- is.character
  pick <- "a"
  key <- "pick"
  names <- function(x) {
    calls$selector <- calls$selector + 1L
    x
  }
  where <- tidyselect::peek_vars
  columns <- c("a", "b", "added")
  selectors <- rlang::exprs(
    tidyselect::where(is.character),
    where(),
    base::names(tidyselect::peek_data())[
      vapply(tidyselect::peek_data(), is.character, logical(1))
    ],
    columns[if (is.numeric(tidyselect::peek_data()$b)) 2L else 1L],
    tidyselect::peek_vars()[read_index()],
    tidyselect::peek_vars()[.env$active_idx],
    tidyselect::all_of(tidyselect::peek_vars()[.env$active_idx]),
    tidyselect::peek_vars()[.env[["delayed_idx"]]],
    tidyselect::all_of(tidyselect::peek_vars()[.env[["delayed_idx"]]]),
    tidyselect::peek_vars()[.env$classed_idx],
    .env$active_idx,
    .env[["delayed_idx"]],
    .env$classed_idx,
    .env[[key]],
    .env$predicate,
    names(tidyselect::peek_data())
  )
  check <- function(selector) {
    expect_error(
      inspect_grouping(step, .by = !!selector),
      "Can't safely determine column metadata",
      class = "marginplyr_error"
    )
    expect_error(
      inspect_grouping(step, .grouping = grouping_set(!!selector)),
      "Can't safely determine column metadata",
      class = "marginplyr_error"
    )
  }
  for (selector in selectors) {
    check(selector)
  }
  check(local({
    names <- "nonfunction shadow"
    rlang::quo((names)(tidyselect::peek_data()))
  }))
  check(local({
    `[[` <- function(x, i) {
      calls$selector <- calls$selector + 1L
      pick
    }
    rlang::quo(.env[["pick"]])
  }))
  # `everything` is supplied by tidyselect's mask, not the lexical chain.
  check(rlang::new_quosure(quote(all_of(everything)), baseenv()))
  check_dots <- function(...) {
    check(rlang::quo(tidyselect::peek_vars()[..1]))
    check(rlang::quo(tidyselect::peek_vars()[.env$..1]))
    check(rlang::quo(
      tidyselect::all_of(tidyselect::peek_vars()[.env[["..1"]]])
    ))
  }
  check_missing <- function(idx) {
    check(rlang::quo(tidyselect::peek_vars()[.env$idx]))
    check(rlang::quo(.env$idx))
  }
  check_dots(read_index())
  check_missing()
  expect_identical(calls$selector, 0L)
  expect_identical(calls$upstream, 0L)
})

test_that("opaque namespace promises stay unforced without typed metadata", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = "x", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "!"))

  # Give a loaded namespace a deferred export to observe when it is forced.
  ns <- asNamespace("testthat")
  name <- "auto_test_package"
  original <- testthat::auto_test_package
  unlockBinding(name, ns)
  on.exit({
    unlockBinding(name, ns)
    assign(name, original, envir = ns)
    lockBinding(name, ns)
  })
  reads <- 0L
  delayedAssign(name, {
    reads <- reads + 1L
    function() 1L
  }, assign.env = ns)
  lockBinding(name, ns)

  selectors <- rlang::exprs(
    testthat::auto_test_package(),
    tidyselect::peek_vars()[testthat::auto_test_package()],
    tidyselect::all_of(tidyselect::peek_vars()[testthat::auto_test_package()])
  )
  for (selector in selectors) {
    expect_error(
      inspect_grouping(step, .by = !!selector),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
    expect_error(
      inspect_grouping(step, .grouping = grouping_set(!!selector)),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
    expect_identical(reads, 0L)
  }
  expect_identical(
    inspect_grouping(data, .by = testthat::auto_test_package())$fixed,
    "(a)"
  )
  expect_identical(reads, 1L)
})

test_that("embraced names select fixed keys and Grouping dimensions", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = "x", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "!"))

  fixed <- function(col, pick = "b") {
    inspect_grouping(step, .by = c({{ col }}, b), .format = "list")$fixed
  }
  dimensions <- function(col, pick = "b") {
    inspect_grouping(
      step, .grouping = grouping_set(c({{ col }}, b)), .format = "list"
    )$included
  }

  expect_identical(
    fixed(a), inspect_grouping(step, .by = c(a, b), .format = "list")$fixed
  )
  expect_identical(
    dimensions(a),
    inspect_grouping(
      step, .grouping = grouping_set(c(a, b)), .format = "list"
    )$included
  )
  pick <- "a"
  expect_identical(fixed(.env$pick), list(c("a", "b")))
  expect_identical(dimensions(.env$pick), list(c("a", "b")))
})

test_that("embraced names settle DuckDB inspection without metadata", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  data <- data.frame(a = "x", b = 1L)
  remote <- dplyr::copy_to(con, data, "embraced_grouping_names")
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE, after = FALSE)
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS n")$n, 1L)
  expect_identical(counter$queries, 1L)
  counter$queries <- 0L

  fixed <- function(col) {
    inspect_grouping(remote, .by = c({{ col }}, b), .format = "list")$fixed
  }
  dimensions <- function(col) {
    inspect_grouping(
      remote, .grouping = grouping_set(c({{ col }}, b)), .format = "list"
    )$included
  }
  pick <- "a"
  for (inspect in list(fixed, dimensions)) {
    expect_identical(
      inspect(base::`(`(base::c(.env[["pick"]], NULL))), list(c("a", "b"))
    )
    expect_identical(counter$queries, 0L)
    expect_error(
      inspect(absent), "`absent` doesn't exist", fixed = TRUE,
      class = "vctrs_error_subscript_oob"
    )
    expect_identical(counter$queries, 0L)
  }
})

test_that("embraced predicates still require typed metadata", {
  skip_if_suggest_absent("dtplyr")
  data <- data.frame(a = "x", b = 1L)
  step <- dplyr::mutate(dtplyr::lazy_dt(data), added = paste0(a, "!"))
  fixed <- function(input, col) {
    inspect_grouping(input, .by = c({{ col }}, b), .format = "list")$fixed
  }
  dimensions <- function(input, col) {
    inspect_grouping(
      input, .grouping = grouping_set(c({{ col }}, b)), .format = "list"
    )$included
  }
  for (inspect in list(fixed, dimensions)) {
    expect_identical(
      inspect(data, tidyselect::where(is.character)), list(c("a", "b"))
    )
    expect_error(
      inspect(step, tidyselect::where(is.character)),
      "Can't safely determine column metadata", class = "marginplyr_error"
    )
  }
})

# dtplyr imports data.table. The typed metadata receiver has its methods
# even though the input to the public Margin verbs is a lazy step.
local_dtplyr_context_names <- function() {
  rlang::local_bindings(
    names.data.table = function(x) {
      columns <- attr(x, "names")
      active <- tryCatch(tidyselect::peek_data(), error = function(cnd) NULL)
      if (identical(x, active) && is.character(x[["a"]])) {
        return(rev(columns))
      }
      columns
    },
    .env = asNamespace("base")$.__S3MethodsTable__.,
    .frame = parent.frame()
  )
}

test_that("dtplyr context names select the typed fixed key", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(a = c("x", "y"), b = c(1L, 1L))
  original <- data.table::copy(source)
  reads <- 0L
  observe <- function(x) {
    reads <<- reads + 1L
    x != ""
  }
  step <- dtplyr::lazy_dt(source, immutable = TRUE) |>
    dplyr::filter(observe(a))
  typed <- data.table::data.table(a = character(), b = integer())
  local_dtplyr_context_names()

  direct <- rlang::quo(names(tidyselect::peek_data())[1L])
  wrapped <- rlang::quo(identity(!!direct))
  for (selection in list(direct, wrapped)) {
    before <- reads
    expect_identical(tidyselect::eval_select(selection, typed), c(b = 2L))
    expect_identical(
      inspect_grouping(step, .by = !!selection, .format = "list")$fixed,
      list("b")
    )
    result <- summarize_with_margins(
      step, count = dplyr::n(), .by = !!selection
    )
    expect_s3_class(result, "dtplyr_step")
    expect_identical(reads, before)
    expect_identical(
      dplyr::collect(result),
      tibble::tibble(b = 1L, count = 2L)
    )
    expect_gt(reads, before)
  }
  expect_identical(source, original)
})

test_that("dtplyr still resolves proven names before typed metadata", {
  skip_if_suggest_absent("dtplyr")
  step <- dtplyr::lazy_dt(data.frame(a = "x", b = 1L), immutable = TRUE) |>
    dplyr::mutate(added = paste0(a, "!"))
  local_dtplyr_context_names()

  expect_identical(
    inspect_grouping(
      step, .by = tidyselect::peek_vars()[[1L]], .format = "list"
    )$fixed,
    list("a")
  )
  expect_error(
    inspect_grouping(step, .by = absent),
    "`absent` doesn't exist", fixed = TRUE, class = "vctrs_error_subscript_oob"
  )
  expect_error(
    inspect_grouping(step, .grouping = grouping_sets(
      absent, names(tidyselect::peek_data())[[1L]]
    )),
    "`absent` doesn't exist", fixed = TRUE, class = "vctrs_error_subscript_oob"
  )
})

test_that("dtplyr context names select dimensions without false overlap", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(a = c("x", "y"), b = c(1L, 1L))
  step <- dtplyr::lazy_dt(source, immutable = TRUE)
  typed <- data.table::data.table(a = character(), b = integer())
  local_dtplyr_context_names()

  direct <- rlang::quo(names(tidyselect::peek_data())[1L])
  wrapped <- rlang::quo(identity(!!direct))
  for (selection in list(direct, wrapped)) {
    expect_identical(tidyselect::eval_select(selection, typed), c(b = 2L))
    for (constructor in list(grouping_set, grouping_sets, rollup, cube,
                             grouping_spec)) {
      expect_identical(
        inspect_grouping(
          step, .by = a, .grouping = constructor(!!selection), .format = "list"
        ),
        inspect_grouping(
          typed, .by = a, .grouping = constructor(b), .format = "list"
        )
      )
      expect_error(
        inspect_grouping(step, .by = b, .grouping = constructor(!!selection)),
        "cannot appear in both", class = "marginplyr_error"
      )
    }
  }
})

test_that("dtplyr captured context names use the typed receiver", {
  skip_if_suggest_absent("dtplyr")
  step <- dtplyr::lazy_dt(data.frame(a = "x", b = 1L), immutable = TRUE)
  local_dtplyr_context_names()

  expression <- quote(base::`(`(marginplyr::grouping_set(
    !!names(tidyselect::peek_data())[1L]
  )))
  selection <- rlang::new_quosure(expression, new.env(parent = baseenv()))
  specification <- eval(rlang::call2("grouping_sets", selection))
  expect_identical(
    inspect_grouping(
      step, .by = a, .grouping = specification, .format = "list"
    )$included,
    list("b")
  )
})

local_tibble_context_names <- function() {
  rlang::local_bindings(
    names.tbl_df = function(x) {
      columns <- attr(x, "names", exact = TRUE)
      active <- tryCatch(tidyselect::peek_data(), error = function(cnd) NULL)
      if (identical(columns, c("a", "b")) && identical(x, active)) {
        return(rev(columns))
      }
      columns
    },
    .env = asNamespace("base")$.__S3MethodsTable__.,
    .frame = parent.frame()
  )
}

expect_tibble_context_names <- function(input, typed) {
  local_tibble_context_names()
  a <- quote(a)
  b <- quote(b)
  direct <- rlang::quo(tidyselect::all_of(names(tidyselect::peek_data())[1L]))
  wrapped <- rlang::quo(tidyselect::all_of(
    identity(names(tidyselect::peek_data())[1L])
  ))
  fixed <- inspect_grouping(typed, .by = !!b, .format = "list")
  dimension <- inspect_grouping(
    typed, .by = !!a, .grouping = grouping_set(!!b), .format = "list"
  )
  for (selection in list(wrapped, direct)) {
    expect_identical(tidyselect::eval_select(selection, typed), c(b = 2L))
    expect_identical(
      inspect_grouping(input, .by = !!selection, .format = "list"), fixed
    )
    expect_identical(
      inspect_grouping(
        input, .by = !!a, .grouping = grouping_set(!!selection),
        .format = "list"
      ),
      dimension
    )
    expect_error(
      inspect_grouping(input, .by = !!b, .grouping = grouping_set(!!selection)),
      "cannot appear in both", class = "marginplyr_error"
    )
  }
}

test_that("DuckDB context names respect tibble methods", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- tibble::tibble(a = 1L, b = "x")
  remote <- dplyr::copy_to(con, data, "tibble_context_names")
  rlang::local_options(marginplyr.audit_sql = TRUE)

  expect_identical(
    inspect_grouping(
      remote, .by = tidyselect::all_of(names(tidyselect::peek_data())[1L]),
      .format = "list"
    )$fixed,
    list("a")
  )
  expect_equal(nrow(last_sent_queries(details = TRUE)), 0L)
  expect_tibble_context_names(remote, data[0L, ])
  audit <- last_sent_queries(details = TRUE)
  expect_identical(audit$purpose, "selection_proxy")
  expect_identical(audit$status, "returned")
  expect_match(audit$sql, "LIMIT 0", fixed = TRUE)
})

test_that("Arrow context names respect tibble methods", {
  skip_if_suggest_absent("arrow")
  data <- tibble::tibble(a = 1L, b = "x")
  expect_tibble_context_names(arrow::arrow_table(data), data[0L, ])
})

test_that("SQLite context names respect tibble methods", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- tibble::tibble(a = 1L, b = "x")
  remote <- dplyr::copy_to(con, data, "tibble_context_names")
  expect_tibble_context_names(remote, data[0L, ])
})

test_that("Grouping selections keep tidyselect's repaired column names", {
  data <- data.frame(
    a1 = c("east", "west"), a2 = 1:2, z = as.Date("2026-11-07") + 0:1
  )
  selection <- rlang::quo(c(a = c(a1, a2)))
  typed <- rlang::quo(identity(c("a1", "a2")))
  expect_identical(
    tidyselect::eval_select(selection, data), c(a1 = 1L, a2 = 2L)
  )
  constructors <- list(grouping_set, grouping_sets, rollup, cube, grouping_spec)
  expand <- function(...) expand_with_margins(..., .id = "set_id")

  for (verb in list(inspect_grouping, expand)) {
    fixed <- verb(data, .by = c(a1, a2))
    expect_identical(verb(data, .by = !!typed), fixed)
    expect_identical(verb(data, .by = !!selection), fixed)

    for (constructor in constructors) {
      literal <- verb(data, .grouping = constructor(c(a1, a2)))
      expect_identical(verb(data, .grouping = constructor(!!typed)), literal)
      expect_identical(
        verb(data, .grouping = constructor(!!selection)), literal
      )

      mixed <- verb(data, .grouping = constructor(c(a1, a2), z))
      date <- rlang::quo(where(function(x) inherits(x, "Date")))
      expect_identical(
        verb(data, .grouping = constructor(!!typed, !!date)), mixed
      )
      expect_identical(
        verb(data, .grouping = constructor(!!selection, !!date)), mixed
      )
    }
  }
})

test_that("DuckDB resolves repaired Grouping names before typed metadata", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  data <- data.frame(a1 = c("east", "west"), a2 = 1:2, value = 3:4)
  remote <- dplyr::copy_to(con, data, "repaired_grouping_names")
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE, after = FALSE)

  selection <- rlang::quo(c(a = a1:a2))
  literal <- grouping_sets(grouping_set(a1, a2), grouping_set())
  repaired <- grouping_sets(grouping_set(!!selection), grouping_set())
  expect_identical(
    inspect_grouping(remote, .by = !!selection),
    inspect_grouping(data, .by = c(a1, a2))
  )
  expect_identical(
    inspect_grouping(remote, .grouping = repaired),
    inspect_grouping(data, .grouping = literal)
  )
  expect_error(
    inspect_grouping(remote, .by = c(renamed = a1:a2)),
    "renamed1 = a1", fixed = TRUE, class = "marginplyr_error"
  )
  expect_error(
    inspect_grouping(remote, .grouping = grouping_set(c(renamed = a1:a2))),
    "renamed1 = a1", fixed = TRUE, class = "marginplyr_error"
  )
  expect_identical(counter$queries, 0L)

  expected <- expand_with_margins(data, .grouping = literal, .id = "set_id")
  result <- expand_with_margins(remote, .grouping = repaired, .id = "set_id")
  expect_s3_class(result, "tbl_lazy")
  expect_identical(counter$queries, 1L)
  expect_identical(
    dplyr::arrange(dplyr::collect(result), set_id, value),
    tibble::as_tibble(dplyr::arrange(expected, set_id, value))
  )
})

test_that("DuckDB name-only inspection skips data-frame receiver methods", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  data <- data.frame(a1 = c("east", "west"), a2 = c(10L, 20L))
  remote <- dplyr::copy_to(con, data, "grouping_proxy_dispatch")
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE, after = FALSE)

  reject_synthetic <- function(x) {
    if (identical(attr(x, "names", exact = TRUE), c("a1", "a2")) &&
          identical(unclass(x)[[1L]], 1L) &&
          identical(unclass(x)[[2L]], 2L)) {
      stop("This method requires actual column prototypes.")
    }
  }
  rlang::local_bindings(
    tidyselect_data_proxy.data.frame = function(x) {
      reject_synthetic(x)
      x
    },
    tidyselect_data_has_predicates.data.frame = function(x) {
      reject_synthetic(x)
      TRUE
    },
    .env = asNamespace("tidyselect")$.__S3MethodsTable__.
  )
  rlang::local_bindings(
    names.data.frame = function(x) {
      reject_synthetic(x)
      attr(x, "names", exact = TRUE)
    },
    .env = asNamespace("base")$.__S3MethodsTable__.
  )
  rlang::local_bindings(
    vec_proxy.data.frame = function(x, ...) {
      reject_synthetic(x)
      x
    },
    .env = asNamespace("vctrs")$.__S3MethodsTable__.
  )

  expect_identical(
    tidyselect::eval_select(quote(c(a = a1:a2)), data[0, ]),
    c(a1 = 1L, a2 = 2L)
  )
  expect_identical(
    inspect_grouping(remote, .grouping = grouping_set(a1, a2)),
    inspect_grouping(data, .grouping = grouping_set(a1, a2))
  )
  expect_identical(
    inspect_grouping(remote, .by = c(a = a1:a2)),
    inspect_grouping(data, .by = c(a1, a2))
  )
  expect_error(
    inspect_grouping(remote, .by = c(a1, a1 = a2)),
    class = "vctrs_error_names_must_be_unique"
  )
  expect_identical(counter$queries, 0L)
})

test_that("dtplyr proxy order determines the fixed key and summary grain", {
  skip_if_suggest_absent("dtplyr")
  source <- data.table::data.table(a = c("x", "y"), b = c(1L, 1L))
  original <- data.table::copy(source)
  step <- dtplyr::lazy_dt(source, immutable = TRUE)
  typed <- data.table::data.table(a = character(), b = integer())
  proxy_rows <- integer()
  rlang::local_bindings(
    tidyselect_data_proxy.data.table = function(x) {
      proxy_rows <<- c(proxy_rows, nrow(x))
      x[, c("b", "a")]
    },
    .env = asNamespace("tidyselect")$.__S3MethodsTable__.
  )

  direct <- rlang::quo(tidyselect::peek_vars()[1L])
  wrapped <- rlang::quo(identity(!!direct))
  expected_plan <- inspect_grouping(step, .by = b, .format = "list")
  expected_summary <- tibble::tibble(b = 1L, count = 2L)

  for (selection in list(direct, wrapped)) {
    expect_identical(tidyselect::eval_select(selection, typed), c(b = 1L))
    expect_identical(
      inspect_grouping(step, .by = !!selection, .format = "list"),
      expected_plan
    )
    expect_identical(
      dplyr::collect(summarize_with_margins(
        step, count = dplyr::n(), .by = !!selection
      )),
      expected_summary
    )
  }
  expect_true(length(proxy_rows) > 0L && all(proxy_rows == 0L))
  expect_identical(source, original)
})

test_that("dtplyr proxy order determines overlap for each constructor", {
  skip_if_suggest_absent("dtplyr")
  step <- dtplyr::lazy_dt(data.frame(a = c("x", "y"), b = c(1L, 1L)))
  rlang::local_bindings(
    tidyselect_data_proxy.data.frame = function(x) {
      if (inherits(x, "data.table")) x[, c("b", "a")] else x
    },
    .env = asNamespace("tidyselect")$.__S3MethodsTable__.
  )

  direct <- rlang::quo(tidyselect::peek_vars()[1L])
  wrapped <- rlang::quo(identity(!!direct))
  helper <- rlang::quo(tidyselect::all_of(!!direct))
  constructors <- list(grouping_set, grouping_sets, rollup, cube, grouping_spec)
  for (constructor in constructors) {
    expected <- inspect_grouping(
      step, .by = a, .grouping = constructor(b), .format = "list"
    )
    for (selection in list(direct, wrapped, helper)) {
      expect_identical(
        inspect_grouping(
          step, .by = a, .grouping = constructor(!!selection), .format = "list"
        ),
        expected
      )
      expect_error(
        inspect_grouping(step, .by = b, .grouping = constructor(!!selection)),
        "cannot appear in both", class = "marginplyr_error"
      )
    }
  }
})

test_that("typed DuckDB selections use captured names after proxy dispatch", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  data <- data.frame(a = 1L, b = "east")
  remote <- dplyr::copy_to(con, data, "typed_grouping_proxy_names")

  rename <- TRUE
  rlang::local_bindings(
    tidyselect_data_proxy.tbl_df = function(x) {
      if (identical(attr(x, "names", exact = TRUE), c("a", "b"))) {
        x <- x[c("b", "a")]
        if (rename) names(x) <- c("invented", "a")
      }
      x
    },
    .env = asNamespace("tidyselect")$.__S3MethodsTable__.
  )

  diagnostic <- "DuckDB selection proxy returned names absent from the input"
  expect_error(
    inspect_grouping(remote, .by = where(is.character)),
    diagnostic, fixed = TRUE, class = "marginplyr_error"
  )
  expect_error(
    inspect_grouping(remote, .grouping = grouping_set(where(is.character))),
    diagnostic, fixed = TRUE, class = "marginplyr_error"
  )

  rename <- FALSE
  expect_identical(
    inspect_grouping(remote, .by = where(is.character)),
    inspect_grouping(data, .by = b)
  )
  expect_identical(
    inspect_grouping(remote, .grouping = grouping_set(where(is.character))),
    inspect_grouping(data, .grouping = grouping_set(b))
  )
})

test_that("DuckDB proxy order determines Grouping keys and summary grain", {
  skip_if_suggest_absent("duckdb", "DBI")
  con <- duckdb_test_connection()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  source <- tibble::tibble(a = c("east", "west"), b = c(1L, 1L))
  original <- source
  remote <- dplyr::copy_to(con, source, "duckdb_context_proxy_order")
  DBI::dbExecute(con, "CREATE SEQUENCE grouping_proxy_reads START 1")
  input <- dplyr::mutate(
    remote,
    a = dbplyr::sql("a || CAST(nextval('grouping_proxy_reads') AS VARCHAR)")
  )
  typed <- dplyr::collect(utils::head(input, 0L))
  selectors <- rlang::quos(
    tidyselect::peek_vars()[1L],
    tidyselect::peek_vars()[[1L]],
    names(tidyselect::peek_data())[1L],
    names(tidyselect::peek_data())[[1L]]
  )
  counter <- duckdb_query_counter()
  on.exit(counter$untrace(), add = TRUE, after = FALSE)
  expect_identical(DBI::dbGetQuery(con, "SELECT 1 AS n")$n, 1L)
  counter$queries <- 0L
  for (selector in selectors) {
    expect_identical(tidyselect::eval_select(selector, typed), c(a = 1L))
    expect_identical(
      inspect_grouping(input, .by = !!selector, .format = "list")$fixed,
      list("a")
    )
  }
  expect_identical(counter$queries, 0L)

  proxy_rows <- integer()
  rlang::local_bindings(
    tidyselect_data_proxy.tbl_df = function(x) {
      proxy_rows <<- c(proxy_rows, nrow(x))
      if (identical(attr(x, "names", exact = TRUE), c("a", "b"))) {
        x[c("b", "a")]
      } else {
        x
      }
    },
    .env = asNamespace("tidyselect")$.__S3MethodsTable__.
  )
  counter$queries <- 0L
  expect_error(
    inspect_grouping(input, .by = absent),
    "`absent` doesn't exist", fixed = TRUE,
    class = "vctrs_error_subscript_oob"
  )
  expect_identical(
    inspect_grouping(input, .by = b, .format = "list")$fixed, list("b")
  )
  expect_identical(counter$queries, 0L)
  constructors <- list(grouping_set, grouping_sets, rollup, cube, grouping_spec)
  for (selector in selectors) {
    wrapped <- rlang::quo(identity(!!selector))
    for (selection in list(selector, wrapped)) {
      expect_identical(tidyselect::eval_select(selection, typed), c(b = 1L))
      counter$queries <- 0L
      expect_identical(
        inspect_grouping(input, .by = !!selection, .format = "list")$fixed,
        list("b")
      )
      expect_identical(counter$queries, 1L)
      for (constructor in constructors) {
        expected <- inspect_grouping(
          typed, .by = a, .grouping = constructor(b), .format = "list"
        )
        counter$queries <- 0L
        expect_identical(
          inspect_grouping(
            input, .by = a, .grouping = constructor(!!selection),
            .format = "list"
          ),
          expected
        )
        expect_identical(counter$queries, 1L)
        counter$queries <- 0L
        expect_error(
          inspect_grouping(
            input, .by = b, .grouping = constructor(!!selection)
          ),
          "cannot appear in both", class = "marginplyr_error"
        )
        expect_identical(counter$queries, 1L)
      }
    }
  }
  expect_true(length(proxy_rows) > 0L && all(proxy_rows == 0L))
  expect_true(is.na(DBI::dbGetQuery(
    con,
    paste0("SELECT last_value FROM duckdb_sequences() ",
           "WHERE sequence_name = 'grouping_proxy_reads'")
  )$last_value))

  for (selector in selectors) {
    for (selection in list(selector, rlang::quo(identity(!!selector)))) {
      result <- dplyr::collect(summarize_with_margins(
        remote, count = dplyr::n(), .by = !!selection
      ))
      expect_identical(result$b, 1L)
      expect_identical(as.double(result$count), 2)
      expect_named(result, c("b", "count"))
    }
  }
  expect_identical(dplyr::collect(remote), original)
  expect_identical(source, original)
})
