# dbplyr 2.6.0 keeps `cte` as a deprecated but working argument of collect()
# and compute(); a direct SQLite Margin result accepts it as native dbplyr does.

# Record every statement RSQLite is asked to run. The caller registers
# `recorder$untrace()` on exit.
sqlite_statement_recorder <- function() {
  recorder <- new.env(parent = emptyenv())
  recorder$statements <- character()
  signature <- c("SQLiteConnection", "character")
  methods <- c("dbSendQuery", "dbSendStatement")
  for (method in methods) {
    # do.call retains the tracer's closure when S4 tracing wraps .local lazily.
    suppressMessages(do.call(trace, list(
      what = method, signature = signature, where = asNamespace("DBI"),
      print = FALSE,
      tracer = function() {
        statement <- get("statement", envir = parent.frame())
        recorder$statements <- c(recorder$statements, statement)
      }
    )))
  }
  recorder$untrace <- function() {
    for (method in methods) {
      suppressMessages(untrace(
        method, signature = signature, where = asNamespace("DBI")
      ))
    }
  }
  recorder
}

test_that("direct SQLite collect() accepts the deprecated cte argument", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(g = c("b", "a", "B"), v = 1:3))
  rlang::local_options(lifecycle_verbosity = "warning")
  source <- dplyr::tbl(con, "t")
  shapes <- list(
    unsorted = summarize_with_margins(
      source, n = sum(v, na.rm = TRUE), .grouping = rollup(g)
    ),
    sorted = summarize_with_margins(
      source, n = sum(v, na.rm = TRUE), .grouping = rollup(g), .sort = "last"
    ),
    typed_missing = summarize_with_margins(
      source, n = sum(v, na.rm = TRUE), .grouping = rollup(g),
      .margin_label = NULL
    ),
    boolean_word = summarize_with_margins(
      source, `TRUE` = sum(v, na.rm = TRUE), .grouping = rollup(g)
    ),
    expansion_sorted = expand_with_margins(
      source, .grouping = rollup(g), .sort = "last"
    ),
    expansion_unsorted = expand_with_margins(source, .grouping = rollup(g))
  )
  for (shape in names(shapes)) {
    query <- shapes[[shape]]
    for (n in list(Inf, 2L)) {
      expected <- dplyr::collect(query, n = n)
      for (cte in c(TRUE, FALSE)) {
        expect_warning(
          actual <- dplyr::collect(query, n = n, cte = cte),
          class = "lifecycle_warning_deprecated"
        )
        expect_identical(actual, expected,
                         info = paste(shape, n, cte))
      }
    }
    expect_error(
      dplyr::collect(query, cte = TRUE,
                     sql_options = dbplyr::sql_options(cte = TRUE)),
      "Exactly one", info = shape
    )
  }
})

test_that("direct SQLite collect() sends the CTE form cte = TRUE asks for", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(g = c("b", "a", "B"), v = 1:3))
  query <- summarize_with_margins(
    dplyr::tbl(con, "t"), n = sum(v, na.rm = TRUE), .grouping = rollup(g),
    .sort = "last"
  )
  recorder <- sqlite_statement_recorder()
  on.exit(recorder$untrace(), add = TRUE)
  dplyr::collect(query, sql_options = dbplyr::sql_options(cte = TRUE))
  expected <- recorder$statements
  expect_true(any(grepl("\\bWITH\\b", expected)))
  recorder$statements <- character()
  suppressWarnings(dplyr::collect(query, cte = TRUE))
  expect_identical(recorder$statements, expected)
})

test_that("direct SQLite compute(cte = TRUE) renders every statement as CTEs", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "t", data.frame(g = c("b", "a", "B"), v = 1:3))
  query <- summarize_with_margins(
    dplyr::tbl(con, "t"), n = sum(v, na.rm = TRUE), .grouping = rollup(g),
    .sort = "last"
  )
  recorder <- sqlite_statement_recorder()
  on.exit(recorder$untrace(), add = TRUE)
  inserts <- function() {
    grep("^INSERT INTO", recorder$statements, value = TRUE)
  }
  dplyr::compute(query, name = "with_options",
                 sql_options = dbplyr::sql_options(cte = TRUE))
  expected <- sub("`with_options`", "`report`", inserts(), fixed = TRUE)
  expect_true(all(grepl("\\bWITH\\b", expected)))
  recorder$statements <- character()
  rlang::local_options(lifecycle_verbosity = "warning")
  expect_warning(
    dplyr::compute(query, name = "deprecated", cte = TRUE),
    class = "lifecycle_warning_deprecated"
  )
  expect_identical(
    sub("`deprecated`", "`report`", inserts(), fixed = TRUE), expected
  )
})
