# Interrupt the first record allocation, then let later tibble calls run. The
# interrupt occurs before the public entry point can register its restoration.
with_entry_interrupt <- function(expr, leaf) {
  original <- dplyr::tibble
  interrupted <- FALSE
  testthat::local_mocked_bindings(tibble = function(...) {
    if (!interrupted) {
      interrupted <<- TRUE
      stop(leaf)
    }
    original(...)
  }, .package = "dplyr")
  force(expr)
}

test_that("interrupted record allocation leaves no active outer owner", {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  saved <- as.list(sent_queries, all.names = TRUE)
  on.exit({
    rm(list = ls(sent_queries, all.names = TRUE), envir = sent_queries)
    list2env(saved, envir = sent_queries)
  }, add = TRUE)
  input <- data.frame(g = "a", v = 1)
  remote <- dbplyr::tbl_lazy(input)
  summarize_with_margins(
    remote, previous = sum(v, na.rm = TRUE), .grouping = rollup(g)
  )
  previous <- last_sent_queries(details = TRUE)
  leaf <- structure(list(message = "record interrupted", call = NULL),
                    class = c("entry_interrupt", "interrupt", "condition"))
  caught <- with_entry_interrupt(
    tryCatch(inspect_grouping(input), interrupt = identity), leaf
  )
  expect_identical(caught, leaf)
  expect_false(sent_queries$active)
  expect_identical(last_sent_queries(details = TRUE), previous)

  fresh <- summarize_with_margins(
    remote, fresh = sum(v, na.rm = TRUE), .grouping = rollup(g)
  )
  expect_false(sent_queries$active)
  rows <- last_sent_queries(details = TRUE)
  expect_s3_class(rows, "tbl_df")
  expect_identical(rows$purpose, "result")
  expect_identical(rows$status, "prepared")
  expect_identical(rows$sql, as.character(dbplyr::sql_render(fresh)))
  expect_false(identical(rows, previous))
})

test_that("interrupted nested record allocation preserves its active parent", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  saved <- as.list(sent_queries, all.names = TRUE)
  on.exit({
    rm(list = ls(sent_queries, all.names = TRUE), envir = sent_queries)
    list2env(saved, envir = sent_queries)
  }, add = TRUE)
  input <- data.frame(g = "a", v = 1)
  remote <- dplyr::copy_to(con, input, "entry_unwind_source")
  original_collect <- dplyr::collect
  leaf <- structure(list(message = "record interrupted", call = NULL),
                    class = c("entry_interrupt", "interrupt", "condition"))
  observed <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(collect = function(x, ...) {
    observed$before <- last_sent_queries(FALSE, TRUE)
    observed$caught <- with_entry_interrupt(
      tryCatch(inspect_grouping(input), interrupt = identity), leaf
    )
    observed$active <- sent_queries$active
    observed$after <- last_sent_queries(FALSE, TRUE)
    original_collect(x, ...)
  }, .package = "dplyr")
  result <- summarize_with_margins(
    remote, total = sum(v, na.rm = TRUE), .grouping = rollup(g),
    .check_margin_label = TRUE
  )
  expect_identical(observed$before$status, "invoking")
  expect_identical(observed$caught, leaf)
  expect_true(observed$active)
  expect_identical(observed$after, observed$before)
  expect_false(sent_queries$active)
  rows <- last_sent_queries(details = TRUE)
  expect_identical(rows$purpose, c("observed_label_collision", "result"))
  expect_identical(rows$status, c("returned", "prepared"))
  expect_identical(rows$sql[[2L]], as.character(dbplyr::sql_render(result)))
})
