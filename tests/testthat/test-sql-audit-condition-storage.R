test_that("SQL condition summaries retain plain classes and diagnostic text", {
  skip_if_suggest_absent("RSQLite", "DBI")
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(1, 2)), "condition_storage"
  )
  leaf <- simpleWarning("attributed SQL warning")
  attr(attr(leaf, "class"), "connection") <- con
  attr(leaf$message, "environment") <- environment()
  collect <- dplyr::collect
  testthat::local_mocked_bindings(collect = function(x, ...) {
    warning(leaf)
    collect(x, ...)
  }, .package = "dplyr")

  observed <- NULL
  query <- withCallingHandlers(
    summarize_with_margins(
      source,
      amount = sum(.data$v, na.rm = TRUE),
      .grouping = rollup(dplyr::all_of("g")),
      .check_margin_label = TRUE
    ),
    warning = function(cnd) {
      observed <<- cnd
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(query, "tbl_lazy")
  expect_identical(observed, leaf)
  rows <- last_sent_queries(include_result = FALSE, details = TRUE)
  expect_identical(rows$purpose, "observed_label_collision")
  expect_identical(rows$status, "returned")
  summary <- rows$conditions[[1L]]
  expect_identical(summary$classes[[1L]], c(
    "simpleWarning", "warning", "condition"
  ))
  expect_null(attributes(summary$classes[[1L]]))
  expect_identical(summary$message, "attributed SQL warning")
  expect_null(attributes(summary$message))
})
