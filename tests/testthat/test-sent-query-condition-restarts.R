test_that("render warnings without a muffle restart do not invent errors", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(
    con, data.frame(g = "a", v = 1), "unmuffled_render_source"
  )
  leaf <- simpleWarning("render warning without a restart", call = NULL)
  testthat::local_mocked_bindings(sql_render = function(...) {
    # `signalCondition()` is allowed to signal without `muffleWarning`.
    signalCondition(leaf)
    "SELECT 1"
  }, .package = "dbplyr")
  testthat::local_mocked_bindings(collect = function(...) data.frame(g = 0),
                                  .package = "dplyr")

  delivered <- list()
  query <- withCallingHandlers(
    summarize_with_margins(
      remote, total = sum(.data$v), .grouping = rollup(dplyr::all_of("g")),
      .check_margin_label = TRUE
    ),
    warning = function(cnd) {
      delivered[[length(delivered) + 1L]] <<- cnd
    }
  )
  expect_s3_class(query, "tbl_sql")
  expect_identical(delivered, list())
  rows <- last_sent_queries(details = TRUE)
  expect_identical(rows$sql, rep(NA_character_, 2L))
  expect_identical(rows$status, c("returned", "prepared"))
  expected <- dplyr::tibble(
    classes = list(class(leaf)), message = conditionMessage(leaf),
    phase = "render", handling = "suppressed"
  )
  expect_identical(rows$conditions, list(expected, expected))
})

test_that("condition formatting failures do not replace driver warnings", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(warn = 2, marginplyr.audit_sql = FALSE)
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(
    con, data.frame(g = "a", v = 1), "warning_format_source"
  )
  calls <- 0L
  method <- function(cnd) {
    calls <<- calls + 1L
    if (calls == 2L) {
      stop("diagnostic formatter failed")
    }
    cnd$message
  }
  registerS3method("conditionMessage", "audit_format_once_warning", method,
                   envir = asNamespace("base"))
  on.exit(rm(
    "conditionMessage.audit_format_once_warning",
    envir = asNamespace("base")$.__S3MethodsTable__.
  ), add = TRUE)
  leaf <- structure(
    list(message = "original driver warning", call = NULL),
    class = c("audit_format_once_warning", "warning", "condition")
  )
  testthat::local_mocked_bindings(collect = function(...) {
    warning(leaf)
    data.frame(g = 0)
  }, .package = "dplyr")
  run <- function() {
    tryCatch(
      summarize_with_margins(
        remote, total = sum(.data$v),
        .grouping = rollup(dplyr::all_of("g")), .check_margin_label = TRUE
      ),
      error = identity
    )
  }
  off <- run()
  expect_match(off$message, "converted from warning", fixed = TRUE)
  calls <- 0L
  options(marginplyr.audit_sql = TRUE)
  on <- run()
  expect_identical(on, off)
  rows <- last_sent_queries(FALSE, TRUE)
  expect_identical(rows$status, "errored")
  summaries <- rows$conditions[[1L]]
  expect_identical(summaries$classes[[1L]], class(leaf))
  expect_identical(summaries$message, c(leaf$message, off$message))
})

test_that("condition summaries isolate secondary formatting conditions", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(warn = 2, marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(
    con, data.frame(g = "a", v = 1), "format_condition_source"
  )
  calls <- 0L
  method <- function(cnd) {
    calls <<- calls + 1L
    if (calls == 1L) {
      return(cnd$message)
    }
    switch(cnd$format,
      warning = warning("formatter warning"),
      message = message("formatter message"),
      bare_warning = signalCondition(simpleWarning("formatter warning")),
      bare_message = signalCondition(simpleMessage("formatter message"))
    )
    if (cnd$format == "invalid") {
      return(character())
    }
    structure(cnd$message, retained_environment = environment())
  }
  registerS3method("conditionMessage", "audit_diagnostic_format", method,
                   envir = asNamespace("base"))
  on.exit(rm(
    "conditionMessage.audit_diagnostic_format",
    envir = asNamespace("base")$.__S3MethodsTable__.
  ), add = TRUE)
  classes <- c("audit_diagnostic_format", "warning", "condition")
  for (format in c("warning", "message", "bare_warning", "bare_message",
                   "invalid", "plain")) {
    calls <- 0L
    leaf <- structure(
      list(message = "stored driver warning", call = NULL, format = format),
      class = classes
    )
    testthat::local_mocked_bindings(collect = function(...) {
      signalCondition(leaf)
      data.frame(g = 0)
    }, .package = "dplyr")
    warnings <- list()
    messages <- list()
    query <- withCallingHandlers(
      summarize_with_margins(
        remote, total = sum(.data$v),
        .grouping = rollup(dplyr::all_of("g")), .check_margin_label = TRUE
      ),
      warning = function(cnd) {
        warnings[[length(warnings) + 1L]] <<- cnd
      },
      message = function(cnd) {
        messages[[length(messages) + 1L]] <<- cnd
      }
    )
    expect_s3_class(query, "tbl_sql")
    expect_identical(warnings, list(leaf))
    expect_identical(messages, list())
    rows <- last_sent_queries(FALSE, TRUE)
    expect_identical(rows$status, "returned")
    expect_identical(rows$conditions[[1L]]$message, "stored driver warning")
    expect_identical(rows$conditions[[1L]]$classes, list(classes))
  }
})

test_that("a condition without stored message retains a typed summary", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(
    con, data.frame(g = "a", v = 1), "missing_message_source"
  )
  calls <- 0L
  method <- function(cnd) {
    calls <<- calls + 1L
    if (calls == 1L) {
      return("driver warning")
    }
    stop("no stored diagnostic available")
  }
  registerS3method("conditionMessage", "audit_missing_message", method,
                   envir = asNamespace("base"))
  registerS3method("conditionCall", "audit_missing_message", function(cnd) NULL,
                   envir = asNamespace("base"))
  on.exit(rm(
    list = c("conditionMessage.audit_missing_message",
             "conditionCall.audit_missing_message"),
    envir = asNamespace("base")$.__S3MethodsTable__.
  ), add = TRUE)
  leaf <- structure(
    1, class = c("audit_missing_message", "warning", "condition")
  )
  testthat::local_mocked_bindings(collect = function(...) {
    withRestarts(signalCondition(leaf), muffleWarning = function() NULL)
    data.frame(g = 0)
  }, .package = "dplyr")
  query <- withCallingHandlers(
    summarize_with_margins(
      remote, total = sum(.data$v), .grouping = rollup(dplyr::all_of("g")),
      .check_margin_label = TRUE
    ),
    warning = function(cnd) invokeRestart("muffleWarning")
  )
  expect_s3_class(query, "tbl_sql")
  rows <- last_sent_queries(FALSE, TRUE)
  expect_identical(rows$status, "returned")
  expect_identical(rows$conditions[[1L]]$message, NA_character_)
  expect_identical(rows$conditions[[1L]]$classes, list(class(leaf)))
})
