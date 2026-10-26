# Uses dbplyr's dialect extension surface while retaining SQLite's translation
# and DBI operations. `signal` runs before each SUM is translated.
with_general_condition_source <- function(signal, code) {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  methods::setClass(
    "GeneralConditionSQLiteConnection", contains = "SQLiteConnection"
  )
  on.exit(methods::removeClass("GeneralConditionSQLiteConnection"), add = TRUE)
  dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(1, 2)), "general_condition_source"
  )
  subclass <- methods::new("GeneralConditionSQLiteConnection", con)
  dialect <- dbplyr::sql_dialect(con)
  variant <- dbplyr::sql_translation(con)
  original_sum <- variant$aggregate$sum
  registerS3method(
    "sql_dialect", "GeneralConditionSQLiteConnection",
    function(con) {
      structure(dialect, class = c("general_condition_sqlite", class(dialect)))
    }, envir = asNamespace("dbplyr")
  )
  registerS3method(
    "sql_translation", "general_condition_sqlite",
    function(con) {
      dbplyr::sql_variant(
        variant$scalar,
        dbplyr::sql_translator(
          .parent = variant$aggregate,
          sum = function(...) {
            signal()
            original_sum(...)
          }
        ),
        variant$window
      )
    }, envir = asNamespace("dbplyr")
  )
  on.exit(rm(
    list = c("sql_dialect.GeneralConditionSQLiteConnection",
             "sql_translation.general_condition_sqlite"),
    envir = asNamespace("dbplyr")$.__S3MethodsTable__.
  ), add = TRUE)
  operations <- new.env(parent = emptyenv())
  operations$counts <- c(send = 0L, fetch = 0L)
  send <- DBI::dbSendQuery
  fetch <- DBI::dbFetch
  testthat::local_mocked_bindings(
    dbSendQuery = function(...) {
      operations$counts[["send"]] <- operations$counts[["send"]] + 1L
      send(...)
    },
    dbFetch = function(...) {
      operations$counts[["fetch"]] <- operations$counts[["fetch"]] + 1L
      fetch(...)
    }, .package = "DBI"
  )
  remote <- dplyr::tbl(subclass, "general_condition_source")
  code(remote, operations)
}

test_that("SQL auditing isolates general diagnostic translation conditions", {
  old <- options(marginplyr.audit_sql = FALSE)
  on.exit(options(old), add = TRUE)
  leaf <- simpleCondition("SQLite translated SUM")
  signal <- function() signalCondition(leaf)
  with_general_condition_source(signal, function(remote, operations) {
    run <- function(audited) {
      options(marginplyr.audit_sql = audited)
      delivered <- list()
      before <- operations$counts
      query <- withCallingHandlers(
        summarize_with_margins(
          remote, total = sum(.data$v, na.rm = TRUE),
          .grouping = rollup(dplyr::all_of("g")), .check_margin_label = TRUE
        ),
        condition = function(cnd) {
          delivered[[length(delivered) + 1L]] <<- cnd
        }
      )
      list(query = query, delivered = delivered,
           operations = operations$counts - before)
    }
    off <- run(FALSE)
    on <- run(TRUE)
    expect_identical(off$delivered, list(leaf))
    expect_identical(on$delivered, off$delivered)
    expect_identical(class(on$query), class(off$query))
    expect_identical(on$operations, off$operations)
    expect_true(all(on$operations > 0L))
    rows <- last_sent_queries(details = TRUE)
    expect_identical(rows$status, c("returned", "prepared"))
    empty <- dplyr::tibble(
      classes = list(), message = character(), phase = character(),
      handling = character()
    )
    expect_identical(rows$conditions, list(empty, empty))
    expect_identical(last_sent_queries(), rows[c("purpose", "sql")])
    expect_identical(last_sent_queries(FALSE, TRUE), rows[1L, ])
    off_values <- dplyr::collect(off$query)
    on_values <- dplyr::collect(on$query)
    expect_identical(on_values, off_values)
    expect_equal(
      on_values[match(c("a", "b", "Total"), on_values$g), ],
      dplyr::tibble(g = c("a", "b", "Total"), total = c(1, 2, 3))
    )
    expect_identical(last_sent_queries(details = TRUE), rows)
  })
})

test_that("SQL condition summaries isolate general formatter notifications", {
  old <- options(marginplyr.audit_sql = FALSE, warn = 0)
  on.exit(options(old), add = TRUE)
  secondary <- simpleCondition("secondary formatter notification")
  formats <- 0L
  method <- function(cnd) {
    # Only diagnostic formatting signals: the original warning's formatter
    # must behave identically in both runs, independently of call counts.
    in_summary <- any(vapply(sys.calls(), function(call) {
      is.call(call) &&
        identical(call[[1L]], as.name("sent_query_condition_message"))
    }, logical(1)))
    if (in_summary) {
      formats <<- formats + 1L
      signalCondition(secondary)
    }
    cnd$message
  }
  registerS3method("conditionMessage", "general_formatter_warning", method,
                   envir = asNamespace("base"))
  on.exit(rm(
    "conditionMessage.general_formatter_warning",
    envir = asNamespace("base")$.__S3MethodsTable__.
  ), add = TRUE)
  leaf <- structure(
    list(message = "SQLite translated SUM", call = NULL),
    class = c("general_formatter_warning", "warning", "condition")
  )
  signal <- function() warning(leaf)
  with_general_condition_source(signal, function(remote, operations) {
    run <- function(audited) {
      options(marginplyr.audit_sql = audited)
      settings <- options(c(
        "warn", "rlib_warning_verbosity", "lifecycle_verbosity"
      ))
      delivered <- list()
      before <- operations$counts
      query <- withCallingHandlers(
        summarize_with_margins(
          remote, total = sum(.data$v, na.rm = TRUE),
          .grouping = rollup(dplyr::all_of("g")), .check_margin_label = TRUE
        ),
        condition = function(cnd) {
          delivered[[length(delivered) + 1L]] <<- cnd
          if (inherits(cnd, "warning")) invokeRestart("muffleWarning")
        }
      )
      expect_identical(options(names(settings)), settings)
      list(query = query, delivered = delivered,
           operations = operations$counts - before)
    }
    for (warn in c(-1, 0, 1, 2)) {
      options(warn = warn)
      off <- run(FALSE)
      on <- run(TRUE)
      expect_identical(off$delivered, list(leaf))
      expect_identical(on$delivered, off$delivered)
      expect_identical(class(on$query), class(off$query))
      expect_identical(on$operations, off$operations)
      expect_true(all(on$operations > 0L))
      rows <- last_sent_queries(details = TRUE)
      expect_identical(rows$status, c("returned", "prepared"))
      expect_true(all(vapply(rows$conditions, function(summary) {
        nrow(summary) > 0L && all(summary$message == leaf$message) &&
          all(vapply(summary$classes, identical, logical(1), class(leaf)))
      }, logical(1))))
      expect_identical(last_sent_queries(), rows[c("purpose", "sql")])
      expect_identical(last_sent_queries(FALSE, TRUE), rows[1L, ])
      off_values <- suppressWarnings(dplyr::collect(off$query))
      on_values <- suppressWarnings(dplyr::collect(on$query))
      expect_identical(on_values, off_values)
      expect_equal(
        on_values[match(c("a", "b", "Total"), on_values$g), ],
        dplyr::tibble(g = c("a", "b", "Total"), total = c(1, 2, 3))
      )
      expect_identical(last_sent_queries(details = TRUE), rows)
    }
    expect_gt(formats, 0L)
  })
})

test_that("invocation notifications retain restarts and nested ownership", {
  old <- options(marginplyr.audit_sql = FALSE)
  on.exit(options(old), add = TRUE)
  leaf <- simpleCondition("resumable SQLite translation")
  with_general_condition_source(function() {
    withRestarts(signalCondition(leaf), resume_translation = function() NULL)
  }, function(remote, operations) {
    run <- function(audited) {
      options(marginplyr.audit_sql = audited)
      delivered <- list()
      held <- NULL
      query <- withCallingHandlers(
        summarize_with_margins(
          remote, total = sum(.data$v, na.rm = TRUE),
          .grouping = rollup(dplyr::all_of("g")), .check_margin_label = TRUE
        ),
        condition = function(cnd) {
          delivered[[length(delivered) + 1L]] <<- cnd
          if (audited) held <<- last_sent_queries(details = TRUE)
          inspect_grouping(dbplyr::lazy_frame(inner = 1))
          if (audited) {
            expect_identical(last_sent_queries(details = TRUE), held)
          }
          invokeRestart("resume_translation")
        }
      )
      list(query = query, delivered = delivered, held = held)
    }
    off <- run(FALSE)
    on <- run(TRUE)
    expect_identical(off$delivered, list(leaf))
    expect_identical(on$delivered, off$delivered)
    expect_identical(class(on$query), class(off$query))
    rows <- last_sent_queries(details = TRUE)
    expect_identical(rows$purpose, c("observed_label_collision", "result"))
    expect_identical(rows$status, c("returned", "prepared"))
    expect_identical(on$held$status, "invoking")
    expect_identical(on$held$purpose, "observed_label_collision")
    expect_identical(dplyr::collect(on$query), dplyr::collect(off$query))
    expect_identical(last_sent_queries(details = TRUE), rows)
  })
})
