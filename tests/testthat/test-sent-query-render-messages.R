# The dialect extension forwards SQLite's SUM translation unchanged after a
# message. The public verb and accessor distinguish invocation from rendering.
test_that("SQL auditing preserves invocation messages from a live dialect", {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(
    marginplyr.audit_sql = FALSE,
    rlib_warning_verbosity = "verbose", lifecycle_verbosity = "warning"
  )
  on.exit(options(old), add = TRUE)
  methods::setClass(
    "RenderMessageSQLiteConnection", contains = "SQLiteConnection"
  )
  on.exit(methods::removeClass("RenderMessageSQLiteConnection"), add = TRUE)
  dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(1, 2)), "render_message_source"
  )
  subclass <- methods::new("RenderMessageSQLiteConnection", con)
  dialect <- dbplyr::sql_dialect(con)
  variant <- dbplyr::sql_translation(con)
  original_sum <- variant$aggregate$sum
  leaf <- simpleMessage("SQLite translated SUM")
  translations <- 0L
  registerS3method(
    "sql_dialect", "RenderMessageSQLiteConnection",
    function(con) {
      structure(dialect, class = c("render_message_sqlite", class(dialect)))
    }, envir = asNamespace("dbplyr")
  )
  registerS3method(
    "sql_translation", "render_message_sqlite",
    function(con) {
      dbplyr::sql_variant(
        variant$scalar,
        dbplyr::sql_translator(
          .parent = variant$aggregate,
          sum = function(...) {
            translations <<- translations + 1L
            if (mode == "ordinary") message(leaf) else signalCondition(leaf)
            original_sum(...)
          }
        ),
        variant$window
      )
    }, envir = asNamespace("dbplyr")
  )
  on.exit(rm(
    list = c("sql_dialect.RenderMessageSQLiteConnection",
             "sql_translation.render_message_sqlite"),
    envir = asNamespace("dbplyr")$.__S3MethodsTable__.
  ), add = TRUE)
  remote <- dplyr::tbl(subclass, "render_message_source")
  run <- function(audited) {
    options(marginplyr.audit_sql = audited)
    settings <- options(c("rlib_warning_verbosity", "lifecycle_verbosity"))
    delivered <- list()
    query <- withCallingHandlers(
      summarize_with_margins(
        remote, total = sum(.data$v, na.rm = TRUE),
        .grouping = rollup(dplyr::all_of("g")), .check_margin_label = TRUE
      ),
      message = function(cnd) {
        delivered[[length(delivered) + 1L]] <<- cnd
        # A bare signal has no restart of its own, even inside caller handling.
        if (mode == "ordinary") invokeRestart("muffleMessage")
      }
    )
    expect_identical(options(names(settings)), settings)
    list(query = query, delivered = delivered)
  }
  for (mode in c("ordinary", "bare")) {
    off <- run(FALSE)
    on <- run(TRUE)
    expect_identical(off$delivered, list(leaf))
    expect_identical(on$delivered, off$delivered)
    expect_identical(class(on$query), class(off$query))
    rows <- last_sent_queries(details = TRUE)
    expect_named(rows, c("purpose", "sql", "source", "status", "conditions"))
    expect_identical(rows$status, c("returned", "prepared"))
    expect_identical(rows$source, rep("render", 2L))
    empty <- dplyr::tibble(
      classes = list(), message = character(), phase = character(),
      handling = character()
    )
    expect_identical(rows$conditions, list(empty, empty))
    before_reads <- translations
    expect_message(last_sent_queries(), NA)
    expect_identical(last_sent_queries(), rows[c("purpose", "sql")])
    expect_identical(last_sent_queries(FALSE, TRUE), rows[1L, ])
    expect_identical(last_sent_queries(details = TRUE), rows)
    expect_identical(translations, before_reads)
    # Collection belongs to the caller after construction; it is not audited.
    off_values <- suppressMessages(dplyr::collect(off$query))
    on_values <- suppressMessages(dplyr::collect(on$query))
    expect_identical(on_values, off_values)
    expect_equal(
      on_values[match(c("a", "b", "Total"), on_values$g), ],
      dplyr::tibble(g = c("a", "b", "Total"), total = c(1, 2, 3))
    )
    expect_identical(last_sent_queries(details = TRUE), rows)
  }
  mode <- "bare"
  completed <- FALSE
  nested <- NULL
  withCallingHandlers(
    message("caller message"),
    message = function(cnd) {
      nested <<- run(TRUE)
      completed <<- TRUE
      invokeRestart("muffleMessage")
    }
  )
  expect_true(completed)
  expect_identical(nested$delivered, list(leaf))
  expect_s3_class(nested$query, "tbl_sql")
  expect_identical(last_sent_queries(details = TRUE)$status,
                   c("returned", "prepared"))
})

# Controlled render signals exercise compound severities without requiring a
# stock backend to produce them; summaries are read through the public accessor.
test_that("message classes do not mask diagnostic warnings or errors", {
  old <- options(marginplyr.audit_sql = TRUE, warn = 0)
  on.exit(options(old), add = TRUE)
  cases <- list(
    c("message", "warning", "condition"),
    c("warning", "message", "condition"),
    c("message", "error", "condition"),
    c("error", "message", "condition"),
    c("message", "error", "warning", "condition")
  )
  for (classes in cases) {
    leaf <- structure(
      list(message = "compound render condition", call = NULL), class = classes
    )
    testthat::local_mocked_bindings(sql_render = function(...) {
      signalCondition(leaf)
      "SELECT 1"
    }, .package = "dbplyr")
    for (warn in c(0, 2)) {
      options(warn = warn)
      settings <- options(c(
        "warn", "rlib_warning_verbosity", "lifecycle_verbosity"
      ))
      delivered <- list()
      query <- withCallingHandlers(
        summarize_with_margins(dbplyr::lazy_frame(v = 1), total = sum(v)),
        condition = function(cnd) {
          delivered[[length(delivered) + 1L]] <<- cnd
        }
      )
      expect_s3_class(query, "tbl_lazy")
      expect_identical(delivered, list())
      expect_identical(options(names(settings)), settings)
      rows <- last_sent_queries(details = TRUE)
      expect_identical(rows$sql, NA_character_)
      expect_identical(rows$status, "prepared")
      handling <- if ("warning" %in% classes) "suppressed" else "handled"
      expect_identical(rows$conditions, list(dplyr::tibble(
        classes = list(classes), message = "compound render condition",
        phase = "render",
        handling = handling
      )))
    }
  }
})
