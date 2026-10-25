# Publication windows are too narrow for a deterministic backend fixture. An
# active rows binding injects at the real store; the public SQLite verb and
# accessor observe the outcome without replacing the invocation observer.
with_status_publication <- function(transition, interrupt, code) {
  skip_if_suggest_absent("RSQLite", "DBI")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "status_source")
  render <- dbplyr::sql_render
  installed <- FALSE
  hit <- FALSE
  testthat::local_mocked_bindings(sql_render = function(query, ...) {
    sql <- render(query, ...)
    if (!installed) {
      installed <<- TRUE
      record <- current_sent_query_scope()$record
      rows <- record$rows
      rm("rows", envir = record)
      makeActiveBinding("rows", function(value) {
        if (missing(value)) return(rows)
        entering <- identical(value$status, "invoking")
        leaving <- identical(rows$status, "invoking") && !entering
        if (!hit && transition == "terminal" && leaving) {
          hit <<- TRUE
          interrupt()
        }
        rows <<- value
        if (!hit && transition == "invoking" && entering) {
          hit <<- TRUE
          interrupt()
        }
        invisible(value)
      }, record)
    }
    sql
  }, .package = "dbplyr")
  check <- function() {
    summarize_with_margins(
      remote, total = sum(.data$v),
      .grouping = rollup(dplyr::all_of("g")), .check_margin_label = TRUE
    )
  }
  code(check)
  expect_true(hit)
}

test_that("interrupted invocation publication leaves unknown completion", {
  leaf <- structure(
    list(message = "publication interrupted", call = NULL),
    class = c("publication_interrupt", "interrupt", "condition")
  )
  with_status_publication("invoking", function() stop(leaf), function(check) {
    caught <- tryCatch(check(), interrupt = identity)
    expect_identical(caught, leaf)
    rows <- last_sent_queries(FALSE, TRUE)
    expect_identical(rows$status, "unknown")
    expect_identical(nrow(rows$conditions[[1L]]), 0L)
  })
})

test_that("terminal publication finishes before delivering cancellation", {
  # rlang's interrupt uses R's interrupt machinery and respects the publication
  # shield. A synthetic stop(interrupt) would bypass that machinery entirely.
  with_status_publication("terminal", rlang::interrupt, function(check) {
    observed <- list()
    caught <- tryCatch(withCallingHandlers({
      check()
      # Drain a deferred interrupt before inspecting the completed call.
      Sys.sleep(0)
    }, interrupt = function(cnd) {
      observed[[length(observed) + 1L]] <<- cnd
    }), interrupt = identity)
    expect_s3_class(caught, "interrupt")
    expect_identical(observed, list(caught))
    rows <- last_sent_queries(FALSE, TRUE)
    expect_identical(rows$status, "returned")
    expect_identical(nrow(rows$conditions[[1L]]), 0L)
  })
})
