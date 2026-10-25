# These fixtures enter the condition observation seam directly: public Margin
# error handling would unwind on a resumable error-class signal before a caller
# could resume it. Outcomes and summaries are read through the public accessor.
test_that("resumed compound signals remain separate ordered summaries", {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  leaf <- errorCondition("repeated driver condition", class = "warning")
  marker <- simpleWarning("between compound signals")
  signals <- list(
    leaf, marker, leaf,
    errorCondition("repeated driver condition", class = "warning")
  )
  run <- function(audited) {
    options(marginplyr.audit_sql = audited)
    inspect_grouping(dbplyr::tbl_lazy(data.frame(v = 1)))
    delivered <- list()
    value <- withCallingHandlers(
      with_sent_query("fixture", dbplyr::sql("SELECT 1"), {
        message("unrecorded driver message")
        for (cnd in signals) {
          withRestarts(signalCondition(cnd), resume_signal = function() NULL)
        }
        42L
      }),
      condition = function(cnd) {
        if (inherits(cnd, "message")) {
          invokeRestart("muffleMessage")
        }
        delivered[[length(delivered) + 1L]] <<- cnd
        invokeRestart("resume_signal")
      }
    )
    list(value = value, delivered = delivered)
  }
  off <- run(FALSE)
  on <- run(TRUE)
  expect_identical(on, off)
  expect_identical(on$value, 42L)
  expect_identical(on$delivered, signals)
  rows <- last_sent_queries(FALSE, TRUE)
  expect_identical(rows$status, "returned")
  expect_identical(rows$conditions[[1L]], dplyr::tibble(
    classes = list(
      c("warning", "error", "condition"),
      c("simpleWarning", "warning", "condition"),
      c("warning", "error", "condition"),
      c("warning", "error", "condition")
    ),
    message = c("repeated driver condition", "between compound signals",
                "repeated driver condition", "repeated driver condition"),
    phase = rep("invocation", 4L), handling = rep("propagated", 4L)
  ))
  testthat::local_mocked_bindings(
    sql_render = function(...) stop("accessor rendered SQL"),
    .package = "dbplyr"
  )
  for (iteration in seq_len(2L)) {
    expect_identical(last_sent_queries(FALSE, TRUE), rows)
    expect_identical(last_sent_queries(), rows[c("purpose", "sql")])
  }
})

test_that("compound probe signals keep the known boundary handling", {
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  leaf <- errorCondition("compound probe condition", class = "warning")
  for (terminating in c(FALSE, TRUE)) {
    run <- function(audited) {
      options(marginplyr.audit_sql = audited)
      inspect_grouping(dbplyr::tbl_lazy(data.frame(v = 1)))
      delivered <- list()
      value <- withCallingHandlers(
        tryCatch(
          suppressWarnings(with_sent_query(
            "fixture", dbplyr::sql("SELECT 1"), {
              if (terminating) {
                stop(leaf)
              }
              warning(leaf)
              42L
            }, error_handling = "handled", warning_handling = "suppressed"
          )),
          error = identity
        ),
        condition = function(cnd) {
          delivered[[length(delivered) + 1L]] <<- cnd
        }
      )
      list(value = value, delivered = delivered)
    }
    off <- run(FALSE)
    on <- run(TRUE)
    expect_identical(on, off)
    expect_identical(on, list(
      value = if (terminating) leaf else 42L, delivered = list()
    ))
    rows <- last_sent_queries(FALSE, TRUE)
    expect_identical(rows$status, if (terminating) "errored" else "returned")
    expect_identical(rows$conditions[[1L]], dplyr::tibble(
      classes = list(c("warning", "error", "condition")),
      message = "compound probe condition", phase = "invocation",
      handling = if (terminating) "handled" else "suppressed"
    ))
  }
})
