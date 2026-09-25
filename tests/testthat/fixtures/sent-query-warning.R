args <- commandArgs(trailingOnly = TRUE)
Sys.unsetenv("TESTTHAT")
.libPaths(c(args[[4L]], .libPaths()))
library(marginplyr)
source(args[[5L]])
.data <- rlang::.data

case <- args[[1L]]
audited <- identical(args[[2L]], "true")
internal <- startsWith(case, "selection_proxy")
explicit <- endsWith(case, "explicit")
options(warn = 2, marginplyr.audit_sql = audited)
sent_sql <- character()

capture_error <- function(expr) {
  tryCatch(list(value = expr), error = function(cnd) {
    list(error = conditionMessage(cnd))
  })
}

run_case <- function() {
  old_verbosity <- getOption("rlib_warning_verbosity")
  if (!internal) {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  } else {
    con <- duckdb_test_connection()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  }

  data <- data.frame(g = c("a", "b", "b"), v = c(1, NA_real_, 2))
  remote <- dplyr::copy_to(con, data, "sent_query_warning")
  if (internal) {
    if (explicit) {
      remote <- remote |>
        dplyr::group_by(.data$g) |>
        dplyr::summarise(v = sum(.data$v, na.rm = TRUE), .groups = "drop")
    } else {
      remote <- remote |>
        dplyr::group_by(.data$g) |>
        dplyr::summarise(v = sum(.data$v), .groups = "drop")
    }
  }
  if (internal && explicit) {
    trace(
      "dbSendQuery", where = asNamespace("DBI"),
      signature = c("duckdb_connection", "character"),
      tracer = quote(sent_sql <<- c(sent_sql, statement)), print = FALSE
    )
    on.exit(untrace(
      "dbSendQuery", where = asNamespace("DBI"),
      signature = c("duckdb_connection", "character")
    ), add = TRUE)
  }

  if (!internal && !explicit) {
    result <- capture_error(summarize_with_margins(
      remote, z = sum(.data$v),
      .grouping = rollup(dplyr::all_of("g"))
    ))
  } else if (!internal) {
    result <- capture_error(summarize_with_margins(
      remote, z = sum(.data$v, na.rm = TRUE),
      .grouping = rollup(dplyr::all_of("g"))
    ))
  } else {
    result <- capture_error(summarize_with_margins(
      remote, z = sum(.data$v, na.rm = TRUE),
      .grouping = rollup(dplyr::all_of("g")),
      .check_margin_label = TRUE
    ))
  }
  record <- if (audited) last_sent_queries() else NULL
  if (is.null(result$error)) {
    result <- capture_error(dplyr::collect(result$value))
  }
  list(
    result = result, record = record, sent_sql = sent_sql,
    warning_verbosity_restored = identical(
      getOption("rlib_warning_verbosity"), old_verbosity
    )
  )
}

saveRDS(run_case(), args[[3L]])
