args <- commandArgs(trailingOnly = TRUE)
Sys.unsetenv("TESTTHAT")
.libPaths(c(args[[5L]], .libPaths()))
library(marginplyr)
source(args[[6L]])
.data <- rlang::.data

run_case <- function() {
  case <- args[[1L]]
  audited <- identical(args[[2L]], "true")
  warning_level <- as.integer(args[[3L]])
  options(marginplyr.audit_sql = audited, warn = warning_level,
          lifecycle_verbosity = if (args[[7L]] == "unset") NULL else args[[7L]])
  settings <- c("warn", "lifecycle_verbosity", "rlib_warning_verbosity")
  before <- options()[settings]
  if (case == "collision") {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  } else {
    con <- duckdb_test_connection()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  }
  remote <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "lifecycle_source")
  remote <- dplyr::mutate(
    remote,
    g2 = dplyr::if_else(
      # str_like() is a dbplyr SQL translation with no R function binding.
      str_like( # nolint: object_usage_linter.
        .data$g, "a", ignore_case = FALSE
      ),
      .data$g, .data$g
    )
  )
  warnings <- list()
  observe <- function(expr) {
    tryCatch(withCallingHandlers(expr, warning = function(cnd) {
      warnings[[length(warnings) + 1L]] <<- list(
        classes = class(cnd), message = conditionMessage(cnd)
      )
    }), error = function(cnd) {
      list(classes = class(cnd), message = conditionMessage(cnd))
    })
  }
  value <- observe(if (case == "inspection") {
    inspect_grouping(remote, .grouping = rollup(dplyr::all_of("g2")))
  } else {
    summarize_with_margins(
      remote, total = sum(.data$v, na.rm = TRUE),
      .grouping = rollup(dplyr::all_of("g2")),
      .check_margin_label = case == "collision"
    )
  })
  delivered <- warnings
  after <- options()[settings]
  plain <- if (audited) last_sent_queries() else NULL
  details <- if (audited) last_sent_queries(details = TRUE) else NULL
  internal <- if (audited) last_sent_queries(FALSE, TRUE) else NULL
  stable <- !audited || identical(last_sent_queries(details = TRUE), details)
  accessor_warnings <- warnings
  # The same input translation ordinarily warns only on its first use.
  warnings <- list()
  subsequent <- observe(dbplyr::sql_render(remote))
  result_class <- if (case == "inspection") "data.frame" else "tbl_sql"
  returned <- inherits(value, result_class)
  list(
    returned = returned, error = if (returned) NULL else value,
    warnings = delivered, options_restored = identical(before, after),
    plain = plain, details = details, internal = internal,
    stable = stable, accessor_warnings = accessor_warnings,
    subsequent_warnings = warnings,
    subsequent_returned = inherits(subsequent, "sql")
  )
}

saveRDS(run_case(), args[[4L]])
