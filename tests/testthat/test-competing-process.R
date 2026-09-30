# Native fallback and configured hooks require independent top-level processes.
# The installed candidate runs under coverage/check, without source helpers.
competing_condition_process <- function(route, handler, hooks) {
  output <- tempfile(fileext = ".rds")
  on.exit(unlink(output), add = TRUE)
  libraries <- .libPaths()
  installed <- libraries[file.exists(file.path(
    libraries, "marginplyr", "Meta", "package.rds"
  ))][[1L]]
  log <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(testthat::test_path(
      "fixtures", "competing-condition-process.R"
    )), shQuote(installed), route, handler, hooks, shQuote(output)),
    stdout = TRUE, stderr = TRUE, timeout = 15
  ))
  list(status = attr(log, "status") %||% 0L,
       result = readRDS(output), log = log)
}

test_that("summary interruption matches native handlers and configured hooks", {
  hook_options <- c("default", "error_function", "error_expression",
                    "interrupt", "both")
  for (hooks in hook_options) {
    for (handler in c("exiting", "both", "calling", "error_only", "none")) {
      native <- competing_condition_process("native", handler, hooks)
      summary <- competing_condition_process("summary", handler, hooks)
      events <- summary$result$events
      expect_identical(summary$status, native$status,
                       info = paste(summary$log, collapse = "\n"))
      expect_false("error handler" %in% events)
      expect_identical("returned" %in% events,
                       "returned" %in% native$result$events)
      expect_identical(
        events[grepl("hook", events)],
        native$result$events[grepl("hook", native$result$events)]
      )
      expect_identical(events[events %in% c("earlier effect", "later branch")],
                       c("earlier effect", "earlier effect", "later branch"))
      if (handler %in% c("exiting", "both", "calling")) {
        first <- summary$result$observed[[1L]]
        expect_s3_class(first, "interrupt")
        expect_false(inherits(first, "error"))
        expect_s3_class(first$interrupt, "interrupt")
        expect_s3_class(first$replay_error, "error")
        expect_match(conditionMessage(first$replay_error),
                     "converted from warning")
        if (handler == "both") {
          expect_identical(summary$result$observed[[2L]], first)
        }
      }
    }
  }
})
