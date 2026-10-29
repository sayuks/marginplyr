# Native fallback and configured hooks require independent top-level processes.
# The installed candidate runs under coverage/check, without source helpers.
# The bounded deadline includes namespace loading on busy CI runners.
competing_condition_process <- function(route, handler, hooks, timeout = 60,
                                        script = testthat::test_path(
                                          "fixtures",
                                          "competing-condition-process.R"
                                        )) {
  output <- tempfile(fileext = ".rds")
  on.exit(unlink(output), add = TRUE)
  libraries <- .libPaths()
  installed <- libraries[file.exists(file.path(
    libraries, "marginplyr", "Meta", "package.rds"
  ))][[1L]]
  process_warnings <- character()
  started <- proc.time()[["elapsed"]]
  log <- withCallingHandlers(
    system2(
      file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(script), shQuote(installed), route, handler, hooks,
        shQuote(output)),
      stdout = TRUE, stderr = TRUE, timeout = timeout
    ),
    warning = function(cnd) {
      process_warnings <<- c(process_warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  elapsed <- proc.time()[["elapsed"]] - started
  status <- attr(log, "status") %||% 0L
  log <- c(log, process_warnings)
  context <- paste0("route=", route, ", handler=", handler, ", hooks=", hooks,
                    ", status=", status, ", limit=", timeout,
                    "s, elapsed=", round(elapsed, 2), "s")
  if (status == 124L) {
    rlang::abort(paste(c("Competing-condition subprocess timed out:",
                         context, log), collapse = "\n"),
                 class = "competing_process_timeout")
  }
  if (!file.exists(output)) {
    rlang::abort(
      paste(c("Competing-condition subprocess produced no checkpoint:",
              context, log), collapse = "\n"),
      class = "competing_process_incomplete"
    )
  }
  list(status = status, result = readRDS(output), log = log, context = context)
}

# A package verb cannot observe its test runner's deadline. Exercise the real
# subprocess boundary, including a partial checkpoint, rather than mock status.
test_that("a subprocess timeout never becomes a native outcome", {
  error <- expect_error(
    competing_condition_process(
      "slow", "none", "default", timeout = 1,
      script = testthat::test_path("fixtures", "competing-process-runner.R")
    ),
    class = "competing_process_timeout"
  )
  expect_match(conditionMessage(error), "route=slow", fixed = TRUE)
  expect_match(conditionMessage(error), "handler=none", fixed = TRUE)
  expect_match(conditionMessage(error), "hooks=default", fixed = TRUE)
  expect_match(conditionMessage(error), "limit=1s", fixed = TRUE)
  expect_match(conditionMessage(error), "status=124", fixed = TRUE)
  expect_match(conditionMessage(error), "elapsed=", fixed = TRUE)
})

test_that("missing subprocess checkpoints retain the child diagnostic", {
  error <- expect_error(
    competing_condition_process(
      "missing", "none", "default",
      script = testthat::test_path("fixtures", "competing-process-runner.R")
    ),
    class = "competing_process_incomplete"
  )
  expect_match(conditionMessage(error), "route=missing", fixed = TRUE)
  expect_match(conditionMessage(error), "startup failed", fixed = TRUE)
  expect_match(conditionMessage(error), "status=7", fixed = TRUE)
})

test_that("completed subprocesses keep their checkpoint and nonzero status", {
  result <- competing_condition_process(
    "exit", "none", "default",
    script = testthat::test_path("fixtures", "competing-process-runner.R")
  )
  expect_identical(result$status, 1L)
  expect_identical(result$result$events, "checkpoint")
  expect_match(paste(result$log, collapse = "\n"), "checkpoint written",
               fixed = TRUE)
  expect_match(result$context, "route=exit", fixed = TRUE)
  expect_match(result$context, "status=1", fixed = TRUE)
  expect_match(result$context, "elapsed=", fixed = TRUE)
})

test_that("a slow completed subprocess remains usable", {
  result <- competing_condition_process(
    "slow", "none", "default",
    script = testthat::test_path("fixtures", "competing-process-runner.R")
  )
  expect_identical(result$status, 0L)
  expect_identical(result$result$events, "checkpoint")
})

test_that("summary interruption matches native handlers and configured hooks", {
  hook_options <- c("default", "error_function", "error_expression",
                    "interrupt", "both")
  for (hooks in hook_options) {
    for (handler in c("exiting", "both", "calling", "error_only", "none")) {
      native <- competing_condition_process("native", handler, hooks)
      summary <- competing_condition_process("summary", handler, hooks)
      events <- summary$result$events
      info <- paste(c(native$context, native$log,
                      summary$context, summary$log), collapse = "\n")
      expect_identical(summary$status, native$status, info = info)
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
