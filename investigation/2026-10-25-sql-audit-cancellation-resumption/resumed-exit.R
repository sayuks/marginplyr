# Run this diagnostic in a disposable Rscript process; it loads this checkout.
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
source(file.path(dirname(normalizePath(sub("^--file=", "", script_arg))),
                 "reproduction-setup.R"))
pkgload::load_all(hunt_checkout, quiet = TRUE)

run <- function(audited, kind, ending, machine_interrupt = FALSE) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  old <- options(marginplyr.audit_sql = audited)
  on.exit(options(old), add = TRUE)
  remote <- dplyr::copy_to(con, data.frame(g = "a", v = 1), "source")
  leaf <- structure(list(message = "resumable driver signal", call = NULL),
                    class = c(kind, "condition"))
  testthat::local_mocked_bindings(collect = function(...) {
    if (machine_interrupt) {
      withRestarts(rlang::interrupt(), resume_driver = function() NULL)
    } else {
      signalCondition(leaf)
    }
    if (ending == "leave") invokeRestart("leave_fixture")
    data.frame(g = 0)
  }, .package = "dplyr")
  observed <- list()
  result <- withRestarts(withCallingHandlers(
    marginplyr::summarize_with_margins(
      remote, total = sum(v), .grouping = marginplyr::rollup(g),
      .check_margin_label = TRUE
    ),
    interrupt = function(cnd) {
      observed[[length(observed) + 1L]] <<- cnd
      if (machine_interrupt) invokeRestart("resume_driver")
    }
  ), leave_fixture = function() "left")
  rows <- if (audited) marginplyr::last_sent_queries(FALSE, TRUE) else NULL
  list(result = if (inherits(result, "tbl_sql")) "returned" else result,
       observed = lapply(observed, function(cnd) {
         list(classes = class(cnd), message = conditionMessage(cnd))
       }), rows = rows)
}

failed <- FALSE
results <- list()
for (machine_interrupt in c(FALSE, TRUE)) {
  for (ending in c("return", "leave")) {
    off <- run(FALSE, "interrupt", ending, machine_interrupt)
    on <- run(TRUE, "interrupt", ending, machine_interrupt)
    stopifnot(identical(off[c("result", "observed")],
                        on[c("result", "observed")]))
    expected <- if (ending == "leave") "unknown" else "returned"
    cat("RLANG_INTERRUPT", machine_interrupt, "ENDING", ending, "EXPECTED", expected,
        "ACTUAL", on$rows$status, "RESULT", on$result, "\n")
    print(on$rows$conditions[[1L]])
    results[[length(results) + 1L]] <- list(
      rlang_interrupt = machine_interrupt, ending = ending, expected = expected,
      off = off, on = on
    )
    stopifnot(nrow(on$rows) == 1L,
              identical(on$rows$status,
                        if (ending == "leave") "interrupted" else "returned"))
    if (!identical(on$rows$status, expected)) failed <- TRUE
  }
}
saveRDS(results, hunt_output("resumed-exit.rds"))
quit(status = as.integer(failed))
