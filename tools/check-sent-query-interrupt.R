#!/usr/bin/env Rscript

# Bounded POSIX SIGINT stress for #810. Run from the repository root:
# Rscript tools/check-sent-query-interrupt.R
# No package function or ownership binding is replaced. Exit 0 means the
# observed interrupts left ownership and later records intact, 1 is a failed
# invariant, and 2 means no conclusive interrupt window was observed.
if (!identical(.Platform$OS.type, "unix")) {
  stop("This check requires POSIX SIGINT and fork support.", call. = FALSE)
}
pkgload::load_all(quiet = TRUE)
state <- utils::getFromNamespace("sent_queries", "marginplyr")

run_native <- function() {
  target_pid <- Sys.getpid()
  sender <- parallel::mcparallel({
    Sys.sleep(0.1)
    deadline <- proc.time()[["elapsed"]] + 12
    while (proc.time()[["elapsed"]] < deadline) {
      tools::pskill(target_pid, signal = 2L)
      Sys.sleep(0.0003)
    }
    NULL
  }, silent = TRUE, mc.set.seed = FALSE)
  stop_sender <- function() {
    tools::pskill(sender$pid, signal = 9L)
    suppressWarnings(parallel::mccollect(sender, wait = TRUE))
    invisible(NULL)
  }
  on.exit(stop_sender(), add = TRUE)
  deadline <- proc.time()[["elapsed"]] + 12
  caught <- 0L
  iterations <- 0L
  leaked <- FALSE
  input <- data.frame(g = 1)
  for (i in seq_len(10000L)) {
    if (proc.time()[["elapsed"]] >= deadline) break
    iterations <- i
    outcome <- tryCatch(allowInterrupts({
      inspect_grouping(input)
      for (j in seq_len(100L)) invisible(j)
    }), interrupt = identity)
    caught <- caught + as.integer(inherits(outcome, "interrupt"))
    if (isTRUE(state$active)) {
      leaked <- TRUE
      break
    }
  }
  stop_sender()
  on.exit(NULL)
  # Drain any signal queued outside the per-call delivery windows, after the
  # sender has been reaped and before checking independent calls.
  tryCatch(allowInterrupts({
    for (i in seq_len(10000L)) invisible(i)
  }), interrupt = function(cnd) NULL)
  cat("Native stress: iterations=", iterations, "; caught_interrupts=", caught,
      "; active_after_exit=", leaked, "\n", sep = "")
  if (leaked) return(1L)
  if (caught == 0L) return(2L)
  for (audited in c(FALSE, TRUE, TRUE)) {
    options(marginplyr.audit_sql = audited)
    query <- summarize_with_margins(
      dbplyr::lazy_frame(v = 1, .name = "after_interrupt"),
      fresh = sum(v, na.rm = TRUE)
    )
    options(marginplyr.audit_sql = !audited)
    rows <- tryCatch(last_sent_queries(details = TRUE), error = identity)
    valid <- if (audited) {
      identical(rows$purpose, "result") &&
        identical(rows$status, "prepared") &&
        identical(rows$sql, as.character(dbplyr::sql_render(query)))
    } else {
      inherits(rows, "marginplyr_error") &&
        grepl("marginplyr.audit_sql", conditionMessage(rows), fixed = TRUE)
    }
    if (!valid || isTRUE(state$active)) return(1L)
  }
  cat("Independent records and saved audit settings: passed\n")
  0L
}

old <- options(marginplyr.audit_sql = TRUE)
status <- NULL
tryCatch(suspendInterrupts({
  status <- run_native()
}), interrupt = function(cnd) {
  cat("SIGINT escaped the per-call window; result is inconclusive.\n")
}, finally = options(old))
if (is.null(status)) status <- 2L
quit(status = status, save = "no")
