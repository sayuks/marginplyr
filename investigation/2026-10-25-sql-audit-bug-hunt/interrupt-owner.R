# Investigated: 2026-10-25. A diagnostic harness, not a package test.
# Run from the repository root in a disposable R process:
#   Rscript investigation/2026-10-25-sql-audit-bug-hunt/interrupt-owner.R
#   Rscript investigation/2026-10-25-sql-audit-bug-hunt/interrupt-owner.R --native
# Both modes require POSIX signals. The default fixture signals once immediately
# before ownership is released. --native changes no runtime package binding and
# searches a bounded scheduling window; absence of a leak is inconclusive.
# Exit 0: asserted invariant held (targeted only); 1: invariant failed;
# 2: native stress did not reproduce the leak; other errors are harness failures.

if (!identical(.Platform$OS.type, "unix")) {
  stop("This diagnostic requires a POSIX platform with SIGINT and fork support.")
}
args <- commandArgs(trailingOnly = TRUE)
if (!identical(args, character()) && !identical(args, "--native")) {
  stop("Usage: Rscript interrupt-owner.R [--native]")
}
pkgload::load_all(quiet = TRUE)
# The accessor cannot distinguish an active owner from a completed empty record.
# Inspect this snapshot's private state only for that ownership assertion.
record_state <- utils::getFromNamespace("sent_queries", "marginplyr")

# Checks the next independent call after the interrupted inspection. Its SQL
# needs no database connection and should replace any earlier local record.
next_independent_call <- function() {
  summarize_with_margins(
    dbplyr::tbl_lazy(data.frame(v = 1)), answer = sum(v)
  )
  rows <- last_sent_queries()
  active <- isTRUE(record_state$active)
  cat("Next independent SQL call: rows=", nrow(rows),
      "; active=", active, "\n", sep = "")
  !active && identical(rows$purpose, "result") && nrow(rows) == 1L
}

# Installs a one-shot write fixture, leaving the restoration function unchanged.
# An interrupt-protected restore can complete the write before delivery, so the
# fixture also permits checking a repair that protects inside that function.
targeted <- function() {
  state <- record_state
  active <- isTRUE(state$active)
  armed <- TRUE
  fired <- FALSE
  if (exists("active", envir = state, inherits = FALSE)) {
    rm("active", envir = state)
  }
  makeActiveBinding("active", function(value) {
    if (missing(value)) {
      return(active)
    }
    if (armed && identical(value, FALSE)) {
      armed <<- FALSE
      fired <<- TRUE
      tools::pskill(Sys.getpid(), signal = 2L)
      Sys.sleep(0.001)
    }
    active <<- value
    invisible(NULL)
  }, state)
  on.exit({
    rm("active", envir = state)
    state$active <- active
  }, add = TRUE)

  # The handler is installed before opening the interrupt-delivery window.
  first <- tryCatch(
    allowInterrupts(inspect_grouping(data.frame(g = 1))),
    interrupt = identity
  )
  cat("Targeted fixture: fired=", fired,
      "; interrupted=", inherits(first, "interrupt"),
      "; active_after_exit=", isTRUE(state$active), "\n", sep = "")
  if (!fired || !inherits(first, "interrupt")) {
    stop("Targeted fixture did not deliver its intended interrupt.")
  }
  released <- !isTRUE(state$active)
  independent <- next_independent_call()
  if (!released || !independent) {
    cat("REPRODUCED: interrupted restoration left an active owner.\n")
    return(1L)
  }
  cat("Targeted invariant held; native scheduling was not established here.\n")
  0L
}

# Sends SIGINT only to this R process, using a bounded child that is stopped and
# reaped before any post-interrupt check. SIGKILL ensures inherited interrupt
# suspension cannot keep the sender alive during cleanup.
native <- function() {
  target_pid <- Sys.getpid()
  sender <- parallel::mcparallel({
    Sys.sleep(0.1)
    deadline <- Sys.time() + 12
    while (Sys.time() < deadline) {
      tools::pskill(target_pid, signal = 2L)
      Sys.sleep(0.0003)
    }
    NULL
  }, silent = TRUE, mc.set.seed = FALSE)
  stop_sender <- function() {
    if (!is.null(sender)) {
      tools::pskill(sender$pid, signal = 9L)
      suppressWarnings(parallel::mccollect(sender, wait = TRUE))
      sender <<- NULL
    }
    invisible(NULL)
  }
  on.exit(stop_sender(), add = TRUE)
  deadline <- Sys.time() + 12
  caught <- 0L
  iterations <- 0L
  leaked <- FALSE
  input <- data.frame(g = 1)
  for (i in seq_len(10000L)) {
    if (Sys.time() >= deadline) {
      break
    }
    iterations <- i
    outcome <- tryCatch(
      allowInterrupts(inspect_grouping(input)), interrupt = identity
    )
    caught <- caught + as.integer(inherits(outcome, "interrupt"))
    if (isTRUE(record_state$active)) {
      leaked <- TRUE
      break
    }
  }
  stop_sender()
  cat("Native stress: iterations=", iterations, "; caught_interrupts=", caught,
      "; active_after_exit=", leaked, "\n", sep = "")
  if (!leaked) {
    cat("NOT REPRODUCED: bounded native stress is not proof of a fix.\n")
    return(2L)
  }
  independent <- next_independent_call()
  cat("REPRODUCED: native SIGINT left an active owner; next_call_ok=",
      independent, "\n", sep = "")
  1L
}

old <- options(marginplyr.audit_sql = TRUE)
exit_status <- NULL
# Suspend across setup and cleanup, while each public call explicitly opens its
# own window. A queued signal at the outer exit is caught after sender cleanup.
tryCatch(
  suspendInterrupts({
    exit_status <- if (identical(args, "--native")) native() else targeted()
  }),
  interrupt = function(cnd) {
    cat("Deferred SIGINT delivered after cleanup.\n")
    if (is.null(exit_status) && identical(args, "--native")) {
      cat("NOT REPRODUCED: SIGINT escaped the per-call window; ",
          "cleanup completed, but this run is inconclusive.\n", sep = "")
      exit_status <<- 2L
    }
  },
  finally = options(old)
)
if (is.null(exit_status)) {
  if (identical(args, "--native")) {
    cat("NOT REPRODUCED: delivery escaped before a comparison was completed.\n")
    quit(status = 2L)
  }
  stop("Diagnostic exited before producing a result.")
}
quit(status = exit_status)
