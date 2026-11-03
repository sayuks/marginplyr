# Separate-worker checkpoint experiments; callers supply their case matrix and
# deadline. Worker observations precede retry, caller decisions and disposal.
supervisor_json <- function(value, path) {
  jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
}

# Stop and reap the owned worker on errors and interrupts as well as success.
supervisor_stop <- function(worker) {
  suspendInterrupts({
    if (worker$is_alive()) worker$kill_tree()
    worker$wait(timeout = 10000L)
    if (worker$is_alive() || is.null(worker$get_exit_status())) {
      stop("Owned worker termination was not confirmed")
    }
  })
}

# Require the flushed notification from this worker and the case's side effects
# before sending a signal. SQLite statement checkpoints follow the real call.
supervisor_checkpoint <- function(notification, worker, case, kind) {
  if (!identical(notification$pid, worker$get_pid())) stop("Checkpoint worker identity mismatch")
  if (kind == "competing-conditions") {
    if (!identical(notification$operation, case$operation)) stop("Checkpoint worker identity mismatch")
    if (case$operation == "summary" && !identical(as.numeric(notification$effects), c(2, 5))) {
      stop("Summary effects not established before signal")
    }
    if (case$operation == "sqlite" &&
        !(isTRUE(notification$inserted) && isTRUE(notification$rolled_back))) {
      stop("SQLite insert/rollback not established before signal")
    }
  } else {
    if (!identical(notification$checkpoint, case$checkpoint)) stop("Checkpoint worker identity mismatch")
    if (!isTRUE(notification$hit)) stop("SQLite checkpoint not established before signal")
    if (case$checkpoint != "before" &&
        !(is.character(notification$savepoint) && length(notification$savepoint) == 1L && nzchar(notification$savepoint))) {
      stop("SQLite savepoint not established before signal")
    }
    if (case$checkpoint %in% c("insert", "analyze", "result", "rollback", "cleanup_release", "release") &&
        !isTRUE(notification$inserted)) stop("SQLite insert not established before signal")
    if (case$checkpoint %in% c("rollback", "cleanup_release") && !isTRUE(notification$rolled_back)) {
      stop("SQLite rollback not established before signal")
    }
  }
}

# Retain one case's terminal outcome even when checkpoint or supervisor work
# fails. A verdict cannot establish success without a successful worker exit.
supervisor_case <- function(root, directory, case, kind, timeout) {
  worker <- NULL
  delivered <- NULL
  failure <- NULL
  verdict <- NULL
  on.exit(if (!is.null(worker)) supervisor_stop(worker), add = TRUE)
  elapsed <- function() unname(proc.time()[["elapsed"]])
  deadline <- elapsed() + timeout
  tryCatch({
    worker <- processx::process$new(file.path(R.home("bin"), "Rscript"),
      c(file.path(root, "tools", kind, "worker.R"), root, directory), wd = root,
      stdout = file.path(directory, "worker.log"), stderr = "2>&1",
      cleanup_tree = TRUE, supervise = TRUE)
    if (kind == "competing-conditions" || case$mode == "sigint") {
      reached <- file.path(directory, "reached.json")
      while (!file.exists(reached) && worker$is_alive() && elapsed() < deadline) Sys.sleep(0.01)
      if (!file.exists(reached)) stop("Worker never confirmed checkpoint")
      notification <- jsonlite::read_json(reached, simplifyVector = TRUE)
      supervisor_checkpoint(notification, worker, case, kind)
      if (case$mode == "sigint") {
        if (!worker$is_alive()) stop("Worker exited before checkpoint confirmation")
        if (!isTRUE(worker$signal(2L))) stop("SIGINT delivery failed")
        delivered <- list(pid = worker$get_pid(), signal = "SIGINT",
                          time_ns = as.numeric(Sys.time()) * 1e9)
        supervisor_json(delivered, file.path(directory, "delivery.json"))
        if (!file.create(file.path(directory, "resume"))) stop("Cannot resume reached worker")
      }
    }
    remaining <- max(0, deadline - elapsed())
    worker$wait(timeout = as.integer(remaining * 1000))
    if (worker$is_alive()) stop("Worker deadline exceeded")
    if (worker$get_exit_status() != 0L) stop(paste("Worker failed with exit status", worker$get_exit_status()))
    path <- file.path(directory, "verdict.json")
    if (!file.exists(path)) stop("Worker verdict missing")
    verdict <- jsonlite::read_json(path, simplifyVector = TRUE)
    if (!isTRUE(verdict$passed)) stop("Worker verdict failed")
  }, error = function(cnd) { failure <<- conditionMessage(cnd) },
     interrupt = function(cnd) { failure <<- "Supervisor interrupted" })
  if (!is.null(worker)) supervisor_stop(worker)
  row <- list(case = basename(directory), configuration = case,
              returncode = if (!is.null(worker)) worker$get_exit_status(),
              delivered = delivered, verdict = verdict)
  if (!is.null(failure)) {
    row$failure <- failure
    row$verdict <- list(passed = FALSE)
  }
  row
}

# Run only a clean committed snapshot and retain source identity with evidence.
supervise_interruption <- function(root, evidence, cases, kind, timeout) {
  root <- normalizePath(root, mustWork = TRUE)
  git <- function(args) {
    result <- processx::run("git", args, wd = root, timeout = 10000)
    trimws(result$stdout)
  }
  snapshot <- git(c("rev-parse", "HEAD"))
  if (nzchar(git(c("status", "--porcelain")))) stop("Acceptance requires a clean committed source snapshot")
  if (dir.exists(evidence) && length(list.files(evidence, all.files = TRUE, no.. = TRUE))) {
    stop("Evidence directory must be empty")
  }
  if (!dir.exists(evidence) && !dir.create(evidence, recursive = TRUE)) stop("Cannot create evidence directory")
  evidence <- normalizePath(evidence, mustWork = TRUE)
  if (identical(evidence, root) || startsWith(evidence, paste0(root, .Platform$file.sep))) {
    stop("Evidence directory must be outside the checkout")
  }
  paths <- c(paste0("tools/", kind, "/run.R"), paste0("tools/", kind, "/worker.R"),
             "tools/interruption-supervisor.R", "tests/testthat/helper-sqlite-interruption.R")
  manifest <- list(snapshot = snapshot, supervisor_pid = Sys.getpid(),
    scripts = stats::setNames(lapply(file.path(root, paths), digest::digest,
                                    algo = "sha256", file = TRUE), paths))
  supervisor_json(manifest, file.path(evidence, "manifest.json"))
  summaries <- list()
  for (index in seq_along(cases)) {
    case <- cases[[index]]
    name <- if (kind == "competing-conditions") paste(case$operation, case$mode, sep = "-") else paste(case$mode, case$checkpoint, sep = "-")
    directory <- file.path(evidence, sprintf("%03d-%s", index - 1L, name))
    if (!dir.create(directory)) stop("Cannot create case directory")
    supervisor_json(case, file.path(directory, "case.json"))
    row <- supervisor_case(root, directory, case, kind, timeout)
    summaries[[index]] <- row
    # Keep the top-level JSON sequence even when only one case completed.
    supervisor_json(summaries, file.path(evidence, "summary.json"))
    cat(sprintf("%d/%d %s: exit %s\n", index, length(cases), basename(directory), row$returncode))
    if (!is.null(row$failure)) stop(paste(row$failure, "Failed:", file.path(directory, "worker.log")))
  }
  invisible(summaries)
}
