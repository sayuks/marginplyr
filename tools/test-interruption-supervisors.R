# Exercise both supervisors with disposable worker executables at the Rscript
# boundary. Real package checkpoint acceptance is a separate manual run.
local({
  root <- normalizePath(".")
  workspace <- tempfile("supervisor-fixtures-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  expect <- function(ok, detail) if (!isTRUE(ok)) stop(detail, call. = FALSE)
  # The supervisors collect their workers. run()'s unconditional tree sweep
  # races with processx's exiting supervision helper on this Mac.
  command <- function(cmd, args, wd) processx::run(cmd, args, wd = wd,
    error_on_status = FALSE, timeout = 120000)
  interrupt_supervisor <- function(args, repo, evidence) {
    run <- processx::process$new(rscript, args, wd = repo, stdout = "|", stderr = "|",
                                cleanup_tree = TRUE, supervise = TRUE)
    on.exit({ if (run$is_alive()) run$kill_tree(); run$wait(timeout = 10000L) }, add = TRUE)
    deadline <- Sys.time() + 10
    repeat {
      pids <- list.files(evidence, pattern = "^pid$", recursive = TRUE)
      if (length(pids)) break
      expect(run$is_alive() && Sys.time() < deadline, "Worker did not start before supervisor interrupt")
      Sys.sleep(0.01)
    }
    expect(run$signal(2L), "Cannot interrupt supervisor")
    run$wait(timeout = 10000L)
    expect(!run$is_alive(), "Interrupted supervisor did not finish")
    list(status = run$get_exit_status(), stdout = run$read_all_output(), stderr = run$read_all_error())
  }
  for (kind in c("competing-conditions", "sqlite-interruption")) {
    repo <- file.path(workspace, kind)
    result <- command("git", c("clone", "--quiet", "--shared", root, repo), root)
    expect(result$status == 0L, result$stderr)
    for (path in c("tools/interruption-supervisor.R", paste0("tools/", kind, "/run.R"))) {
      expect(file.copy(file.path(root, path), file.path(repo, path), overwrite = TRUE), path)
    }
    # Shorten only the command's deadline in this committed disposable source.
    entry <- file.path(repo, "tools", kind, "run.R")
    entry_source <- readLines(entry)
    for (mode in c("success", "missing", "invalid", "identity", "effects", "early", "timeout", "after_checkpoint", "supervisor", "supervisor_interrupt", "verdict")) {
      timeout <- if (mode %in% c("timeout", "after_checkpoint")) 1L else 10L
      writeLines(sub("timeout = (30|45)", paste("timeout =", timeout), entry_source), entry)
      worker <- c(
        'args <- commandArgs(TRUE); directory <- args[[2L]]',
        'writeLines(as.character(Sys.getpid()), file.path(directory, "pid"))',
        'case <- jsonlite::read_json(file.path(directory, "case.json"), simplifyVector = TRUE)',
        sprintf('mode <- "%s"', mode),
        'if (mode == "early") stop("early worker failure")',
        'if (mode == "missing") quit(status = 0L)',
        'if (mode %in% c("timeout", "supervisor_interrupt")) Sys.sleep(60)',
        'notification <- list(pid = Sys.getpid(), operation = case$operation, checkpoint = case$checkpoint, effects = c(2,5), inserted = TRUE, rolled_back = TRUE, hit = TRUE, savepoint = "fixture")',
        'if (mode == "identity") notification$pid <- 1L',
        'if (mode == "effects") { notification$effects <- 0; notification$hit <- FALSE }',
        'pending <- file.path(directory,"reached.pending")',
        'jsonlite::write_json(notification, pending, auto_unbox=TRUE, null="null")',
        'if (mode == "invalid") writeLines("{invalid", pending)',
        'stopifnot(file.rename(pending, file.path(directory,"reached.json")))',
        'if (mode == "supervisor" && case$mode == "sigint") dir.create(file.path(directory,"delivery.json"))',
        'if (mode %in% c("invalid","identity","effects") || (mode == "supervisor" && case$mode == "sigint")) Sys.sleep(60)',
        'if (case$mode == "sigint") tryCatch({ while (!file.exists(file.path(directory,"resume"))) Sys.sleep(.01) }, interrupt=function(cnd) NULL)',
        'if (mode == "after_checkpoint") Sys.sleep(60)',
        'if (mode != "verdict") jsonlite::write_json(list(passed=TRUE), file.path(directory,"verdict.json"), auto_unbox=TRUE)'
      )
      writeLines(worker, file.path(repo, "tools", kind, "worker.R"))
      for (args in list(c("config", "user.name", "Supervisor fixture"),
                        c("config", "user.email", "fixture@example.com"),
                        c("add", "-A"), c("commit", "--allow-empty", "-qm", mode))) {
        result <- command("git", args, repo)
        expect(result$status == 0L, result$stderr)
      }
      evidence <- file.path(workspace, paste(kind, mode, sep = "-"))
      args <- c(file.path("tools", kind, "run.R"), evidence)
      result <- if (mode == "supervisor_interrupt") interrupt_supervisor(args, repo, evidence) else command(rscript, args, repo)
      output <- paste(result$stdout, result$stderr)
      expect(result$status == if (mode == "success") 0L else 1L, output)
      summary <- jsonlite::read_json(file.path(evidence, "summary.json"))
      expect(length(summary) > 0L, "Missing summary")
      if (mode != "success") {
        row <- tail(summary, 1L)[[1L]]
        expect(!is.null(row$failure) && !isTRUE(row$verdict$passed), output)
        fragment <- switch(mode, missing = "never confirmed checkpoint", invalid = "invalid",
          identity = "identity mismatch", effects = "not established before signal",
          early = "never confirmed checkpoint", timeout = "never confirmed checkpoint",
          after_checkpoint = "deadline exceeded",
          supervisor_interrupt = "Supervisor interrupted",
          supervisor = "cannot open", verdict = "verdict missing")
        expect(grepl(fragment, row$failure, fixed = TRUE), output)
        if (mode %in% c("missing", "invalid", "identity", "effects", "early", "timeout", "supervisor_interrupt")) {
          expect(is.null(row$delivered), "Invalid checkpoint received SIGINT")
        }
      }
      pid_files <- list.files(evidence, pattern = "^pid$", recursive = TRUE, full.names = TRUE)
      for (path in pid_files) {
        pid <- as.integer(readLines(path))
        alive <- tryCatch(ps::ps_is_running(ps::ps_handle(pid)), error = function(cnd) FALSE)
        expect(!alive, paste("Owned worker remains", pid))
      }
      cat(kind, mode, "passed\n")
    }
  }
})
