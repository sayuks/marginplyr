# Exercise both supervisors with disposable worker executables at the Rscript
# boundary. Real package checkpoint acceptance is a separate manual run.
local({
  root <- normalizePath(".")
  workspace <- tempfile("supervisor-fixtures-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  expect <- function(ok, detail) if (!isTRUE(ok)) stop(detail, call. = FALSE)
  command <- function(cmd, args, wd) processx::run(cmd, args, wd = wd,
    error_on_status = FALSE, timeout = 120000, cleanup_tree = TRUE)
  for (kind in c("competing-conditions", "sqlite-interruption")) {
    repo <- file.path(workspace, kind)
    result <- command("git", c("clone", "--quiet", "--shared", root, repo), root)
    expect(result$status == 0L, result$stderr)
    for (path in c("tools/interruption-supervisor.R", paste0("tools/", kind, "/run.R"))) {
      expect(file.copy(file.path(root, path), file.path(repo, path), overwrite = TRUE), path)
    }
    # Shorten only the command's deadline in this committed disposable source.
    entry <- file.path(repo, "tools", kind, "run.R")
    writeLines(sub("timeout = (30|45)", "timeout = 1", readLines(entry)), entry)
    for (mode in c("success", "missing", "invalid", "identity", "effects", "early", "timeout", "after_checkpoint", "supervisor", "verdict")) {
      worker <- c(
        'args <- commandArgs(TRUE); directory <- args[[2L]]',
        'writeLines(as.character(Sys.getpid()), file.path(directory, "pid"))',
        'case <- jsonlite::read_json(file.path(directory, "case.json"), simplifyVector = TRUE)',
        sprintf('mode <- "%s"', mode),
        'if (mode == "early") stop("early worker failure")',
        'if (mode == "missing") quit(status = 0L)',
        'if (mode == "timeout") Sys.sleep(60)',
        'notification <- list(pid = Sys.getpid(), operation = case$operation, checkpoint = case$checkpoint, effects = c(2,5), inserted = TRUE, rolled_back = TRUE, hit = TRUE, savepoint = "fixture")',
        'if (mode == "identity") notification$pid <- 1L',
        'if (mode == "effects") { notification$effects <- 0; notification$hit <- FALSE }',
        'jsonlite::write_json(notification, file.path(directory,"reached.json"), auto_unbox=TRUE, null="null")',
        'if (mode == "invalid") writeLines("{invalid", file.path(directory,"reached.json"))',
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
      result <- command(rscript, c(file.path("tools", kind, "run.R"), evidence), repo)
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
          supervisor = "cannot open", verdict = "verdict missing")
        expect(grepl(fragment, row$failure, fixed = TRUE), output)
        if (mode %in% c("missing", "invalid", "identity", "effects", "early", "timeout")) {
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
