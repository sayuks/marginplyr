# Exercises the real reporter and process boundary with a small disposable
# package. Run from the repository root with the review dependencies installed.
source("tools/review-ready-check-lib.R")
source("tools/review-ready-progress.R")
source("tools/review-ready-timing.R")
source("tools/review-ready-priority.R")

local({
  root <- normalizePath(".", winslash = "/")
  workspace <- tempfile("review-ready-progress-smoke-")
  package <- file.path(workspace, "fixture")
  dir.create(file.path(package, "tests", "testthat"), recursive = TRUE)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  writeLines(c(
    "Package: fixture", "Version: 0.0.1", "Title: Progress Fixture",
    "Description: Exercises live progress during source package checks.",
    'Authors@R: person("A", "B", email="a@example.com", role=c("aut", "cre"))',
    "License: GPL-3", "Suggests: testthat, withr", "Config/testthat/edition: 3"
  ), file.path(package, "DESCRIPTION"))
  writeLines(character(), file.path(package, "NAMESPACE"))
  writeLines(gsub("marginplyr", "fixture", readLines("tests/testthat.R"), fixed = TRUE),
    file.path(package, "tests", "testthat.R"))
  writeLines('test_that("first", expect_true(TRUE))',
    file.path(package, "tests", "testthat", "test-first.R"))
  stopifnot(file.copy("tests/testthat/helper-review-timing.R",
    file.path(package, "tests", "testthat", "helper-review-timing.R")))
  repos <- review_ready_offline_repositories(workspace)
  writeLines('test_that("skipped", skip("fixture all skipped"))',
    file.path(package, "tests", "testthat", "test-skipped.R"))
  # The edition-2 teardown queue exercises cleanup after source_file returns.
  writeLines(c(
    'local({ local_edition(2); teardown({ Sys.sleep(0.15); cat(sprintf("%.9f", as.numeric(Sys.time())), "\\n", file=Sys.getenv("FIXTURE_CLEANUP"), append=TRUE) }) })',
    'test_that("cleanup", expect_snapshot(cat("fixture snapshot\\n")))'
  ), file.path(package, "tests", "testthat", "test-cleanup.R"))
  dir.create(file.path(package, "tests", "testthat", "_snaps"))
  writeLines(c('# cleanup', '', '    Code', '      cat("fixture snapshot\\n")',
    '    Output', '      fixture snapshot'),
    file.path(package, "tests", "testthat", "_snaps", "cleanup.md"))
  for (mode in c("pass", "delayed", "fail", "worker", "teardown", "startup", "unsupported", "shape", "recorder")) {
    fail <- mode %in% c("fail", "worker", "teardown", "startup")
    helper <- file.path(package, "tests", "testthat", "helper-worker-startup.R")
    writeLines(c(
      'Sys.sleep(0.2)',
      'cat(paste("setup", Sys.getpid(), sprintf("%.9f", proc.time()[["elapsed"]]), sep="\\t"), "\\n", file=paste0(Sys.getenv("FIXTURE_LIFECYCLE"), ".", Sys.getpid()), append=TRUE)',
      'withr::defer({ cat(paste("teardown", Sys.getpid(), sprintf("%.9f", proc.time()[["elapsed"]]), sep="\\t"), "\\n", file=paste0(Sys.getenv("FIXTURE_LIFECYCLE"), ".", Sys.getpid()), append=TRUE); Sys.sleep(0.2) }, envir=.GlobalEnv)'
    ), helper)
    if (mode == "startup") writeLines('stop("fixture worker startup failure")', helper)
    expression <- switch(mode,
      pass = 'test_that("second", expect_true(TRUE))',
      delayed = 'test_that("second", expect_true(TRUE))',
      shape = 'test_that("second", expect_true(TRUE))',
      fail = 'test_that("second", expect_true(FALSE))',
      worker = 'test_that("worker exits", quit(status = 17L, save = "no"))',
      teardown = c(
        'withr::defer(stop("fixture teardown failure"), envir = .GlobalEnv)',
        'test_that("second", expect_true(TRUE))'
      ),
      startup = 'test_that("second", expect_true(TRUE))',
      unsupported = 'test_that("second", expect_true(TRUE))',
      recorder = 'test_that("second", expect_true(TRUE))'
    )
    writeLines(expression, file.path(package, "tests", "testthat", "test-second.R"))
    check_dir <- file.path(workspace, mode)
    timing <- file.path(check_dir, "timing")
    metadata <- list(version = 1L, scope = "full", sha = strrep("a", 40L),
      started_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"))
    stopifnot(review_ready_timing_prepare(package, timing, metadata))
    saveRDS(metadata, file.path(check_dir, "invocation.rds"))
    recorder <- file.path(root, "tools", "review-ready-timing.R")
    if (mode == "unsupported") {
      text <- readLines(recorder)
      stopifnot(sum(grepl('identical(version, "3.3.2")', text, fixed = TRUE)) == 1L)
      text <- sub('identical(version, "3.3.2")', 'identical(version, "0.0.0")', text, fixed = TRUE)
      recorder <- file.path(check_dir, "unsupported.R")
      writeLines(text, recorder)
    }
    if (mode == "shape") {
      recorder <- file.path(check_dir, "shape.R")
      text <- readLines(file.path(root, "tools", "review-ready-timing.R"))
      text <- sub('snapshotter <- SubprocessSnapshotReporter$new(snap_dir = "_snaps")',
        'snapshotter <- NULL', text, fixed = TRUE)
      writeLines(text, recorder)
    }
    progress_reporter <- file.path(root, "tools", "review-ready-progress-reporter.R")
    if (mode == "delayed") {
      text <- readLines(progress_reporter)
      text <- sub('        completed_file <<- filename',
        '        Sys.sleep(0.2); cat(sprintf("%.9f", as.numeric(Sys.time())), "\\n", file=Sys.getenv("FIXTURE_REPLAY"), append=TRUE); completed_file <<- filename',
        text, fixed = TRUE)
      progress_reporter <- file.path(check_dir, "delayed-reporter.R")
      writeLines(text, progress_reporter)
    }
    if (mode == "recorder") {
      writeLines(c(
        'local({ p <- Sys.getenv("MARGINPLYR_REVIEW_TIMING"); unlink(list.files(p, full.names=TRUE)); unlink(p, recursive=TRUE); writeLines("blocked", p) })'
      ), helper)
    }
    output <- capture.output({
      result <- review_ready_progress_check(
        package, args = c("--no-manual", "--no-vignettes"), build_args = character(),
        check_dir = check_dir, error_on = "never", repos = repos,
        env = c(
          TESTTHAT_PARALLEL = "true", TESTTHAT_CPUS = "4", NOT_CRAN = "true",
          MARGINPLYR_REVIEW_TIMING = timing,
          MARGINPLYR_REVIEW_TIMING_HELPER = recorder,
          FIXTURE_CLEANUP = file.path(check_dir, "cleanup.txt"),
          FIXTURE_LIFECYCLE = file.path(check_dir, "lifecycle.tsv"),
          FIXTURE_REPLAY = file.path(check_dir, "replay.txt"),
          MARGINPLYR_REVIEW_PROGRESS_REPORTER = progress_reporter
        )
      )
    })
    # This registered fixture uses four real workers in a tiny disposable package.
    stopifnot(
      "Starting 4 test processes." %in% trimws(output),
      identical(result$status == 0L, !fail),
      identical(length(result$errors) == 0L, !fail)
    )
    terminal <- c(metadata, list(status = if (fail) "failed" else "passed",
      heavy_check = if (fail) "failed" else "passed"))
    saveRDS(terminal, file.path(check_dir, "result.rds"))
    final_output <- capture.output(review_ready_timing_finish(timing, terminal,
      review_ready_test_transcript(check_dir)))
    cat(final_output, sep = "\n")
    cat("\n")
    if (mode != "recorder") {
      evidence <- readRDS(file.path(timing, "evidence.rds"))
      if (!identical(evidence$eligible, mode %in% c("pass", "delayed"))) {
        cat(review_ready_test_transcript(check_dir), sep = "\n")
        print(list.files(timing, recursive = TRUE))
        for (p in list.files(timing, "[.]disabled$", full.names = TRUE)) cat(readLines(p), sep = "\n")
      }
      stopifnot(identical(evidence$eligible, mode %in% c("pass", "delayed")))
    }
    if (mode %in% c("pass", "delayed")) {
      stopifnot(evidence$actual_workers == 4L, nrow(evidence$files) == 4L,
        all(evidence$files$valid_pair), sum(evidence$files$eligible) == 3L,
        evidence$files$all_skipped[evidence$files$file == "test-skipped.R"],
        evidence$files$duration[evidence$files$file == "test-cleanup.R"] >= 0.15)
      cleanup <- as.numeric(readLines(file.path(check_dir, "cleanup.txt")))
      end <- subset(evidence$observations, file == "test-cleanup.R" & event == "end")
      stopifnot(length(cleanup) == 1L, end$wall >= cleanup)
    }
    if (mode == "pass") {
      proposal <- review_ready_priority_propose(package, check_dir)
      stopifnot(proposal$eligible, length(proposal$registered) == 3L,
        !"skipped" %in% proposal$registered,
        file.exists(file.path(proposal$directory, "priority.patch")))
      reject <- function(name, edit = function(path) NULL, verdict = terminal,
                         transcript = review_ready_test_transcript(check_dir)) {
        copy <- file.path(check_dir, name)
        dir.create(copy)
        stopifnot(all(file.copy(list.files(timing, full.names = TRUE), copy)))
        unlink(file.path(copy, "evidence.rds"))
        edit(copy)
        invisible(capture.output(review_ready_timing_finish(copy, verdict, transcript)))
        stopifnot(identical(readRDS(file.path(copy, "evidence.rds"))$eligible, FALSE))
      }
      reject("duplicate", function(path) {
        log <- list.files(path, "[.]tsv$", full.names = TRUE)
        log <- log[basename(log) != "file-results.tsv"][[1L]]
        cat(readLines(log)[[1L]], "\n", file = log, append = TRUE)
      })
      reject("incomplete", function(path) {
        log <- list.files(path, "^[0-9]+[.]tsv$", full.names = TRUE)[[1L]]
        writeLines(head(readLines(log), -1L), log)
      })
      reject("truncated", function(path) {
        log <- list.files(path, "^[0-9]+[.]tsv$", full.names = TRUE)[[1L]]
        bytes <- readBin(log, "raw", n = file.info(log)$size)
        writeBin(head(bytes, -1L), log)
      })
      reject("malformed", function(path) {
        log <- list.files(path, "^[0-9]+[.]tsv$", full.names = TRUE)[[1L]]
        cat("end\tunknown.R\t1\tNaN\t-1\n", file = log, append = TRUE)
      })
      reject("count", transcript = "Starting 2 test processes.")
      reject("cancelled", verdict = modifyList(terminal, list(status = "cancelled")))
      reject("stale-sha", verdict = modifyList(terminal, list(sha = strrep("b", 40L))))
      blocked <- file.path(timing, "evidence.rds")
      unlink(blocked)
      dir.create(blocked)
      diagnostic <- capture.output(review_ready_timing_finish(timing, terminal,
        review_ready_test_transcript(check_dir)))
      stopifnot(any(grepl("Review-ready timing disabled:", diagnostic, fixed = TRUE)), dir.exists(blocked))
      unlink(blocked, recursive = TRUE)
    }
    if (mode == "delayed") {
      replay <- as.numeric(readLines(file.path(check_dir, "replay.txt")))
      stopifnot(length(replay) == 4L,
        any(subset(evidence$observations, event == "end")$wall < min(replay)))
    }
    if (mode %in% c("pass", "delayed")) {
      lifecycle <- do.call(rbind, lapply(list.files(check_dir,
        "^lifecycle[.]tsv[.][0-9]+$", full.names = TRUE), function(path) {
          read.delim(path, header = FALSE, col.names = c("phase", "pid", "elapsed"))
        }))
      for (pid in unique(evidence$observations$pid)) {
        interval <- evidence$observations[evidence$observations$pid == pid, ]
        setup <- lifecycle$elapsed[lifecycle$pid == pid & lifecycle$phase == "setup"]
        teardown <- lifecycle$elapsed[lifecycle$pid == pid & lifecycle$phase == "teardown"]
        stopifnot(length(setup) == 1L, length(teardown) == 1L,
          all(interval$elapsed >= setup), all(interval$elapsed <= teardown))
      }
    }
    if (mode %in% c("unsupported", "shape", "recorder")) {
      transcript <- review_ready_test_transcript(check_dir)
      stopifnot(any(grepl("Review-ready timing disabled:", c(transcript, final_output), fixed = TRUE)))
    }
    unlink(helper)
    if (mode == "worker") {
      stopifnot(
        any(grepl("testthat subprocess exited in file 'test-second.R'", output, fixed = TRUE)),
        any(grepl("R session crashed with exit code 17", output, fixed = TRUE))
      )
      next
    }
    if (mode == "teardown") {
      stopifnot(
        any(grepl("At least one parallel worker failed to run teardown", output, fixed = TRUE)),
        any(grepl("fixture teardown failure", output, fixed = TRUE))
      )
      next
    }
    if (mode == "startup") {
      stopifnot(
        any(grepl("testthat subprocess failed to start", output, fixed = TRUE)),
        any(grepl("fixture worker startup failure", output, fixed = TRUE))
      )
      next
    }
    events <- readLines(file.path(check_dir, "test-progress.tsv"))
    stopifnot(
      identical(events[[1L]], "total\t4"),
      setequal(events[startsWith(events, "done\t")],
        paste0("done\t", c("test-first.R", "test-second.R", "test-skipped.R", "test-cleanup.R"))),
      identical(tail(events, 1L), "finished")
    )
    logs <- list.files(result$checkdir, "testthat[.]Rout", recursive = TRUE,
      full.names = TRUE)
    tally <- if (fail) "[ FAIL 1 | WARN 0 | SKIP 1 | PASS 2 ]" else {
      "[ FAIL 0 | WARN 0 | SKIP 1 | PASS 3 ]"
    }
    stopifnot(length(logs) == 1L, tally %in% trimws(readLines(logs)))
  }
  cat("Real progress reporter and process checks passed.\n")
})
