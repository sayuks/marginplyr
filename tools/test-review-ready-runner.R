# Public-command contracts use disposable Git repositories and real OS locks.
# External tools, process inspection and the account-home adapter use replacements.
local({
  root <- normalizePath(".", winslash = "/")
  workspace <- tempfile("review-ready-process-")
  dir.create(workspace)
  workspace <- normalizePath(workspace, winslash="/")
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  library <- file.path(workspace, "library")
  dir.create(library)
  rscript <- file.path(R.home("bin"), "Rscript")
  runs <- list()
  captured <- new.env(parent = emptyenv())
  on.exit(for (run in runs) {
    if (run$is_alive()) run$kill_tree()
    run$wait()
  }, add = TRUE, after = FALSE)
  expect <- function(value, label) {
    if (!isTRUE(value)) stop(label, call. = FALSE)
  }
  command <- function(program, args, wd = workspace, env = NULL) {
    processx::run(program, args, wd = wd, env = env, error_on_status = TRUE)
  }
  git <- function(repo, ...) trimws(command("git", c(...), repo)$stdout)
  install_tool <- function(name, code, exports) {
    path <- file.path(workspace, name)
    dir.create(file.path(path, "R"), recursive = TRUE)
    writeLines(c(paste("Package:", name), "Version: 99.0.0", "Title: External Tool Fixture",
      "Description: A disposable external tool for process contracts.", "License: GPL-3",
      'Authors@R: person("A", "B", email="a@example.com", role=c("aut", "cre"))'),
      file.path(path, "DESCRIPTION"))
    writeLines(paste0("export(", exports, ")"), file.path(path, "NAMESPACE"))
    writeLines(code, file.path(path, "R", "tool.R"))
    command(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", "--no-byte-compile",
      paste0("--library=", library), path))
  }
  install_tool("spelling", c(
    'spell_check_package <- function(...) {',
    '  cat("fixture spelling executed\\n")',
    '  if (Sys.getenv("FIXTURE_LARGE_OUTPUT") == "true") cat(strrep("fixture stdout payload\\n", 65536L))',
    '  ready <- Sys.getenv("FIXTURE_OUTPUT_READY")',
    '  if (nzchar(ready)) writeLines("ready", ready)',
    '  if (file.exists("spelling-failure")) return(data.frame(word="failure"))',
    '  baseline_ready <- Sys.getenv("FIXTURE_BASELINE_READY")',
    '  baseline_release <- Sys.getenv("FIXTURE_BASELINE_RELEASE")',
    '  if (nzchar(baseline_ready)) writeLines("ready", baseline_ready)',
    '  while (nzchar(baseline_release) && !file.exists(baseline_release)) Sys.sleep(0.05)',
    '  data.frame()', '}'
  ), "spell_check_package")
  install_tool("pkgload", c(
    'load_all <- function(...) { options(fixture.loaded=TRUE); cat("fixture package loaded\\n") }'
  ), "load_all")
  install_tool("lintr", c(
    'lint_package <- function(...) {',
    '  stopifnot(isTRUE(getOption("fixture.loaded")), Sys.getenv("LINTR_ERROR_ON_LINT") == "true")',
    '  cat("fixture package-aware lintr executed\\n")', '}'
  ), "lint_package")
  bin <- file.path(workspace, "bin")
  dir.create(bin)
  writeLines(c("#!/bin/sh", "echo 'fixture jarl executed'", "test ! -f jarl-failure"),
    file.path(bin, "jarl"))
  Sys.chmod(file.path(bin, "jarl"), "0755")
  account <- file.path(workspace, "account")
  dir.create(account)
  env <- c(R_LIBS = paste(c(library, .libPaths()), collapse = .Platform$path.sep),
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    TESTTHAT_PARALLEL = "false")
  make_repo <- function(name, account_home = account) {
    repo <- file.path(workspace, name)
    dir.create(repo)
    paths <- c("tools/review-ready.R", "tools/review-ready-check-lib.R", "tools/review-ready-fifo.R",
      "tools/review-ready-progress.R", "tools/review-ready-progress-reporter.R",
      "tools/review-ready-timing.R", "tools/review-ready-priority.R",
      "tools/dependency-requirements.R", "tools/required-test-suggests.R",
      ".github/scripts/cran-note-policy.R", "inst/suggests/guard.R")
    for (relative in paths) {
      dir.create(dirname(file.path(repo, relative)), recursive = TRUE, showWarnings = FALSE)
      expect(file.copy(file.path(root, relative), file.path(repo, relative)), relative)
    }
    runner <- file.path(repo, "tools/review-ready-check-lib.R")
    text <- readLines(runner)
    expect(sum(grepl('path.expand(paste0("~", Sys.info()[["user"]]))', text, fixed=TRUE)) == 1L,
      "fixture must replace exactly one account-home adapter before invoking the runner")
    text <- sub('path.expand(paste0("~", Sys.info()[["user"]]))',
      encodeString(account_home, quote = '"'), text, fixed = TRUE)
    writeLines(text, runner)
    writeLines(c("Package: fixture", "Version: 0.0.1", "Title: Process Fixture",
      "Description: Exercises the repository command in a disposable package.",
      'Authors@R: person("A", "B", email="a@example.com", role=c("aut", "cre"))',
      "License: GPL-3", "Suggests: testthat, stats, processx", "Config/testthat/edition: 3",
      "Config/marginplyr/cran-status: unpublished",
      "Config/Needs/review: spelling, pkgload, lintr, processx, ps, filelock, rcmdcheck"),
      file.path(repo, "DESCRIPTION"))
    writeLines(character(), file.path(repo, "NAMESPACE"))
    writeLines(c("/.review-ready/"), file.path(repo, ".gitignore"))
    writeLines(c("^\\.review-ready$", "^tools$", "^\\.github$"), file.path(repo, ".Rbuildignore"))
    dir.create(file.path(repo, "tests", "testthat"), recursive = TRUE)
    expect(file.copy(file.path(root, "tests/testthat/helper-review-timing.R"),
      file.path(repo, "tests/testthat/helper-review-timing.R")), "common timing helper")
    writeLines('optional_suggest_spec <- function() list(stats = TRUE)',
      file.path(repo, "tests", "testthat", "helper-optional-backends.R"))
    writeLines(gsub("marginplyr", "fixture", readLines(file.path(root, "tests/testthat.R")), fixed = TRUE),
      file.path(repo, "tests", "testthat.R"))
    writeLines('test_that("committed revision", expect_identical(readLines(system.file("revision.txt", package="fixture")), "submitted"))',
      file.path(repo, "tests", "testthat", "test-revision.R"))
    for (i in 1:3) writeLines(paste0('test_that("additional ', i, '", expect_true(TRUE))'),
      file.path(repo, "tests", "testthat", paste0("test-additional-", i, ".R")))
    writeLines(c(
      'test_that("owned child", {',
      '  release <- Sys.getenv("FIXTURE_RELEASE")',
      '  if (nzchar(release)) local({',
      '    child <- processx::process$new(file.path(R.home("bin"),"Rscript"), c("-e","Sys.sleep(600)"), cleanup_tree=TRUE, supervise=TRUE)',
      '    on.exit({ if(child$is_alive()) child$kill_tree(); child$wait() })',
      '    writeLines(c(as.character(child$get_pid()), tempdir(), tools::R_user_dir("fixture","cache")), Sys.getenv("FIXTURE_READY"))',
      '    while (!file.exists(release)) Sys.sleep(0.05)',
      '  })',
      '  expect_true(TRUE)', '})'
    ), file.path(repo, "tests", "testthat", "test-owned-child.R"))
    writeLines("submitted", file.path(repo, "inst", "revision.txt"))
    git(repo, "init", "--quiet")
    git(repo, "config", "user.email", "fixture@example.com")
    git(repo, "config", "user.name", "Fixture")
    git(repo, "add", ".")
    git(repo, "commit", "--quiet", "-m", "fixture")
    repo
  }
  start <- function(repo, scope = "source", extra = character()) {
    cat("Runner fixture: ", basename(repo), " --scope ", scope, "\n", sep = "")
    flush.console()
    values <- Sys.getenv()
    values[names(env)] <- env
    values[names(extra)] <- extra
    run <- processx::process$new(rscript,
      c("tools/review-ready.R", "--scope", scope, "--reason", "fixture explicit judgment"),
      wd = repo, env = values, stdout = "|", stderr = "2>&1",
      cleanup_tree = TRUE, supervise = TRUE)
    runs[[length(runs) + 1L]] <<- run
    run
  }
  drain_output <- function(run, timeout = 0L) {
    run$poll_io(timeout)
    key <- as.character(run$get_pid())
    captured[[key]] <- c(captured[[key]], run$read_output())
    invisible(NULL)
  }
  collected_output <- function(run) {
    paste0(captured[[as.character(run$get_pid())]], collapse = "")
  }
  fail_command <- function(run, label, output = collected_output(run)) {
    cat(output, "\n", sep = "")
    stop(label, call. = FALSE)
  }
  finish <- function(run, status = 0L, timeout = 120000L) {
    deadline <- Sys.time() + timeout / 1000
    repeat {
      drain_output(run, 50L)
      if (!run$is_alive() || Sys.time() >= deadline) break
    }
    timed_out <- run$is_alive()
    if (timed_out) {
      run$kill_tree()
      run$wait()
      fail_command(run, paste0("Command timed out after ", timeout / 1000,
        " seconds; expected status ", status, ", actual status ", run$get_exit_status(), "."))
    }
    run$wait()
    output <- paste0(collected_output(run), run$read_all_output())
    captured[[as.character(run$get_pid())]] <- NULL
    if (!identical(run$get_exit_status(), status)) {
      fail_command(run, paste0("Command failed: expected status ", status,
        ", actual status ", run$get_exit_status(), "."), output)
    }
    output
  }
  result_record <- function(output) {
    lines <- strsplit(output, "\n", fixed = TRUE)[[1L]]
    directory <- sub("^Artifacts: ", "", grep("^Artifacts: ", lines, value = TRUE))
    expect(length(directory) == 1L, "command names exactly one artifact directory")
    readRDS(file.path(directory, "result.rds"))
  }
  repo <- make_repo("first")
  output <- finish(start(repo))
  expect(all(vapply(c("Test dependency syntax", "Required test Suggests",
    "fixture spelling executed", "fixture jarl executed", "fixture package loaded",
    "fixture package-aware lintr executed",
    "fixture explicit judgment", git(repo, "rev-parse", "HEAD")),
    grepl, logical(1), x = output, fixed = TRUE)), "source executes five checks and records its scope/reason/SHA")
  expect(grepl("Package build/check, the complete suite and snapshots omitted: fixture explicit judgment",
    output, fixed = TRUE), "source explicitly records package build/check, complete suite and snapshots omission")
  expect(!grepl("Waiting for heavy", output, fixed = TRUE), "source never requests admission")
  # Only the dependency's infinite-wait return is replaced; guards use real locks.
  null_lock_profile <- file.path(workspace, "null-lock.R")
  writeLines(c(
    'local({',
    '  namespace <- asNamespace("filelock")',
    '  lock <- filelock::lock',
    '  unlockBinding("lock", namespace)',
    '  assign("lock", function(path, exclusive = TRUE, timeout = Inf) {',
    '    if (is.infinite(timeout) && basename(path) == "review-ready-heavy.lock") return(NULL)',
    '    lock(path, exclusive = exclusive, timeout = timeout)',
    '  }, namespace)',
    '  lockBinding("lock", namespace)',
    '})'
  ), null_lock_profile)
  null_lock <- finish(start(repo, "full", c(R_PROFILE_USER = null_lock_profile)), 1L)
  expect(grepl("Cannot acquire heavy package-check capacity", null_lock, fixed = TRUE) &&
    !grepl("capacity acquired|Source-tarball R CMD check", null_lock) &&
    identical(result_record(null_lock)$status, "failed"),
    "an unexpected infinite-wait timeout fails without starting package work")

  unusable_home <- file.path(workspace, "unusable-account")
  dir.create(file.path(unusable_home, ".local", "state", "marginplyr", "review-ready-heavy.lock"),
    recursive = TRUE)
  unusable_repo <- make_repo("unusable-lock", unusable_home)
  unusable <- finish(start(unusable_repo, "full"), 1L, 30000L)
  expect(grepl("Cannot open lock file", unusable, fixed = TRUE) &&
    !grepl("capacity acquired|Source-tarball R CMD check", unusable) &&
    identical(result_record(unusable)$status, "failed"),
    "an unusable heavy-lock target fails without starting package work")
  flooded <- finish(start(repo, extra = c(FIXTURE_LARGE_OUTPUT = "true")))
  expect(grepl("Five mandatory checks passed", flooded, fixed = TRUE) &&
    length(gregexpr("fixture stdout payload", flooded, fixed = TRUE)[[1L]]) == 65536L,
    "source completes and retains child output larger than a pipe buffer")
  profile <- file.path(root, "tools", "fixtures", "review-ready-ps-errors.R")
  race_marker <- file.path(workspace, "race-injected")
  actual_git <- unname(Sys.which("git"))
  writeLines(c("#!/bin/sh", "sleep 0.2", paste("exec", shQuote(actual_git), '"$@"')),
    file.path(bin, "git"))
  Sys.chmod(file.path(bin, "git"), "0755")
  exited <- finish(start(repo, extra = c(R_PROFILE_USER = profile,
    FIXTURE_INSPECTION_MODE = "exited", FIXTURE_RACE_INJECTED = race_marker)))
  expect(file.exists(race_marker) && grepl("Five mandatory checks passed", exited, fixed = TRUE),
    "an exited leader's generic process-inspection error does not fail the command")
  transient_marker <- file.path(workspace, "transient-race")
  refreshed <- finish(start(repo, extra = c(R_PROFILE_USER = profile,
    FIXTURE_INSPECTION_MODE = "transient", FIXTURE_RACE_INJECTED = transient_marker)))
  unlink(file.path(bin, "git"))
  expect(file.exists(transient_marker) && file.exists(paste0(transient_marker, "-refreshed")) &&
    identical(readLines(transient_marker)[[2L]], "live leader") &&
    grepl("Five mandatory checks passed", refreshed, fixed = TRUE),
    "a transient child-handle ENOENT accepts a fresh valid snapshot while its leader lives")
  shared <- file.path(account, ".local", "state", "marginplyr", "review-ready-heavy.lock")
  dir.create(dirname(shared), recursive = TRUE, showWarnings = FALSE)
  lock <- filelock::lock(shared, timeout = 0)
  on.exit(if (!is.null(lock)) filelock::unlock(lock), add = TRUE)
  # Capacity is occupied, yet source-only work and failed preflight finish.
  queue_state <- file.path(dirname(shared), "review-ready-fifo", "state.rds")
  before_baselines <- readRDS(queue_state)
  finish(start(repo))
  file.create(file.path(repo, "jarl-failure"))
  git(repo, "add", ".")
  git(repo, "commit", "--quiet", "-m", "failing preflight")
  failed <- finish(start(repo, "full"), 1L)
  expect(grepl("jarl failed", failed, fixed = TRUE) &&
    !grepl("Waiting for heavy", failed, fixed = TRUE), "preflight fails before admission")
  expect(identical(readRDS(queue_state), before_baselines), "source and failed baselines never register")
  git(repo, "rm", "--quiet", "jarl-failure")
  git(repo, "commit", "--quiet", "-m", "repair preflight")
  submitted <- git(repo, "rev-parse", "HEAD")
  waiter <- start(repo, "full")
  deadline <- Sys.time() + 30
  repeat {
    drain_output(waiter, 100L)
    observed <- collected_output(waiter)
    if (grepl("Waiting for heavy", observed, fixed = TRUE)) break
    if (!waiter$is_alive() || Sys.time() > deadline) {
      fail_command(waiter, "Full fixture did not reach shared admission.")
    }
  }
  # Observe only the waiting runner, after baseline children have exited.
  waiter_handle <- ps::ps_handle(waiter$get_pid())
  descriptors <- vapply(c(1, 1, 2), function(delay) {
    Sys.sleep(delay)
    drain_output(waiter)
    expect(waiter$is_alive() && !grepl("capacity acquired", collected_output(waiter), fixed = TRUE),
      "occupied capacity keeps the measured runner waiting")
    ps::ps_num_fds(waiter_handle)
  }, integer(1))
  cat("Waiting runner descriptors at 1/2/4 seconds: ", paste(descriptors, collapse = "/"), "\n", sep = "")
  expect(diff(range(descriptors)) <= 2L, "heavy admission waiting keeps descriptor use bounded")
  # Change HEAD only after the submission has reached the occupied slot.
  writeLines("later", file.path(repo, "inst", "revision.txt"))
  git(repo, "add", ".")
  git(repo, "commit", "--quiet", "-m", "later revision must not be checked")
  filelock::unlock(lock)
  lock <- NULL
  checked <- finish(waiter)
  expect(grepl(submitted, checked, fixed=TRUE) &&
    grepl("Starting 4 test processes.", checked, fixed=TRUE) &&
    length(gregexpr("Source-tarball R CMD check --as-cran", checked, fixed = TRUE)[[1L]]) == 1L &&
    grepl("Full package check passed", checked, fixed=TRUE),
    "automatic admission checks submitted source once with four workers")
  timing_files <- list.files(file.path(repo, ".review-ready", "runs"),
    "^evidence[.]rds$", recursive = TRUE, full.names = TRUE)
  expect(length(timing_files) == 1L, "only full scope produces timing evidence")
  timing <- readRDS(timing_files[[1L]])
  expect(isTRUE(timing$eligible) && identical(timing$sha, submitted) &&
    timing$actual_workers == 4L && nrow(timing$files) == 5L && all(timing$files$eligible),
    "terminal full evidence identifies the pinned source and every worker file")
  wait_for <- function(run, predicate, label, timeout = 45) {
    deadline <- Sys.time() + timeout
    repeat {
      drain_output(run, 50L)
      if (predicate()) return(invisible(NULL))
      if (!run$is_alive() || Sys.time() > deadline) {
        actual <- if (run$is_alive()) "running" else run$get_exit_status()
        fail_command(run, paste0(label, ": readiness condition unmet; actual status ", actual, "."))
      }
    }
  }
  waiting <- function(run) {
    wait_for(run, function() {
      grepl("Waiting for heavy", collected_output(run), fixed=TRUE)
    }, "waiter reached admission")
    collected_output(run)
  }
  finalizing_repo <- make_repo("timing-finalization")
  recorder <- file.path(finalizing_repo, "tools", "review-ready-timing.R")
  text <- readLines(recorder)
  boundary <- "review_ready_timing_finish <- function(directory, terminal, transcript) {"
  expect(sum(text == boundary) == 1L, "fixture replaces one timing-output boundary")
  text[text == boundary] <- paste(boundary, paste(c(
    '  ready <- Sys.getenv("FIXTURE_FINAL_READY")',
    '  if (identical(terminal$status, "passed") && nzchar(ready)) {',
    '    writeLines("ready", ready)',
    '    repeat Sys.sleep(0.05)',
    '  }'
  ), collapse = "\n"), sep = "\n")
  writeLines(text, recorder)
  git(finalizing_repo, "add", ".")
  git(finalizing_repo, "commit", "--quiet", "-m", "timing output barrier")
  final_marker <- file.path(workspace, "final-output-ready")
  finalizing <- start(finalizing_repo, "full", c(FIXTURE_FINAL_READY = final_marker))
  wait_for(finalizing, function() file.exists(final_marker), "timing output reached terminal boundary")
  finalizing$signal(2L)
  final_output <- finish(finalizing, 130L, 30000L)
  expect(identical(result_record(final_output)$status, "cancelled"),
    "cancellation during timing output replaces the provisional success result")
  evidence <- list.files(file.path(finalizing_repo, ".review-ready", "runs"),
    "^evidence[.]rds$", recursive = TRUE, full.names = TRUE)
  expect(length(evidence) == 1L && identical(readRDS(evidence[[1L]])$eligible, FALSE),
    "cancelled timing finalization cannot leave eligible evidence")
  marker <- file.path(workspace, "ready-after-output")
  buffered <- start(repo, extra = c(FIXTURE_LARGE_OUTPUT = "true", FIXTURE_OUTPUT_READY = marker))
  wait_for(buffered, function() file.exists(marker), "marker follows large child output")
  buffered_output <- finish(buffered)
  expect(grepl("Five mandatory checks passed", buffered_output, fixed = TRUE) &&
    length(gregexpr("fixture stdout payload", buffered_output, fixed = TRUE)[[1L]]) == 65536L,
    "marker wait retains every child output record until terminal completion")
  # A cancelled waiter remains cancelled after the occupied slot is released.
  lock <- filelock::lock(shared, timeout=0)
  cancelled <- start(repo,"full")
  waiting(cancelled)
  Sys.sleep(0.5)
  cancelled$signal(2L)
  cancelled_output <- finish(cancelled,130L,30000L)
  filelock::unlock(lock)
  lock <- NULL
  expect(grepl("cancelled",cancelled_output,fixed=TRUE) &&
    !grepl("capacity acquired|Source-tarball R CMD check",cancelled_output) &&
    identical(result_record(cancelled_output)$status, "cancelled") && !cancelled$is_alive(),
    "cancelled waiter records cancellation and never starts after release")

  git(repo,"reset","--hard",submitted)
  clone <- file.path(workspace,"separate-clone")
  command("git",c("clone","--quiet","--no-local",repo,clone))
  linked <- file.path(workspace,"linked-worktree")
  git(repo,"worktree","add","--quiet","--detach",linked,submitted)
  owned <- function(name) c(FIXTURE_RELEASE=file.path(workspace,paste0(name,"-release")),
    FIXTURE_READY=file.path(workspace,paste0(name,"-ready")))
  sequence_of <- function(output) {
    lines <- strsplit(output, "\n", fixed = TRUE)[[1L]]
    as.numeric(sub("^FIFO accepted sequence ([0-9]+)[.].*$", "\\1",
      grep("^FIFO accepted sequence ", lines, value = TRUE)))
  }
  registered <- function(run, ahead) {
    wait_for(run, function() {
      grepl(paste0(": ", ahead, " earlier live waiter(s)."), collected_output(run), fixed = TRUE)
    }, "observable FIFO registration/count barrier")
    sequence_of(collected_output(run))
  }
  another <- file.path(workspace, "another-worktree")
  git(repo, "worktree", "add", "--quiet", "--detach", another, submitted)
  first_env <- owned("clone")
  second_env <- owned("linked")
  third_env <- owned("another")
  lock <- filelock::lock(shared, timeout = 0)
  first <- start(clone, "full", first_env)
  b <- registered(first, 0)
  second <- start(linked, "full", second_env)
  c_sequence <- registered(second, 1)
  # A live suspended head keeps its position, including arrivals at release.
  first$signal(tools::SIGSTOP)
  filelock::unlock(lock)
  lock <- NULL
  third <- start(another, "full", third_env)
  d <- registered(third, 2)
  expect(b < c_sequence && c_sequence < d, "acceptance order is B, C, D across clone/worktrees")
  descriptors <- vapply(c(1, 1, 2), function(delay) {
    Sys.sleep(delay)
    drain_output(second)
    expect(!any(file.exists(c(first_env[["FIXTURE_READY"]], second_env[["FIXTURE_READY"]],
      third_env[["FIXTURE_READY"]]))) &&
      !grepl("capacity acquired", collected_output(second), fixed = TRUE),
      "a suspended live head prevents successors from taking free capacity")
    ps::ps_num_fds(ps::ps_handle(second$get_pid()))
  }, integer(1))
  cat("FIFO follower descriptors at 1/2/4 seconds: ", paste(descriptors, collapse = "/"), "\n", sep = "")
  expect(diff(range(descriptors)) <= 2L, "FIFO metadata/liveness polling keeps descriptors bounded")
  first$signal(tools::SIGCONT)
  wait_for(first, function() file.exists(first_env[["FIXTURE_READY"]]), "B admitted automatically", 60)
  expect(!any(file.exists(c(second_env[["FIXTURE_READY"]], third_env[["FIXTURE_READY"]]))),
    "only B owns heavy capacity")
  file.create(first_env[["FIXTURE_RELEASE"]])
  first_output <- finish(first)
  wait_for(second, function() file.exists(second_env[["FIXTURE_READY"]]), "C follows B", 60)
  expect(!file.exists(third_env[["FIXTURE_READY"]]), "D cannot overlap or overtake C")
  file.create(second_env[["FIXTURE_RELEASE"]])
  finish(second)
  wait_for(third, function() file.exists(third_env[["FIXTURE_READY"]]), "D follows C", 60)
  file.create(third_env[["FIXTURE_RELEASE"]])
  finish(third)
  expect(grepl("elapsed wait [0-9]+[.][0-9]+ seconds", first_output) &&
    !grepl("ETA", first_output, fixed = TRUE), "admission reports actual elapsed wait without an ETA")

  # A middle cancellation and dead head leave only the live successor.
  lock <- filelock::lock(shared, timeout = 0)
  dead_head <- start(clone, "full")
  registered(dead_head, 0)
  middle <- start(linked, "full")
  middle_sequence <- registered(middle, 1)
  survivor <- start(another, "full")
  registered(survivor, 2)
  middle$signal(2L)
  middle_output <- finish(middle, 130L)
  registered(survivor, 1)
  ps::ps_kill(ps::ps_handle(dead_head$get_pid()))
  registered(survivor, 0)
  dead_head$wait()
  survivor$signal(2L)
  finish(survivor, 130L)
  # Killing a middle waiter also lets its successor advance without head death.
  head <- start(clone, "full")
  registered(head, 0)
  dead_middle <- start(linked, "full")
  registered(dead_middle, 1)
  tail <- start(another, "full")
  registered(tail, 2)
  ps::ps_kill(ps::ps_handle(dead_middle$get_pid()))
  registered(tail, 1)
  dead_middle$wait()
  head$signal(2L)
  finish(head, 130L)
  registered(tail, 0)
  tail$signal(2L)
  finish(tail, 130L)
  retry <- start(linked, "full")
  retry_sequence <- registered(retry, 0)
  retry$signal(2L)
  retry_output <- finish(retry, 130L)
  expect(retry_sequence > middle_sequence &&
    !identical(result_record(retry_output)$started_utc, result_record(middle_output)$started_utc),
    "retry has a fresh invocation identity and acceptance sequence")
  filelock::unlock(lock)
  lock <- NULL

  # Launch order cannot outrank successful five-check acceptance.
  lock <- filelock::lock(shared, timeout = 0)
  baseline_ready <- file.path(workspace, "baseline-ready")
  baseline_release <- file.path(workspace, "baseline-release")
  launched_first <- start(clone, "full", c(FIXTURE_BASELINE_READY = baseline_ready,
    FIXTURE_BASELINE_RELEASE = baseline_release))
  wait_for(launched_first, function() file.exists(baseline_ready), "first launch is still in baseline")
  accepted_first <- start(linked, "full")
  earlier_sequence <- registered(accepted_first, 0)
  file.create(baseline_release)
  later_sequence <- registered(launched_first, 1)
  expect(earlier_sequence < later_sequence, "registration after all five checks decides acceptance order")
  launched_first$signal(2L)
  finish(launched_first, 130L)
  accepted_first$signal(2L)
  finish(accepted_first, 130L)
  filelock::unlock(lock)
  lock <- NULL

  # Kill after guarding but before acceptance to leave an interrupted registration.
  orphan_profile <- file.path(workspace, "orphan-profile.R")
  orphan_marker <- file.path(workspace, "orphan-guard")
  writeLines(c(
    'local({',
    '  namespace <- asNamespace("filelock")',
    '  acquire <- filelock::lock',
    '  unlockBinding("lock", namespace)',
    '  assign("lock", function(path, exclusive = TRUE, timeout = Inf) {',
    '    guard <- acquire(path, exclusive = exclusive, timeout = timeout)',
    '    if (startsWith(basename(path), "request-") && !is.null(guard)) {',
    '      writeLines(path, Sys.getenv("FIXTURE_ORPHAN_MARKER"))',
    '      Sys.sleep(600)',
    '    }',
    '    guard',
    '  }, namespace)',
    '  lockBinding("lock", namespace)',
    '})'
  ), orphan_profile)
  orphan <- start(clone, "full", c(R_PROFILE_USER = orphan_profile,
    FIXTURE_ORPHAN_MARKER = orphan_marker))
  wait_for(orphan, function() file.exists(orphan_marker), "guard predates registration")
  orphan_path <- readLines(orphan_marker)[[1L]]
  orphan$signal(9L)
  orphan$wait()
  recovered <- start(clone, "full")
  registered(recovered, 0)
  expect(!file.exists(orphan_path), "an ended interrupted registration's guard is reclaimed")
  recovered$signal(2L)
  finish(recovered, 130L)

  # Corrupt, incompatible and ambiguous state never bypasses FIFO ownership.
  bad_account <- file.path(workspace, "bad-state-account")
  bad_repo <- make_repo("bad-state", bad_account)
  bad_queue <- file.path(bad_account, ".local", "state", "marginplyr", "review-ready-fifo")
  dir.create(bad_queue, recursive = TRUE)
  state_path <- file.path(bad_queue, "state.rds")
  clean_state <- list(version = 1L, counter = 50, requests = list())
  reject_state <- function(pattern) {
    output <- finish(start(bad_repo, "full"), 1L, 30000L)
    expect(grepl(pattern, output, fixed = TRUE) &&
      !grepl("capacity acquired|Source-tarball R CMD check", output) &&
      identical(result_record(output)$status, "failed"), "unsafe FIFO state fails visibly before package work")
  }
  expect(file.symlink(file.path(bad_queue, "missing-state"), state_path), "dangling state fixture")
  reject_state("Unsafe or missing Review-ready FIFO file")
  expect(nzchar(Sys.readlink(state_path)), "a dangling state link is preserved on rejection")
  unlink(state_path)
  writeLines("corrupt", state_path)
  reject_state("Review-ready failed")
  incompatible <- clean_state
  incompatible$version <- 99L
  saveRDS(incompatible, state_path)
  reject_state("Incompatible Review-ready FIFO state")
  live_orphan_name <- paste0("request-", strrep("b", 32L), "-", Sys.getpid(), "-",
    sprintf("%.6f", as.numeric(ps::ps_create_time())), ".lock")
  live_orphan_guard <- filelock::lock(file.path(bad_queue, live_orphan_name), timeout = 0)
  on.exit(if (!is.null(live_orphan_guard)) filelock::unlock(live_orphan_guard), add = TRUE)
  live_active <- clean_state
  live_active$requests <- list(list(sequence = 49, guard = live_orphan_name, phase = "active"))
  saveRDS(live_active, state_path)
  reject_state("Review-ready FIFO active ownership conflicts with free capacity")
  saveRDS(clean_state, state_path)
  reject_state("Ambiguous live unregistered Review-ready FIFO guard")
  expect(identical(readRDS(state_path), clean_state), "an unexplained live guard cannot be bypassed")
  filelock::unlock(live_orphan_guard)
  live_orphan_guard <- NULL
  unlink(file.path(bad_queue, live_orphan_name))
  # A PID with a different OS creation time is not the owner of that registration.
  ambiguous_name <- paste0("request-", strrep("a", 32L), "-", Sys.getpid(), "-",
    sprintf("%.6f", as.numeric(ps::ps_create_time()) - 100), ".lock")
  ambiguous_guard <- filelock::lock(file.path(bad_queue, ambiguous_name), timeout = 0)
  on.exit(if (!is.null(ambiguous_guard)) filelock::unlock(ambiguous_guard), add = TRUE)
  ambiguous <- clean_state
  ambiguous$requests <- list(list(sequence = 49, guard = ambiguous_name, phase = "waiting"))
  saveRDS(ambiguous, state_path)
  reject_state("Ambiguous Review-ready FIFO guard ownership")
  expect(identical(readRDS(state_path), ambiguous), "ambiguous ownership preserves the possible earlier waiter")
  filelock::unlock(ambiguous_guard)
  ambiguous_guard <- NULL
  # The same inactive guard can now be positively acquired and safely reclaimed.
  after_ambiguity <- start(bad_repo, "full")
  registered(after_ambiguity, 0)
  after_ambiguity$signal(2L)
  finish(after_ambiguity, 130L)
  expect(!file.exists(file.path(bad_queue, ambiguous_name)), "inactive owned guards are reclaimed with positive evidence")
  saveRDS(ambiguous, state_path)
  reject_state("Unsafe or missing Review-ready FIFO file")
  saveRDS(clean_state, state_path)
  unlink(state_path)
  reject_state("Missing Review-ready FIFO state")
  saveRDS(clean_state, state_path)
  unlink(file.path(bad_queue, "metadata.lock"))
  dir.create(file.path(bad_queue, "metadata.lock"))
  reject_state("Unsafe or missing Review-ready FIFO file")

  # Ordinary cancellation must collect owned test work before releasing the slot.
  active_env <- owned("ordinary-cancel")
  running <- start(repo,"full",active_env)
  wait_for(running,function() file.exists(active_env[["FIXTURE_READY"]]),"owned child started",60)
  observed <- readLines(active_env[["FIXTURE_READY"]])
  child <- ps::ps_handle(as.integer(observed[[1L]]))
  expect(all(startsWith(observed[2:3],file.path(repo,".review-ready","runs"))),
    "child temporary/cache outputs stay in invocation artifacts")
  running$signal(2L)
  deadline <- Sys.time()+30
  repeat {
    drain_output(running)
    probe <- filelock::lock(shared,timeout=0)
    if (!is.null(probe)) {
      expect(!ps::ps_is_running(child), "owned child exits before shared slot is released")
      filelock::unlock(probe)
      break
    }
    expect(Sys.time()<deadline,"ordinary cancellation releases capacity after cleanup")
    Sys.sleep(0.02)
  }
  finish(running,130L)

  inspection_env <- owned("live-inspection")
  inspection_gate <- file.path(workspace, "inspection-gate")
  inspection_blocked <- file.path(workspace, "inspection-blocked")
  file.create(inspection_gate)
  inspecting <- start(repo, "full", c(inspection_env, R_PROFILE_USER = profile,
    FIXTURE_INSPECTION_MODE = "live", FIXTURE_INSPECTION_GATE = inspection_gate,
    FIXTURE_INSPECTION_BLOCKED = inspection_blocked))
  wait_for(inspecting, function() file.exists(inspection_blocked) &&
    grepl("Waiting for owned-process cleanup", collected_output(inspecting), fixed = TRUE),
    "live inspection failure reaches guarded cleanup")
  child <- ps::ps_handle(as.integer(readLines(inspection_env[["FIXTURE_READY"]])[[1L]]))
  leader <- ps::ps_handle(as.integer(readLines(inspection_blocked)[[1L]]))
  probe <- filelock::lock(shared, timeout = 0)
  if (!is.null(probe)) filelock::unlock(probe)
  expect(is.null(probe) && inspecting$is_alive() &&
    ps::ps_is_running(leader) && ps::ps_is_running(child),
    "live inspection errors keep capacity occupied while owned children remain")
  unlink(inspection_gate)
  inspected <- finish(inspecting, 1L)
  expect(grepl("No such file or directory", inspected, fixed = TRUE) &&
    !grepl("Full package check passed", inspected, fixed = TRUE) && !ps::ps_is_running(child),
    "live inspection failure is preserved after owned child cleanup")
  probe <- filelock::lock(shared, timeout = 0)
  expect(!is.null(probe), "capacity is released after the unknown live work is collected")
  filelock::unlock(probe)

  # Observe processx's actual abrupt-parent cleanup without promising exclusion.
  abrupt_env <- owned("abrupt-cancel")
  abrupt <- start(repo,"full",abrupt_env)
  wait_for(abrupt,function() file.exists(abrupt_env[["FIXTURE_READY"]]),"abrupt fixture child started",60)
  descendants <- ps::ps_children(ps::ps_handle(abrupt$get_pid()),recursive=TRUE)
  abrupt$signal(9L)
  abrupt$wait()
  Sys.sleep(1)
  survived <- vapply(descendants,ps::ps_is_running,logical(1))
  cat("Abrupt processx cleanup on ",Sys.info()[["sysname"]],": ",sum(!survived),"/",length(descendants),
    " observed descendants stopped after one second; delayed descendants are an accepted limit.\n",sep="")
  if(any(survived)) { ps::ps_kill(descendants[survived]); expect(all(ps::ps_wait(descendants[survived],timeout=10000L)),"abrupt fixture survivors collected") }

  # Native R test timeouts and check warnings fail; every ordinary NOTE is shown.
  writeLines('test_that("timeout", { Sys.sleep(30); expect_true(TRUE) })',
    file.path(repo,"tests","testthat","test-timeout.R"))
  git(repo,"add",".")
  git(repo,"commit","--quiet","-m","timeout fixture")
  timed <- finish(start(repo,"full",c(`_R_CHECK_TESTS_ELAPSED_TIMEOUT_`="2")),1L)
  expect(grepl("elapsed time limit|timed out|TIMEOUT",timed,ignore.case=TRUE),
    "native R timeout is visible and fails terminal verdict")
  git(repo,"reset","--hard",submitted)
  dir.create(file.path(repo,"R"))
  writeLines("fixture_export <- function() TRUE",file.path(repo,"R","export.R"))
  writeLines("export(fixture_export)",file.path(repo,"NAMESPACE"))
  writeLines("nonstandard top-level file",file.path(repo,"unexpected.txt"))
  git(repo,"add",".")
  git(repo,"commit","--quiet","-m","warning and NOTE fixture")
  warned <- finish(start(repo,"full"),1L)
  expect(grepl("1 WARNING(s)",warned,fixed=TRUE) &&
    grepl("review required",warned,fixed=TRUE) && grepl("unexpected.txt",warned,fixed=TRUE),
    "check warning fails and unclassified NOTE remains visible")
  git(repo,"reset","--hard",submitted)

  # A native worker exit fails once; no retry can turn it into passing evidence.
  writeLines('test_that("native worker exit", quit(status=17L,save="no"))',
    file.path(repo,"tests","testthat","test-native-exit.R"))
  git(repo,"add",".")
  git(repo,"commit","--quiet","-m","native exit fixture")
  failure <- finish(start(repo,"full"),1L)
  expect(grepl("testthat subprocess exited",failure,fixed=TRUE) &&
    length(gregexpr("Source-tarball R CMD check --as-cran",failure,fixed=TRUE)[[1L]])==1L &&
    !grepl("Full package check passed|passed-after-retry|Retrying",failure), "terminal native failure is retained without retry")
  git(repo,"reset","--hard",submitted)

  # The dependency scan and required versions are mechanical pre-admission checks.
  lock <- filelock::lock(shared,timeout=0)
  writeLines('test_that("undeclared", neverdeclaredfixture::missing())',
    file.path(repo,"tests","testthat","test-undeclared.R"))
  git(repo,"add",".")
  git(repo,"commit","--quiet","-m","undeclared dependency fixture")
  rejected <- finish(start(repo,"full"),1L,30000L)
  expect(grepl("undeclared package candidates",rejected,fixed=TRUE) &&
    !grepl("Waiting for heavy",rejected,fixed=TRUE), "undeclared test dependency fails before capacity")
  git(repo,"reset","--hard",submitted)
  description <- readLines(file.path(repo,"DESCRIPTION"))
  writeLines(sub("testthat, stats, processx", "testthat, stats (>= 9999.0.0), processx",description,fixed=TRUE),
    file.path(repo,"DESCRIPTION"))
  git(repo,"add",".")
  git(repo,"commit","--quiet","-m","required version fixture")
  rejected <- finish(start(repo,"full"),1L,30000L)
  expect(grepl("requires usable optional Suggests",rejected,fixed=TRUE) &&
    !grepl("Waiting for heavy",rejected,fixed=TRUE), "unsuitable required Suggests version fails before capacity")
  git(repo,"reset","--hard",submitted)
  filelock::unlock(lock)
  lock <- NULL

  runs_root <- file.path(repo, ".review-ready", "runs")
  metadata <- readRDS(file.path(list.dirs(runs_root, recursive=FALSE)[[1L]], "invocation.rds"))
  expired <- function(name, age, result = "passed") {
    path <- file.path(runs_root, name)
    dir.create(path)
    item <- metadata
    item$started_utc <- format(Sys.time() - age, "%Y-%m-%dT%H:%M:%OS6Z", tz="UTC")
    saveRDS(item, file.path(path, "invocation.rds"))
    file.create(file.path(path, "active.lock"))
    if (!is.null(result)) saveRDS(list(status=result), file.path(path,"result.rds"))
    writeLines("recent log must not renew start", file.path(path,"console.log"))
    path
  }
  week <- 7 * 24 * 60 * 60
  young <- expired("before-threshold", week - 60)
  boundary <- expired("at-threshold", week)
  old_failure <- expired("expired-failure", week + 10, "failed")
  killed <- expired("killed-no-result", week + 10, NULL)
  active <- expired("active-expired", week + 10)
  guard <- filelock::lock(file.path(active,"active.lock"), timeout=0)
  on.exit(if (!is.null(guard)) filelock::unlock(guard), add=TRUE)
  uncertain <- expired("uncertain", week + 10)
  unlink(file.path(uncertain,"active.lock"))
  invalid <- expired("invalid-time", week + 10)
  writeLines("broken", file.path(invalid,"invocation.rds"))
  outside <- file.path(workspace,"other-worktree-data")
  dir.create(outside)
  symlink <- file.path(runs_root,"foreign-symlink")
  expect(file.symlink(outside,symlink), "fixture symlink")
  before_retention <- readRDS(queue_state)
  finish(start(repo))
  expect(identical(readRDS(queue_state), before_retention), "retention preserves acceptance sequencing and live coordination")
  expect(!any(dir.exists(c(boundary,old_failure,killed))) &&
    all(dir.exists(c(young,active,uncertain,invalid,outside))),
    "seven-day start-time expiry protects active, uncertain and foreign data")
  filelock::unlock(guard)
  guard <- NULL
  finish(start(repo))
  expect(!dir.exists(active), "inactive expired run is removed as a unit")
  expect(identical(git(repo,"status","--porcelain=v1","--untracked-files=all"),""),
    "all invocation artifacts are ignored by Git")
  tarballs <- list.files(runs_root,"[.]tar[.]gz$",recursive=TRUE,full.names=TRUE)
  expect(length(tarballs)>0L && !any(grepl("[.]review-ready",utils::untar(tarballs[[1L]],list=TRUE))),
    "source tarball excludes invocation storage")
  foreign <- list.dirs(file.path(linked,".review-ready","runs"),recursive=FALSE)[[1L]]
  foreign_metadata <- readRDS(file.path(foreign,"invocation.rds"))
  foreign_metadata$started_utc <- format(Sys.time()-week-10,"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC")
  saveRDS(foreign_metadata,file.path(foreign,"invocation.rds"))
  finish(start(repo))
  expect(dir.exists(foreign),"retention never sweeps another worktree")
  cat("Source, pinning, clone/worktree exclusion, cancellation, failure and retention contracts passed.\n")
})
