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
  make_repo <- function(name) {
    repo <- file.path(workspace, name)
    dir.create(repo)
    paths <- c("tools/review-ready.R", "tools/review-ready-check-lib.R",
      "tools/review-ready-progress.R", "tools/review-ready-progress-reporter.R",
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
      encodeString(account, quote = '"'), text, fixed = TRUE)
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
    run <- processx::process$new(rscript,
      c("tools/review-ready.R", "--scope", scope, "--reason", "fixture explicit judgment"),
      wd = repo, env = c(env, extra), stdout = "|", stderr = "2>&1",
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
      stop("Command timed out after ", timeout / 1000,
        " seconds. Child output:\n", collected_output(run), call. = FALSE)
    }
    run$wait()
    output <- paste0(collected_output(run), run$read_all_output())
    captured[[as.character(run$get_pid())]] <- NULL
    if (!identical(run$get_exit_status(), status)) stop(output, call. = FALSE)
    output
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
  flooded <- finish(start(repo, extra = c(FIXTURE_LARGE_OUTPUT = "true")), timeout = 10000L)
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
    FIXTURE_INSPECTION_MODE = "exited", FIXTURE_RACE_INJECTED = race_marker)), timeout = 10000L)
  unlink(file.path(bin, "git"))
  expect(file.exists(race_marker) && grepl("Five mandatory checks passed", exited, fixed = TRUE),
    "an exited leader's generic process-inspection error does not fail the command")
  shared <- file.path(account, ".local", "state", "marginplyr", "review-ready-heavy.lock")
  dir.create(dirname(shared), recursive = TRUE)
  lock <- filelock::lock(shared, timeout = 0)
  on.exit(if (!is.null(lock)) filelock::unlock(lock), add = TRUE)
  # Capacity is occupied, yet source-only work and failed preflight finish.
  finish(start(repo))
  file.create(file.path(repo, "jarl-failure"))
  git(repo, "add", ".")
  git(repo, "commit", "--quiet", "-m", "failing preflight")
  failed <- finish(start(repo, "full"), 1L)
  expect(grepl("jarl failed", failed, fixed = TRUE) &&
    !grepl("Waiting for heavy", failed, fixed = TRUE), "preflight fails before admission")
  git(repo, "rm", "--quiet", "jarl-failure")
  git(repo, "commit", "--quiet", "-m", "repair preflight")
  submitted <- git(repo, "rev-parse", "HEAD")
  waiter <- start(repo, "full")
  deadline <- Sys.time() + 30
  repeat {
    drain_output(waiter, 100L)
    observed <- collected_output(waiter)
    if (grepl("Waiting for heavy", observed, fixed = TRUE)) break
    if (!waiter$is_alive() || Sys.time() > deadline) stop(observed)
  }
  # Change HEAD only after the submission has reached the occupied slot.
  writeLines("later", file.path(repo, "inst", "revision.txt"))
  git(repo, "add", ".")
  git(repo, "commit", "--quiet", "-m", "later revision must not be checked")
  filelock::unlock(lock)
  lock <- NULL
  checked <- finish(waiter)
  expect(grepl(submitted, checked, fixed=TRUE) &&
    grepl("Starting 4 test processes.", checked, fixed=TRUE) &&
    grepl("Full package check passed", checked, fixed=TRUE),
    "automatic admission checks submitted source once with four workers")
  wait_for <- function(run, predicate, label, timeout = 45) {
    deadline <- Sys.time() + timeout
    repeat {
      drain_output(run, 50L)
      if (predicate()) return(invisible(NULL))
      if (!run$is_alive() || Sys.time() > deadline) {
        stop(paste(label, collected_output(run)), call.=FALSE)
      }
      Sys.sleep(0.05)
    }
  }
  waiting <- function(run) {
    wait_for(run, function() {
      grepl("Waiting for heavy", collected_output(run), fixed=TRUE)
    }, "waiter reached admission")
    collected_output(run)
  }
  marker <- file.path(workspace, "ready-after-output")
  buffered <- start(repo, extra = c(FIXTURE_LARGE_OUTPUT = "true", FIXTURE_OUTPUT_READY = marker))
  wait_for(buffered, function() file.exists(marker), "marker follows large child output", 10)
  buffered_output <- finish(buffered)
  expect(grepl("Five mandatory checks passed", buffered_output, fixed = TRUE) &&
    length(gregexpr("fixture stdout payload", buffered_output, fixed = TRUE)[[1L]]) == 65536L,
    "marker wait retains every child output record until terminal completion")
  # A cancelled waiter remains cancelled after the occupied slot is released.
  lock <- filelock::lock(shared, timeout=0)
  cancelled <- start(repo,"full")
  waiting(cancelled)
  cancelled$signal(2L)
  cancelled_output <- finish(cancelled,130L)
  filelock::unlock(lock)
  lock <- NULL
  expect(grepl("cancelled",cancelled_output,fixed=TRUE) &&
    !grepl("capacity acquired",cancelled_output,fixed=TRUE), "cancelled waiter never starts")

  git(repo,"reset","--hard",submitted)
  clone <- file.path(workspace,"separate-clone")
  command("git",c("clone","--quiet","--no-local",repo,clone))
  linked <- file.path(workspace,"linked-worktree")
  git(repo,"worktree","add","--quiet","--detach",linked,submitted)
  owned <- function(name) c(FIXTURE_RELEASE=file.path(workspace,paste0(name,"-release")),
    FIXTURE_READY=file.path(workspace,paste0(name,"-ready")))
  first_env <- owned("clone")
  second_env <- owned("linked")
  lock <- filelock::lock(shared,timeout=0)
  first <- start(clone,"full",first_env)
  second <- start(linked,"full",second_env)
  waiting(first)
  waiting(second)
  filelock::unlock(lock)
  lock <- NULL
  deadline <- Sys.time()+60
  while (!any(file.exists(c(first_env[["FIXTURE_READY"]],second_env[["FIXTURE_READY"]])))) {
    drain_output(first)
    drain_output(second)
    expect(Sys.time()<deadline && first$is_alive() && second$is_alive(), "one shared invocation starts")
    Sys.sleep(0.05)
  }
  clone_first <- file.exists(first_env[["FIXTURE_READY"]])
  leader <- if(clone_first) first else second
  follower <- if(clone_first) second else first
  leader_env <- if(clone_first) first_env else second_env
  follower_env <- if(clone_first) second_env else first_env
  expect(!file.exists(follower_env[["FIXTURE_READY"]]), "clone and linked worktree cannot check concurrently")
  file.create(leader_env[["FIXTURE_RELEASE"]])
  finish(leader)
  wait_for(follower,function() file.exists(follower_env[["FIXTURE_READY"]]), "waiter starts automatically after release",60)
  file.create(follower_env[["FIXTURE_RELEASE"]])
  finish(follower)

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
  inspected <- finish(inspecting, 1L, 10000L)
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
  finish(start(repo))
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
