# Exercises the proposal entry against one invocation and native Git adoption.
source("tools/review-ready-timing.R")
source("tools/review-ready-priority.R")
source("tools/review-ready-check-lib.R")
source("tools/review-ready-progress.R")

# CI checks the tracked list as well as the disposable proposal fixtures.
tracked_priority <- review_ready_priority_read(readLines("DESCRIPTION"))
tracked_files <- testthat::find_test_scripts("tests/testthat", full.names = FALSE)
tracked_dispatch <- testthat::find_test_scripts("tests/testthat", full.names = FALSE,
  start_first = tracked_priority)
stopifnot(!anyDuplicated(tracked_dispatch), setequal(tracked_files, tracked_dispatch))

local({
  workspace <- tempfile("priority-validation-")
  dir.create(file.path(workspace, "tests", "testthat"), recursive = TRUE)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  entry <- normalizePath("tools/verify-test-priority.R", mustWork = FALSE)
  check <- function() processx::run(file.path(R.home("bin"), "Rscript"), entry,
    wd = workspace, error_on_status = FALSE)
  description <- c("Package: fixture", "Version: 0.0.1")
  write_priority <- function(priority) writeLines(review_ready_priority_description(description, priority),
    file.path(workspace, "DESCRIPTION"))
  writeLines('stop("metadata validation must not execute tests")',
    file.path(workspace, "tests", "testthat", "test-a.R"))
  writeLines('testthat::test_that("skipped", testthat::skip("all skipped"))',
    file.path(workspace, "tests", "testthat", "test-skip.R"))
  writeLines('stop("metadata validation must not execute helpers")',
    file.path(workspace, "tests", "testthat", "helper-fixture.R"))
  write_priority(c("skip", "a"))
  valid <- check()
  stopifnot(valid$status == 0L, grepl("Test priority registration passed", valid$stdout, fixed = TRUE))
  before_append <- testthat::find_test_scripts(file.path(workspace, "tests", "testthat"),
    full.names = FALSE, start_first = "skip")
  after_append <- testthat::find_test_scripts(file.path(workspace, "tests", "testthat"),
    full.names = FALSE, start_first = c("skip", "a"))
  stopifnot(identical(before_append, c("test-skip.R", "test-a.R")), identical(before_append, after_append))
  for (case in list(
    list(priority = "skip", diagnostic = "missing registrations: a"),
    list(priority = c("skip", "a", "deleted"), diagnostic = "obsolete registrations: deleted"),
    list(priority = c("skip", "a", "a"), diagnostic = "a (positions 2, 3)"),
    list(priority = c("skip", "*"), diagnostic = "invalid or empty entry"),
    list(priority = c("skip", ""), diagnostic = "invalid or empty entry"),
    list(priority = character(), diagnostic = "missing registrations: a, skip")
  )) {
    write_priority(case$priority)
    before <- tools::md5sum(file.path(workspace, "DESCRIPTION"))
    rejected <- check()
    stopifnot(rejected$status == 1L, grepl(case$diagnostic, rejected$stderr, fixed = TRUE),
      identical(tools::md5sum(file.path(workspace, "DESCRIPTION")), before))
  }
  write_priority(c("skip", "a"))
  file.rename(file.path(workspace, "tests", "testthat", "test-a.R"),
    file.path(workspace, "tests", "testthat", "test-renamed.R"))
  renamed <- check()
  stopifnot(renamed$status == 1L, grepl("missing registrations: renamed", renamed$stderr, fixed = TRUE),
    grepl("obsolete registrations: a", renamed$stderr, fixed = TRUE))
  write_priority(c("skip", "renamed"))
  stopifnot(check()$status == 0L)
  unlink(file.path(workspace, "tests", "testthat", "test-renamed.R"))
  deleted <- check()
  stopifnot(deleted$status == 1L, grepl("obsolete registrations: renamed", deleted$stderr, fixed = TRUE))
  write_priority("skip")
  stopifnot(check()$status == 0L)
})

local({
  workspace <- tempfile("review-ready-priority-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  expect <- function(value, label) if (!isTRUE(value)) stop(label, call. = FALSE)
  command <- function(program, args, wd = workspace, env = NULL) {
    processx::run(program, args, wd = wd, env = env, error_on_status = TRUE)
  }
  git <- function(root, ...) trimws(command("git", c(...), root)$stdout)
  fixture <- function(priority, durations, ineligible = character()) {
    root <- tempfile("fixture-", tmpdir = workspace)
    dir.create(file.path(root, "tests", "testthat"), recursive = TRUE)
    description <- c("Package: fixture", "Version: 0.0.1", "Title: Priority Fixture",
      "Description: Exercises native dispatch with committed priority.",
      'Authors@R: person("A", "B", email="a@example.com", role=c("aut", "cre"))',
      "License: GPL-3", "Suggests: testthat", "Config/testthat/edition: 3")
    writeLines(review_ready_priority_description(description, priority), file.path(root, "DESCRIPTION"))
    writeLines(character(), file.path(root, "NAMESPACE"))
    writeLines(c('library(testthat)', 'library(fixture)', 'test_check("fixture")'),
      file.path(root, "tests", "testthat.R"))
    for (name in names(durations)) {
      writeLines(c(
        paste0('cat("done\\n", file=file.path(Sys.getenv("FIXTURE_COMPLETIONS"), "', name, '"), append=TRUE)'),
        if (name %in% ineligible) 'test_that("fixture", skip("all skipped"))' else {
          'test_that("fixture", expect_true(TRUE))'
        }), file.path(root, "tests", "testthat", paste0("test-", name, ".R")))
    }
    git(root, "init", "-q")
    git(root, "config", "user.email", "fixture@example.com")
    git(root, "config", "user.name", "Fixture")
    git(root, "add", ".")
    git(root, "commit", "-qm", "Measured fixture")
    run <- tempfile("run-", tmpdir = workspace)
    timing <- file.path(run, "timing")
    metadata <- list(version = 1L, sha = git(root, "rev-parse", "HEAD"),
      started_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"),
      scope = "full", reason = "Priority proposal fixture")
    expect(review_ready_timing_prepare(root, timing, metadata), "prepare timing")
    saveRDS(metadata, file.path(run, "invocation.rds"))
    terminal <- c(metadata, list(repository_contracts = "passed", status = "passed", heavy_check = "passed"))
    saveRDS(terminal, file.path(run, "result.rds"))
    files <- testthat::find_test_scripts(file.path(root, "tests", "testthat"), full.names = FALSE)
    for (i in seq_along(files)) {
      duration <- durations[[sub("[.]R$", "", sub("^test-", "", files[[i]]))]]
      pid <- (i - 1L) %% 8L + 1L
      write(paste(c("start", "end"), files[[i]], pid,
        c(1000, 1000 + duration), c(0, duration), sep = "\t"),
        file.path(timing, paste0(pid, ".tsv")), append = TRUE)
      saveRDS(list(pid = pid, supported_testthat = "3.3.2",
        session = utils::sessionInfo(), boundary = "queue_task"),
        file.path(timing, paste0(pid, ".worker.rds")))
    }
    skipped <- files %in% paste0("test-", ineligible, ".R")
    writeLines(paste(files, as.integer(skipped), as.integer(!skipped), sep = "\t"),
      file.path(timing, "file-results.tsv"))
    evidence <- review_ready_timing_evidence(timing, terminal, "Starting 8 test processes.")
    saveRDS(evidence, file.path(timing, "evidence.rds"))
    list(root = root, run = run, timing = timing)
  }
  propose <- function(fixture) review_ready_priority_propose(fixture$root, fixture$run)
  set_priority <- function(fixture, priority) {
    path <- file.path(fixture$root, "DESCRIPTION")
    writeLines(review_ready_priority_description(readLines(path), priority), path)
  }
  proposal_cli <- function(fixture) {
    dir.create(file.path(fixture$root, "tools"), showWarnings = FALSE)
    for (name in c("propose-test-priority.R", "review-ready-priority.R", "review-ready-timing.R")) {
      expect(file.copy(file.path("tools", name), file.path(fixture$root, "tools", name), overwrite = TRUE),
        "copy proposal command")
    }
    processx::run(file.path(R.home("bin"), "Rscript"),
      c("tools/propose-test-priority.R", "--run", fixture$run), wd = fixture$root, error_on_status = FALSE)
  }
  successful <- fixture(c("old", "new"), c(old = 5, new = 30))
  proposal <- propose(successful)
  expect(proposal$eligible && identical(names(proposal$models), c("native", "current", "candidate", "selected")) &&
    file.exists(file.path(proposal$directory, "proposal.rds")),
    "successful runner results with passed repository contracts generate priority proposals")
  expect(proposal_cli(successful)$status == 0L, "explicit recalculation accepts successful runner results")
  dir.create(file.path(successful$run, "check"))
  writeLines("Starting 8 test processes.", file.path(successful$run, "check", "testthat.Rout"))
  automatic <- capture.output(finish_review_ready_timing(successful$root,
    list(directory = successful$run, metadata = readRDS(file.path(successful$run, "invocation.rds")))))
  expect(any(startsWith(automatic, "Review-ready priority proposal: ")),
    "automatic full-run finalization generates a proposal from successful runner results")
  incomplete <- fixture("old", c(old = 5, new = 30))
  rejected <- proposal_cli(incomplete)
  expect(rejected$status == 1L && grepl("missing registrations: new", rejected$stdout, fixed = TRUE),
    "proposal command requires registration before measurement and never inserts missing files")
  reject <- function(edit, pattern) {
    f <- fixture(c("old", "new", "tiny", "skip"), c(old = 5, new = 30, tiny = 0.1, skip = 100), "skip")
    edit(f)
    result <- propose(f)
    expect(!result$eligible && grepl(pattern, result$reason, fixed = TRUE), pattern)
  }

  historical <- fixture(c("old", "new", "old"), c(old = 5, new = 30, tiny = 0.1))
  retained_paths <- list.files(historical$run, recursive = TRUE, full.names = TRUE)
  retained_hashes <- tools::md5sum(retained_paths)
  set_priority(historical, c("old", "new", "tiny"))
  historical_output <- capture.output({
    recovered <- propose(historical)
  })
  expect(recovered$eligible && identical(recovered$native, c("old", "new", "tiny")),
    "historical literal duplicates recover the measured native first-occurrence order")
  expect(any(grepl("old (positions 1, 3)", historical_output, fixed = TRUE)),
    "historical normalization reports the duplicate and its positions")
  expect(identical(tools::md5sum(retained_paths), retained_hashes),
    "recovery preserves original raw evidence, metadata and terminal outcome")
  expect(file.exists(file.path(recovered$directory, "proposal.rds")) &&
    any(grepl("old (positions 1, 3)", readLines(file.path(recovered$directory, "explanation.md")), fixed = TRUE)),
    "recovered proposal artifacts record historical normalization")
  for (invalid in list(c("old", "old"), c("old", "*"), c("old", ""))) {
    set_priority(historical, invalid)
    bad_description <- readLines(file.path(historical$root, "DESCRIPTION"))
    rejected <- propose(historical)
    expect(!rejected$eligible && grepl("current DESCRIPTION", rejected$reason, fixed = TRUE) &&
      identical(readLines(file.path(historical$root, "DESCRIPTION")), bad_description) &&
      identical(tools::md5sum(retained_paths), retained_hashes),
      "a valid historical recovery never silently repairs invalid current metadata")
  }
  for (invalid in list(c("old", "*", "old"), c("old", "", "old"), c("old", ""))) {
    malformed <- fixture(invalid, c(old = 5, new = 30))
    set_priority(malformed, "old")
    rejected <- propose(malformed)
    expect(!rejected$eligible && grepl("archived DESCRIPTION", rejected$reason, fixed = TRUE),
      "historical tolerance rejects globs and empty entries including a trailing comma")
  }

  merged <- fixture(letters[1:9], c(stats::setNames(rep(1, 9), letters[1:9]), new = 30))
  common <- git(merged$root, "rev-parse", "HEAD")
  first_registration <- c("a", "b", "new", letters[3:9])
  git(merged$root, "checkout", "-qb", "first-registration")
  set_priority(merged, first_registration)
  git(merged$root, "commit", "-qam", "Register near the beginning")
  git(merged$root, "checkout", "-qb", "second-registration", common)
  set_priority(merged, c(letters[1:7], "new", letters[8:9]))
  git(merged$root, "commit", "-qam", "Register near the end")
  git(merged$root, "merge", "--no-edit", "first-registration")
  rejected <- propose(merged)
  expect(!rejected$eligible && grepl("new (positions 3, 9)", rejected$reason, fixed = TRUE),
    "a clean merge of two unique priority lists is rejected when combined registrations duplicate")
  before_dispatch <- testthat::find_test_scripts(file.path(merged$root, "tests/testthat"),
    full.names = FALSE, start_first = c("a", "b", "new", letters[3:7], "new", letters[8:9]))
  set_priority(merged, first_registration)
  after_dispatch <- testthat::find_test_scripts(file.path(merged$root, "tests/testthat"),
    full.names = FALSE, start_first = review_ready_priority_read(readLines(file.path(merged$root, "DESCRIPTION"))))
  expect(identical(before_dispatch, paste0("test-", first_registration, ".R")) &&
    identical(before_dispatch, after_dispatch), "explicit later-duplicate removal preserves exact native dispatch")

  complete <- c("old", letters[1:7], "tiny", "new", "skip")
  f <- fixture(complete, c(old = 20, stats::setNames(rep(20, 7), letters[1:7]),
    tiny = 0.1, new = 100, skip = 0), "skip")
  original <- readLines(file.path(f$root, "DESCRIPTION"))
  proposal <- propose(f)
  expect(proposal$eligible && identical(proposal$selected, c("new", "old", letters[1:7], "tiny", "skip")),
    "reordering preserves complete registration including lightweight and skipped files")
  expect(identical(readLines(file.path(f$root, "DESCRIPTION")), original), "generation never edits source")
  again <- propose(f)
  for (name in c("priority.patch", "explanation.md", "proposal.rds")) {
    expect(identical(readBin(file.path(proposal$directory, name), "raw", n = 1e6),
      readBin(file.path(again$directory, name), "raw", n = 1e6)), "identical-input determinism")
  }
  set_priority(f, c(letters[1:7], "old", "tiny", "new", "skip"))
  updated <- propose(f)
  expect(identical(updated$current, c(letters[1:7], "old", "tiny", "new", "skip")),
    "priority-only changes recalculate against the current complete order")
  expect(identical(updated$native, proposal$native), "recalculation preserves measured native dispatch comparison")
  set_priority(f, proposal$selected)
  unchanged <- propose(f)
  expect(unchanged$eligible && !file.exists(file.path(unchanged$directory, "priority.patch")), "unchanged configuration has no patch")
  set_priority(f, complete)

  dir.create(file.path(f$root, "tools"))
  for (name in c("propose-test-priority.R", "review-ready-priority.R", "review-ready-timing.R")) {
    expect(file.copy(file.path("tools", name), file.path(f$root, "tools", name)), "copy proposal command")
  }
  cli <- command(file.path(R.home("bin"), "Rscript"),
    c("tools/propose-test-priority.R", "--run", f$run), f$root)
  expect(grepl("reviewable patch; source unchanged", cli$stdout, fixed = TRUE), "explicit recalculation command")
  broken_git <- file.path(workspace, "broken-git")
  dir.create(broken_git)
  writeLines(c("#!/bin/sh", "exit 2"), file.path(broken_git, "git"))
  Sys.chmod(file.path(broken_git, "git"), "0755")
  old_path <- Sys.getenv("PATH")
  failed <- tryCatch({
    Sys.setenv(PATH = paste(broken_git, old_path, sep = .Platform$path.sep))
    propose(f)
  }, finally = Sys.setenv(PATH = old_path))
  expect(!failed$eligible && grepl("could not calculate DESCRIPTION patch", failed$reason, fixed = TRUE), "calculation failure disables only proposal")
  expect(readRDS(file.path(f$run, "result.rds"))$status == "passed", "proposal failure preserves ordinary check result")

  # Strict boundaries: exactly 50 percent or exactly ten seconds never cross.
  boundary <- fixture(c("a", "b", "c", "d"), c(a = 20, b = 30, c = 40, d = 50))
  b <- propose(boundary)
  expect(identical(b$candidate, c("a", "b", "c", "d")), "equal thresholds retain order")
  close <- fixture(c("a", "b", "c", "d"), c(a = 1, b = 1, c = 1, d = 100))
  p <- propose(close)
  expect(identical(p$candidate, c("d", "a", "b", "c")) &&
    identical(p$selected, c("a", "b", "c", "d")) && !p$adopt_reordering, "negligible whole-suite gain retains existing order")
  big <- fixture(letters[1:16], stats::setNames(c(rep(20, 8), rep(c(100, 90, 80, 70), 2)), letters[1:16]))
  p <- propose(big)
  expect(p$adopt_reordering && identical(p$selected, c(letters[9:16], letters[1:8])) &&
    p$models[["native"]] == 120 && p$models[["selected"]] == 110, "ten-second adoption boundary uses whole-suite model")
  ties <- fixture(c("old", "skip", "z", "a", "tiny"), c(old = 5, skip = 0, z = 5, a = 5, tiny = 0.1), "skip")
  expect(identical(propose(ties)$selected, c("old", "skip", "z", "a", "tiny")),
    "equal-duration files retain their committed order")
  barrier <- fixture(c("a", "skip", "b", "c"), c(a = 1, skip = 1, b = 100, c = 2), "skip")
  expect(identical(propose(barrier)$candidate, c("a", "skip", "b", "c")), "ineligible adjacent comparison blocks crossing")

  reject(function(f) cat("# changed\n", file = file.path(f$root, "tests", "testthat", "test-old.R"), append = TRUE), "test/helper content changed")
  reject(function(f) writeLines("# helper", file.path(f$root, "tests", "testthat", "helper-new.R")), "test/helper membership changed")
  reject(function(f) file.rename(file.path(f$root, "tests", "testthat", "test-old.R"),
    file.path(f$root, "tests", "testthat", "test-renamed.R")), "test membership changed")
  reject(function(f) {
    path <- file.path(f$timing, "manifest.rds")
    x <- readRDS(path); x$dependencies[1L, "Version"] <- "0.0.0"; saveRDS(x, path)
  }, "dependency environment changed")
  reject(function(f) {
    path <- file.path(f$run, "invocation.rds")
    x <- readRDS(path); x$started_utc <- "2000-01-01T00:00:00.000000Z"; saveRDS(x, path)
  }, "expired")
  reject(function(f) unlink(file.path(f$timing, "1.tsv")), "changed or is incomplete")
  reject(function(f) {
    path <- file.path(f$run, "result.rds")
    x <- readRDS(path); x$status <- "failed"; saveRDS(x, path)
  }, "successful eligible local full")
  for (fields in list(
    list(repository_contracts = "failed"), list(repository_contracts = "not-run"),
    list(repository_contracts = NULL), list(repository_contracts = TRUE),
    list(status = "cancelled"), list(status = NULL),
    list(heavy_check = "failed"), list(heavy_check = NULL),
    list(sha = strrep("b", 40L)), list(started_utc = "2000-01-01T00:00:00.000000Z"),
    list(reason = "different invocation")
  )) {
    reject(function(f) {
      path <- file.path(f$run, "result.rds")
      saveRDS(utils::modifyList(readRDS(path), fields), path)
    }, "successful eligible local full")
  }
  reject(function(f) {
    for (name in c("invocation.rds", "result.rds")) {
      path <- file.path(f$run, name)
      x <- readRDS(path); x$scope <- "source"; saveRDS(x, path)
    }
  }, "successful eligible local full")
  reject(function(f) {
    path <- file.path(f$timing, "evidence.rds")
    x <- readRDS(path); x$eligible <- FALSE; saveRDS(x, path)
  }, "successful eligible local full")
  reject(function(f) {
    path <- file.path(f$timing, "evidence.rds")
    x <- readRDS(path); x$actual_workers <- 4L; saveRDS(x, path)
  }, "changed or is incomplete")
  reject(function(f) {
    path <- file.path(f$timing, "manifest.rds")
    x <- readRDS(path)
    x$md5[[1L]] <- strrep("0", 32L)
    saveRDS(x, path)
  }, "archived input identities changed")
  reject(function(f) {
    path <- file.path(f$timing, "1.worker.rds")
    x <- readRDS(path)
    x$session$loadedOnly$testthat$Version <- "0.0.0"
    saveRDS(x, path)
  }, "worker dependency differs")

  # Only the workers' R/platform/linear-algebra stack is held to the parent's; see review-ready-runner.md, "File timing evidence".
  edit_workers <- function(f, edit, pids = 1:4) {
    for (pid in pids) {
      path <- file.path(f$timing, paste0(pid, ".worker.rds"))
      x <- readRDS(path); x$session <- edit(x$session); saveRDS(x, path)
    }
  }
  collated <- fixture(c("old", "new", "tiny", "skip"), c(old = 5, new = 30, tiny = 0.1, skip = 100), "skip")
  edit_workers(collated, function(s) {
    s$locale <- paste0("C/", s$locale); s$time.zone <- "Fixture/Zone"; s
  })
  expect(propose(collated)$eligible, "workers that agree may differ from the parent in locale and time zone")
  for (field in c("R.version", "platform", "running", "locale", "time.zone", "matprod", "BLAS", "LAPACK")) {
    reject(function(f) edit_workers(f, function(s) { s[[field]] <- "fixture-disagreement"; s }, pids = 1L),
      paste0("workers disagree on their environment: ", field))
  }
  for (field in c("R.version", "platform", "running", "matprod", "BLAS", "LAPACK")) {
    reject(function(f) edit_workers(f, function(s) { s[[field]] <- "fixture-difference"; s }),
      paste0("worker environment differs from the measured environment: ", field))
  }
  for (field in c("locale", "time.zone")) {
    reject(function(f) {
      path <- file.path(f$timing, "manifest.rds")
      x <- readRDS(path); x$session[[field]] <- "fixture-adoption-change"; saveRDS(x, path)
    }, "dependency environment changed")
  }

  # Apply and commit only in this fixture, then install an independent clean clone.
  patch <- file.path(proposal$directory, "priority.patch")
  git(f$root, "apply", "--check", patch)
  git(f$root, "apply", patch)
  git(f$root, "add", "DESCRIPTION")
  git(f$root, "commit", "-qm", "Adopt fixture priority")
  clone <- file.path(workspace, "independent-clone")
  command("git", c("clone", "--no-local", "-q", f$root, clone))
  expect(!nzchar(git(clone, "status", "--porcelain")) && !dir.exists(file.path(clone, ".review-ready")), "independent clone starts clean without timing state")
  library <- file.path(workspace, "library")
  completions <- file.path(workspace, "completions")
  dir.create(library); dir.create(completions)
  command(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", paste0("--library=", library), clone))
  script <- file.path(workspace, "native.R")
  dispatch <- file.path(workspace, "dispatch.txt")
  writeLines(c(
    'library(testthat)',
    'trace("queue_setup", where=asNamespace("testthat"), print=FALSE, tracer=quote(writeLines(test_paths, Sys.getenv("FIXTURE_DISPATCH"))))',
    'test_check("fixture")'
  ), script)
  command(file.path(R.home("bin"), "Rscript"), script, file.path(clone, "tests"),
    c(R_LIBS = paste(c(library, .libPaths()), collapse = .Platform$path.sep),
      TESTTHAT_PARALLEL = "true", TESTTHAT_CPUS = "8", FIXTURE_DISPATCH = dispatch,
      FIXTURE_COMPLETIONS = completions, MARGINPLYR_REVIEW_TIMING = "", MARGINPLYR_REVIEW_TIMING_HELPER = ""))
  expect(identical(readLines(dispatch), paste0("test-", c("new", "old", letters[1:7], "tiny", "skip"), ".R")),
    "native controller dispatch follows committed priorities")
  done <- list.files(completions, full.names = TRUE)
  expect(length(done) == 11L && all(vapply(done, function(p) identical(readLines(p), "done"), logical(1))), "native execution runs every file exactly once")
  cat("Priority proposal contracts passed.\n")
})
