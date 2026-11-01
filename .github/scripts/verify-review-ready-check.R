# Exercises the review-ready command's deterministic contract without running
# its expensive test, lint, build, and R CMD check subprocesses.

source(".github/scripts/cran-note-policy.R")
source("tools/dependency-requirements.R")
source("tools/required-test-suggests.R")
source("tools/review-ready-progress.R")
source("tools/review-ready-check-lib.R")

expect_identical <- function(actual, expected, label) {
  if (!identical(actual, expected)) {
    stop(
      sprintf(
        "%s: expected %s, got %s.",
        label,
        paste(deparse(expected), collapse = " "),
        paste(deparse(actual), collapse = " ")
      ),
      call. = FALSE
    )
  }
}

expect_true <- function(actual, label) {
  expect_identical(isTRUE(actual), TRUE, label)
}

expect_error <- function(expression, pattern, label) {
  condition <- tryCatch({
    force(expression)
    NULL
  }, error = function(cnd) cnd)
  expect_true(inherits(condition, "error"), paste(label, "raises"))
  expect_true(
    grepl(pattern, conditionMessage(condition), fixed = TRUE),
    paste(label, "message")
  )
}

expect_identical(parse_review_ready_args(character()), list(), "no arguments")

# The process boundary exposes progress before returning its terminal verdict.
local({
  directory <- tempfile("review-ready-progress-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  ticks <- 0L
  parsed <- FALSE
  process <- list(
    poll_io = function(timeout) {
      ticks <<- ticks + 1L
      transcript <- file.path(directory, "fixture.Rcheck", "tests", "testthat.Rout")
      dir.create(dirname(transcript), recursive = TRUE, showWarnings = FALSE)
      if (ticks == 1L) {
        writeLines(c("Starting 1 test process.", "Error:",
          "! testthat subprocess failed to start.", "Caused by error:",
          "! fixture worker startup failure"), transcript)
        cat("partial frame", file = transcript, append = TRUE)
      } else {
        writeLines(c("total\t2", "done\ttest-first.R"),
          file.path(directory, "test-progress.tsv"))
      }
      if (ticks == 2L) {
        cat(" complete\n", file = transcript, append = TRUE)
        file.rename(transcript, paste0(transcript, ".fail"))
      }
    },
    read_output_lines = function() paste("checker poll", ticks),
    is_alive = function() {
      if (ticks == 3L) {
        writeLines(c("total\t2", "done\ttest-first.R", "done\ttest-second.R", "finished"),
          file.path(directory, "test-progress.tsv"))
      }
      ticks < 3L
    },
    wait = function() NULL,
    kill = function() stop("A completed process must not be killed."),
    parse_results = function() {
      parsed <<- TRUE
      list(status = 17L, timeout = FALSE, errors = "fixture failed")
    }
  )
  factory <- function(..., env) {
    expect_identical(env[["MARGINPLYR_REVIEW_PROGRESS"]],
      file.path(directory, "test-progress.tsv"), "the attempt-local progress file")
    process
  }
  output <- capture.output(result <- review_ready_progress_check(
    "fixture.tar.gz", args = "--as-cran", build_args = NULL,
    check_dir = directory, error_on = "never", env = character(), repos = character(),
    process_factory = factory
  ))
  expect_identical(sum(output == "Starting 1 test process."), 1L,
    "the native selected-process count is forwarded once")
  expect_true(match("Starting 1 test process.", output) <
    grep("1/2 files completed", output, fixed = TRUE)[[1L]],
    "the startup message is visible while the check is running")
  expect_true(match("! fixture worker startup failure", output) <
    match("checker poll 2", output),
    "worker startup diagnostics are visible before reporter events or terminal exit")
  expect_identical(sum(output == "Test worker diagnostic:"), 1L,
    "renaming the transcript to .fail does not repeat the diagnostic")
  expect_true("partial frame complete" %in% output,
    "an unfinished diagnostic line is forwarded after it becomes complete")
  expect_true(any(grepl("1/2 files completed (50.0%); last: test-first.R",
    output, fixed = TRUE)), "live file progress")
  expect_true(any(grepl("2/2 files completed (100.0%); last: test-second.R",
    output, fixed = TRUE)), "completion events remain visible at process exit")
  expect_true(parsed, "only the terminal process result supplies the verdict")
  expect_identical(result$status, 17L, "progress cannot turn a failed check green")
})
expect_error(
  parse_review_ready_args("--fast"),
  "takes no arguments",
  "a configurable gate"
)

description <- read.dcf("DESCRIPTION")
versioned_review_description <- matrix(
  "alpha (>= 1.0), beta",
  nrow = 1L,
  dimnames = list(NULL, "Config/Needs/review")
)
versioned_review_packages <- character()
versioned_review_prerequisites <- review_ready_prerequisites(
  description = versioned_review_description,
  package_available = function(package) {
    versioned_review_packages <<- c(versioned_review_packages, package)
    TRUE
  },
  find_command = function(command) "/jarl",
  r_bin = "/R/bin"
)
expect_identical(
  versioned_review_prerequisites,
  list(r = "/R/bin/R", rscript = "/R/bin/Rscript", jarl = "/jarl"),
  "versioned review prerequisites"
)
expect_identical(
  versioned_review_packages,
  c("alpha", "beta"),
  "review package derivation with constraints"
)

fixture_review_packages <- character()
fixture_prerequisites <- review_ready_prerequisites(
  description = description,
  package_available = function(package) {
    fixture_review_packages <<- c(fixture_review_packages, package)
    TRUE
  },
  find_command = function(command) "/jarl",
  r_bin = "/R/bin"
)
expect_identical(
  fixture_prerequisites,
  list(r = "/R/bin/R", rscript = "/R/bin/Rscript", jarl = "/jarl"),
  "available prerequisites"
)
expect_identical(
  fixture_review_packages,
  c("altdoc", "askpass", "cpp11", "devtools", "filelock", "fs", "jsonlite", "lintr", "openssl",
    "pkgload", "processx", "ps", "R6", "rcmdcheck", "roxygen2", "spelling", "sys",
    "testthat", "yaml"),
  "the declared review packages"
)
expect_error(
  review_ready_prerequisites(
    description = description,
    package_available = function(package) package != "lintr",
    find_command = function(command) "",
    r_bin = "/R/bin"
  ),
  "lintr, jarl",
  "missing prerequisites"
)
expect_error(
  review_ready_prerequisites(
    description = description,
    package_available = function(package) package != "spelling",
    find_command = function(command) "/jarl",
    r_bin = "/R/bin"
  ),
  "spelling",
  "a missing spelling prerequisite"
)

fixture_sha <- paste(rep("a", 40L), collapse = "")
clean_git <- function(root, args) {
  key <- paste(args, collapse = " ")
  switch(
    key,
    `rev-parse --show-toplevel` = normalizePath(root),
    `status --porcelain=v1 --untracked-files=all` = character(),
    `rev-parse HEAD` = fixture_sha,
    stop("Unexpected git query: ", key)
  )
}
clean_identity <- review_ready_identity(".", git_output = clean_git)
expect_identical(clean_identity$sha, fixture_sha, "the checked SHA")

dirty_git <- function(root, args) {
  if (identical(args[[1L]], "status")) {
    return(" M R/example.R")
  }
  clean_git(root, args)
}
expect_error(
  review_ready_identity(".", git_output = dirty_git),
  "requires a clean committed HEAD",
  "a dirty tree"
)

prerequisites <- list(
  r = "/R",
  rscript = "/Rscript",
  jarl = "/jarl"
)
steps <- review_ready_source_steps(prerequisites)
expect_identical(
  names(steps),
  c("Package spelling", "jarl", "package-aware lintr"),
  "the fixed source-step order"
)
expect_true(
  grepl(
    "spelling::spell_check_package",
    steps[["Package spelling"]]$args[[2L]],
    fixed = TRUE
  ),
  "the package spelling command"
)
expect_identical(steps$jarl$args, c("check", "."), "the jarl command")
expect_identical(
  steps[["package-aware lintr"]]$env,
  "LINTR_ERROR_ON_LINT=true",
  "the lint failure setting"
)
expect_true(
  grepl("pkgload::load_all", steps[["package-aware lintr"]]$args[[2L]], fixed = TRUE),
  "the package-aware lint load"
)

empty_result <- list(
  status = 0L,
  timeout = FALSE,
  errors = character(),
  warnings = character(),
  notes = character()
)
empty_outcome <- review_ready_check_outcome(empty_result, "published")
expect_identical(empty_outcome$passed, TRUE, "a clean R CMD check")
expect_identical(empty_outcome$unexpected_notes, integer(), "no NOTE review")

allowed_note <- paste(
  "* checking CRAN incoming feasibility ... NOTE",
  "Maintainer: 'Yusuke Sasaki <sayuks.dev@gmail.com>'",
  "",
  "New submission",
  sep = "\n"
)
allowed_result <- empty_result
allowed_result$notes <- allowed_note
allowed_outcome <- review_ready_check_outcome(allowed_result, "unpublished")
expect_identical(allowed_outcome$unexpected_notes, integer(), "an allowed NOTE")

timed_allowed_result <- empty_result
timed_allowed_result$notes <- sub(
  "feasibility ... NOTE",
  "feasibility ... [3s/34s] NOTE",
  allowed_note,
  fixed = TRUE
)
timed_allowed_outcome <- review_ready_check_outcome(
  timed_allowed_result,
  "unpublished"
)
expect_identical(
  timed_allowed_outcome$unexpected_notes,
  integer(),
  "an allowed NOTE with an rcmdcheck duration"
)

unknown_result <- empty_result
unknown_result$notes <- "An unexplained NOTE"
unknown_outcome <- review_ready_check_outcome(unknown_result, "unpublished")
expect_identical(unknown_outcome$passed, TRUE, "a NOTE-only check result")
expect_identical(unknown_outcome$unexpected_notes, 1L, "an unexplained NOTE review")

warning_result <- empty_result
warning_result$warnings <- "A warning"
expect_identical(
  review_ready_check_outcome(warning_result, "published")$passed,
  FALSE,
  "a WARNING"
)

halted_result <- empty_result
halted_result$status <- 1L
expect_identical(
  review_ready_check_outcome(halted_result, "published")$passed,
  FALSE,
  "a halted R CMD check with no parsed conditions"
)
timed_out_result <- empty_result
timed_out_result$timeout <- TRUE
expect_identical(
  review_ready_check_outcome(timed_out_result, "published")$passed,
  FALSE,
  "a timed-out R CMD check"
)
fixture_suggests <- c("stats", "utils")
fixture_check_env <- review_ready_rcmdcheck_env(fixture_suggests)
expect_identical(
  fixture_check_env,
  c(
    `_R_CHECK_CRAN_INCOMING_REMOTE_` = "false",
    `_R_CHECK_SYSTEM_CLOCK_` = "false",
    NOT_CRAN = "true",
    TESTTHAT_PARALLEL = "true",
    TESTTHAT_CPUS = "4",
    MARGINPLYR_REQUIRED_SUGGESTS = "stats,utils"
  ),
  "the offline settings, snapshots, four test workers, and required test Suggests"
)

# Real guard and version constraints, using only packages supplied with R.
suggest_fixture <- tempfile("review-ready-suggests-")
dir.create(file.path(suggest_fixture, "tests", "testthat"), recursive = TRUE)
dir.create(file.path(suggest_fixture, "inst", "suggests"), recursive = TRUE)
on.exit(unlink(suggest_fixture, recursive = TRUE), add = TRUE)
expect_true(file.copy("inst/suggests/guard.R",
  file.path(suggest_fixture, "inst", "suggests", "guard.R")), "the fixture guard")
suite_path <- file.path(suggest_fixture, "tests", "testthat", "helper-optional-backends.R")
writeLines("optional_suggest_spec <- function() list(stats = NULL, utils = NULL)", suite_path)
writeLines("Suggests: stats (>= 1.0.0), utils", file.path(suggest_fixture, "DESCRIPTION"))
expect_identical(required_test_suggests(suggest_fixture), fixture_suggests,
  "the required Suggests derive from the suite table")
writeLines("Suggests: stats (>= 9999.0.0), utils", file.path(suggest_fixture, "DESCRIPTION"))
expect_error(required_test_suggests(suggest_fixture), "marginplyr requires >= 9999.0.0",
  "an installed but unsuitable Suggest")
writeLines("optional_suggest_spec <- function() list(marginplyrmissingfixture = NULL)",
  suite_path)
writeLines("Suggests: marginplyrmissingfixture", file.path(suggest_fixture, "DESCRIPTION"))
expect_error(required_test_suggests(suggest_fixture), "{marginplyrmissingfixture} is not installed",
  "an absent required Suggest")
writeLines("optional_suggest_spec <- function() list()", suite_path)
expect_error(required_test_suggests(suggest_fixture), "No optional Suggests are registered",
  "an empty suite table")

expect_identical(
  review_ready_test_package_candidates("."),
  character(),
  "no undeclared test package candidate requiring repository indexes"
)

rin_fixture <- tempfile("review-ready-rin-")
dir.create(file.path(rin_fixture, "tests"), recursive = TRUE)
on.exit(unlink(rin_fixture, recursive = TRUE), add = TRUE)
expect_true(
  file.copy("DESCRIPTION", file.path(rin_fixture, "DESCRIPTION")),
  "the Rin fixture DESCRIPTION"
)
writeLines(
  "definitelymissingpkg::f()",
  file.path(rin_fixture, "tests", "fixture.Rin")
)
expect_identical(
  review_ready_test_package_candidates(rin_fixture),
  "definitelymissingpkg",
  "an undeclared package candidate in a top-level Rin test"
)
expect_error(
  verify_review_ready_test_packages(".", candidates = "pkg"),
  "undeclared package candidates: pkg",
  "an undeclared test package candidate"
)

offline_workspace <- tempfile("review-ready-offline-")
dir.create(offline_workspace)
on.exit(unlink(offline_workspace, recursive = TRUE), add = TRUE)
offline_repositories <- review_ready_offline_repositories(offline_workspace)
expect_identical(
  names(offline_repositories),
  c("CRAN", "BioCsoft", "BioCann", "BioCexp"),
  "the standard repository names"
)
expect_true(
  all(startsWith(offline_repositories, "file:")),
  "local repository URLs"
)
expect_identical(
  nrow(utils::available.packages(
    repos = offline_repositories,
    filters = list()
  )),
  0L,
  "readable empty repository indexes"
)

entrypoint <- paste(readLines("tools/review-ready-check.R", warn = FALSE), collapse = "\n")
library_source <- paste(
  readLines("tools/review-ready-check-lib.R", warn = FALSE),
  collapse = "\n"
)
expect_true(
  grepl("c(\"archive\", \"--format=tar\"", library_source, fixed = TRUE),
  "the exact-HEAD archive"
)
expect_true(
  grepl("--as-cran", library_source, fixed = TRUE),
  "the CRAN-style check"
)
expect_true(
  !grepl("--no-manual", library_source, fixed = TRUE),
  "the complete CRAN-style check"
)
expect_true(
  grepl("review_ready_rcmdcheck_env(required_test_suggests(source_root), selection$path)",
    library_source, fixed = TRUE) &&
    grepl("env = check_env", library_source, fixed = TRUE),
  "the check environment derives from the archived suite"
)
expect_true(
  grepl(
    "repositories <- review_ready_offline_repositories(workspace)",
    library_source,
    fixed = TRUE
  ) && grepl("repos = repositories", library_source, fixed = TRUE),
  "the applied offline repositories"
)
expect_true(
  grepl("review_ready_check_cli", entrypoint, fixed = TRUE),
  "the public entry point"
)

# The checker is an external-process boundary. Its fixture leaves the same
# kinds of files as R CMD check, without depending on a native crash.
diagnostic_fixture <- tempfile("review-ready-diagnostics-")
dir.create(diagnostic_fixture)
on.exit(unlink(diagnostic_fixture, recursive = TRUE), add = TRUE)
fixture_package <- file.path(diagnostic_fixture, "fixture")
dir.create(fixture_package)
writeLines(c(
  "Package: fixture",
  "Version: 0.0.1",
  "Suggests: stats (>= 1.0.0), utils",
  "Config/marginplyr/cran-status: unpublished"
), file.path(fixture_package, "DESCRIPTION"))
dir.create(file.path(fixture_package, "tests", "testthat"), recursive = TRUE)
dir.create(file.path(fixture_package, "inst", "suggests"), recursive = TRUE)
writeLines("optional_suggest_spec <- function() list(stats = NULL, utils = NULL)",
  file.path(fixture_package, "tests", "testthat", "helper-optional-backends.R"))
expect_true(file.copy("inst/suggests/guard.R",
  file.path(fixture_package, "inst", "suggests", "guard.R")), "the archived fixture guard")
fixture_tarball <- file.path(diagnostic_fixture, "fixture.tar.gz")
local({
  previous <- setwd(diagnostic_fixture)
  on.exit(setwd(previous))
  utils::tar(fixture_tarball, "fixture/DESCRIPTION", compression = "gzip")
})
diagnostic_root <- file.path(diagnostic_fixture, "retained")
check_workspace <- file.path(diagnostic_fixture, "workspace")
dir.create(check_workspace)
checker_calls <- 0L
failed_checker <- function(path, ..., check_dir) {
  checker_calls <<- checker_calls + 1L
  check_root <- file.path(check_dir, "fixture.Rcheck")
  dir.create(file.path(check_root, "tests"), recursive = TRUE, showWarnings = FALSE)
  writeLines("check failed", file.path(check_root, "00check.log"))
  writeLines("vignette failed", file.path(check_root, "recipes.log"))
  writeLines("test failed", file.path(check_root, "tests", "test.Rout.fail"))
  writeLines("do not retain", file.path(check_root, "core"))
  writeLines("do not retain", file.path(check_root, "native.ips"))
  child <- review_ready_run_system(
    file.path(R.home("bin"), "Rscript"),
    c("-e", "cat('child process failed\\n'); quit(status = 17L)"),
    directory = check_root,
    capture = TRUE
  )
  result <- empty_result
  result$status <- child$status
  result$stdout <- child$output
  result
}
failure_output <- capture.output(expect_error(
  run_review_ready_rcmdcheck(
    fixture_tarball, check_workspace, clean_identity,
    checker = failed_checker, diagnostic_root = diagnostic_root, env = fixture_check_env
  ),
  "The source-tarball R CMD check did not pass.",
  "a simulated child-process failure"
))
expect_identical(checker_calls, 1L, "an ordinary failure is not retried")
bundles <- normalizePath(list.dirs(diagnostic_root, recursive = FALSE, full.names = TRUE))
expect_identical(length(bundles), 1L, "one retained failure bundle")
expect_true(
  any(grepl(bundles[[1L]], failure_output, fixed = TRUE)),
  "the printed diagnostic location"
)
bundle_files <- list.files(bundles[[1L]], recursive = TRUE)
expect_true(
  all(c("metadata.txt", "stdout.txt", "console.log",
    "check/fixture.Rcheck/00check.log", "check/fixture.Rcheck/recipes.log",
    "check/fixture.Rcheck/tests/test.Rout.fail") %in% bundle_files),
  "available check, console, and vignette logs"
)
expect_true(
  !any(grepl("[.]tar|DESCRIPTION|native[.]ips|core", bundle_files)),
  "no source archive or native memory/report collection"
)
metadata <- readLines(file.path(bundles[[1L]], "metadata.txt"))
expect_true(any(grepl(fixture_sha, metadata, fixed = TRUE)), "retained SHA")
expect_true(any(grepl("Process status: 17", metadata, fixed = TRUE)), "retained exit status")
expect_true(
  all(vapply(c("Started (UTC):", "Failed (UTC):", "R:", "OS:", "Quarto:", "Deno:"),
    function(label) any(startsWith(metadata, label)), logical(1))),
  "execution identity and versions"
)
expect_true(
  any(grepl("child process failed", readLines(file.path(bundles[[1L]], "stdout.txt")),
    fixed = TRUE)),
  "the child console output"
)

throwing_checker <- function(path, ..., check_dir) {
  failed_checker(path, check_dir = check_dir)
  cat("console before exception\n")
  message("message before exception")
  stop("check crashed before returning a result", call. = FALSE)
}
exception_workspace <- file.path(diagnostic_fixture, "exception-workspace")
dir.create(exception_workspace)
exception_output <- capture.output(expect_error(
  run_review_ready_rcmdcheck(
    fixture_tarball, exception_workspace, clean_identity,
    checker = throwing_checker, diagnostic_root = diagnostic_root, env = fixture_check_env
  ),
  "check crashed before returning a result",
  "an exception before the check returns"
))
bundles <- list.dirs(diagnostic_root, recursive = FALSE, full.names = TRUE)
expect_identical(length(bundles), 2L, "an exception also retains diagnostics")
thrown_bundle <- bundles[vapply(bundles, function(bundle) {
  any(grepl("check crashed before returning a result",
    readLines(file.path(bundle, "error.txt")), fixed = TRUE))
}, logical(1))]
expect_identical(length(thrown_bundle), 1L, "the original exception is retained")
console <- readLines(file.path(thrown_bundle, "console.log"))
expect_true(
  all(c("console before exception", "message before exception") %in% console),
  "console output survives an exception"
)

# External commands are simulated to reach the CLI's cleanup/error boundary
# without installing a package in this focused verifier.
observed_workspace <- NULL
fixture_runner <- function(command, args, directory, ...) {
  if (identical(command, "git")) {
    archive <- sub("^--output=", "", args[startsWith(args, "--output=")])
    observed_workspace <<- dirname(archive)
    previous <- setwd(fixture_package)
    on.exit(setwd(previous))
    utils::tar(archive, c("DESCRIPTION", "tests", "inst"))
  } else if (identical(args[1:2], c("CMD", "build"))) {
    file.copy(fixture_tarball, directory)
  }
  list(status = 0L, output = character())
}
run_fixture_cli <- function(checker) {
  review_ready_check_cli(character(), getwd(),
    identity = clean_identity, prerequisites = prerequisites,
    runner = fixture_runner, checker = checker, diagnostic_root = diagnostic_root
  )
}
success_output <- capture.output({
  success_status <- run_fixture_cli(function(..., env) {
    expect_true(endsWith(env[["MARGINPLYR_REVIEW_PROGRESS_REPORTER"]],
      "/source/tools/review-ready-progress-reporter.R"),
      "the progress reporter comes from the committed archive")
    expect_identical(env[names(fixture_check_env)], fixture_check_env,
      "the CLI passes the complete test environment")
    empty_result
  })
})
expect_identical(success_status, 0L, "the successful CLI status")
expect_true(!dir.exists(observed_workspace), "successful workspace cleanup")
expect_identical(
  length(list.dirs(diagnostic_root, recursive = FALSE)), 2L,
  "success creates no diagnostic bundle"
)
failure_output <- capture.output({
  failure_status <- run_fixture_cli(throwing_checker)
})
expect_identical(failure_status, 1L, "the failing CLI status")
expect_true(!dir.exists(observed_workspace), "failed workspace cleanup")
expect_identical(
  length(list.dirs(diagnostic_root, recursive = FALSE)), 3L,
  "diagnostics outlive failed workspace cleanup"
)
expect_identical(checker_calls, 3L, "each failed invocation ran its checker only once")

# The worker transport is bound to this admitted request and checked commit.
local({
  keys <- c("MARGINPLYR_REVIEW_SELECTION", "MARGINPLYR_REVIEW_REQUEST")
  previous <- Sys.getenv(keys, unset = NA_character_)
  on.exit({
    Sys.unsetenv(keys)
    present <- !is.na(previous)
    if (any(present)) do.call(Sys.setenv, as.list(previous[present]))
  })
  transport <- tempfile("review-ready-selection-")
  on.exit(unlink(transport), add = TRUE)
  Sys.unsetenv(keys)
  expect_error(read_review_ready_selection(), "selection transport is required",
    "missing transport cannot authorize Full or omit CI artifact checks")
  Sys.setenv(MARGINPLYR_REVIEW_SELECTION = transport,
    MARGINPLYR_REVIEW_REQUEST = "fixture")
  valid <- c("Path: Repository", paste("Commit:", clean_identity$sha),
    "Request: fixture", "Protocol: 3", "Capacity: exempt",
    "Verifiers: .github/scripts/verify-context-budget.R", paste("Base:", clean_identity$sha))
  writeLines(valid, transport)
  proof <- function() list(path = "Repository", capacity = "exempt")
  expect_identical(read_review_ready_selection(proof),
    list(path = "Repository", sha = clean_identity$sha, base = clean_identity$sha,
      verifiers = ".github/scripts/verify-context-budget.R", protocol = "3", capacity = "exempt"),
    "the admitted worker's Repository transport retains its pinned comparison")
  writeLines(sub(paste("Base:", clean_identity$sha), "Base: invalid", valid, fixed = TRUE), transport)
  expect_error(read_review_ready_selection(proof), "Invalid admitted comparison base",
    "an invalid comparison base")
  writeLines(valid[!startsWith(valid, "Base:")], transport)
  expect_error(read_review_ready_selection(proof), "Invalid admitted",
    "a supervised transport without Base cannot replace CI artifact evidence")
  writeLines(sub("Protocol: 3", "Protocol: 2", valid, fixed = TRUE), transport)
  expect_error(read_review_ready_selection(proof), "Invalid admitted",
    "a legacy transport cannot authorize the supervised gate")
  writeLines(valid, transport)
  Sys.setenv(MARGINPLYR_REVIEW_REQUEST = "another-request")
  expect_error(read_review_ready_selection(proof), "Invalid admitted",
    "a different admitted request")
  Sys.setenv(MARGINPLYR_REVIEW_REQUEST = "fixture")
  writeLines(sub("Verifiers: .github/scripts/verify-context-budget.R", "Verifiers:",
    valid, fixed = TRUE), transport)
  expect_error(read_review_ready_selection(proof), "requires mapped",
    "a missing required verifier route")
})

# Repository keeps every common source check and its mapped artifact checks,
# but never reaches dependency validation, build, or the package checker.
local({
  commands <- list()
  repository_runner <- function(command, args, directory, env = character(), ...) {
    commands[[length(commands) + 1L]] <<- list(command = command, args = args, env = env)
    fixture_runner(command, args, directory, ...)
  }
  route <- list(path = "Repository", sha = clean_identity$sha,
    base = clean_identity$sha, verifiers = ".github/scripts/verify-context-budget.R")
  never_check <- function(...) stop("Repository must not run R CMD check.")
  output <- capture.output(status <- review_ready_check_cli(character(), getwd(),
    identity = clean_identity, prerequisites = prerequisites,
    runner = repository_runner, checker = never_check, selection = route))
  expect_identical(status, 0L, "Repository success")
  expect_identical(length(commands), length(steps) + 2L,
    "Repository runs archive, all common checks, and mapped verification")
  expect_identical(lapply(commands[2:4], `[[`, "args"),
    unname(lapply(steps, `[[`, "args")), "Repository retains exact common commands")
  expect_identical(commands[[5L]]$args, route$verifiers,
    "Repository executes its required verifier")
  expect_identical(commands[[5L]]$env, c("TESTTHAT_PARALLEL=false", "TESTTHAT_CPUS=1",
    "MARGINPLYR_REVIEW_STAGE_REPORT=",
    paste0("MARGINPLYR_REVIEW_BASE=", route$base)),
    "artifact commands receive the pinned base in system2 environment format")
  expect_true(any(grepl("Repository passed", output, fixed = TRUE)),
    "Repository success has its own verdict")
  expect_true(any(grepl("package build/check omitted", output, fixed = TRUE)),
    "Repository explicitly records omissions")
  expect_true(!dir.exists(observed_workspace), "Repository success cleanup")
  route$path <- "Full"
  route$reused <- "build-full"
  commands <- list()
  output <- capture.output(status <- review_ready_check_cli(character(), getwd(),
    identity = clean_identity, prerequisites = prerequisites,
    runner = repository_runner, checker = never_check, selection = route))
  expect_identical(status, 0L, "a reused Full obligation retains current source and artifact checks")
  expect_identical(length(commands), length(steps) + 2L,
    "reuse executes no source build or package checker")
  expect_true(any(grepl("not a new Full pass", output, fixed = TRUE)),
    "reused Full evidence is not reported as a new execution")
  route$path <- "Documentation"
  route$reused <- c("build-documentation", "documentation-assertions")
  commands <- list()
  capture.output(status <- review_ready_check_cli(character(), getwd(),
    identity = clean_identity, prerequisites = prerequisites,
    runner = repository_runner, checker = never_check, selection = route))
  expect_identical(status, 0L, "Documentation can reuse its two distinct units")
  expect_identical(length(commands), length(steps) + 2L,
    "reused Documentation does not reconstruct assertions or build/check")
  route$path <- "Repository"
  route$reused <- NULL
  for (failed in 2:5) {
    seen <- 0L
    failing_runner <- function(...) {
      seen <<- seen + 1L
      if (seen == failed) return(list(status = 7L, output = character()))
      repository_runner(...)
    }
    output <- capture.output(status <- review_ready_check_cli(character(), getwd(),
      identity = clean_identity, prerequisites = prerequisites,
      runner = failing_runner, checker = never_check, selection = route))
    expect_identical(status, 1L, "a required Repository stage fails the invocation")
    expect_identical(seen, failed, "failure stops subsequent Repository stages")
    expect_true(!dir.exists(observed_workspace), "Repository failure cleanup")
  }
  route$path <- "Full"
  seen <- 0L
  failing_artifact <- function(...) {
    seen <<- seen + 1L
    if (seen == 5L) return(list(status = 7L, output = character()))
    repository_runner(...)
  }
  output <- capture.output(status <- review_ready_check_cli(character(), getwd(),
    identity = clean_identity, prerequisites = prerequisites,
    runner = failing_artifact, checker = never_check, selection = route))
  expect_identical(status, 1L, "Full cannot bypass a failing required artifact")
  expect_identical(seen, 5L, "Full stops before package checking after artifact failure")
  route$path <- "Repository"
  route$verifiers <- "missing-required-verifier.R"
  output <- capture.output(status <- review_ready_check_cli(character(), getwd(),
    identity = clean_identity, prerequisites = prerequisites,
    runner = repository_runner, checker = never_check, selection = route))
  expect_identical(status, 1L, "a missing mapped verifier fails")
  route$sha <- paste(rep("b", 40L), collapse = "")
  expect_error(run_review_ready_check(getwd(), identity = clean_identity,
    prerequisites = prerequisites, selection = route),
    "does not match", "a substituted submitted identity")
})

child_script <- file.path(diagnostic_fixture, "failed-cli.R")
writeLines(c(
  'source(".github/scripts/cran-note-policy.R")',
  'source("tools/dependency-requirements.R")',
  'source("tools/required-test-suggests.R")',
  'source("tools/review-ready-check-lib.R")'
), child_script)
dump(c(
  "empty_result", "fixture_package", "fixture_tarball", "diagnostic_root",
  "checker_calls", "observed_workspace", "clean_identity", "prerequisites",
  "failed_checker", "throwing_checker", "fixture_runner", "run_fixture_cli"
), file = child_script, append = TRUE)
cat("quit(status = run_fixture_cli(throwing_checker), save = 'no')\n",
  file = child_script, append = TRUE)
child_cli <- review_ready_run_system(
  file.path(R.home("bin"), "Rscript"), child_script,
  directory = getwd(), capture = TRUE
)
expect_identical(child_cli$status, 1L, "the failed CLI's process exit")
expect_true(
  any(grepl("Review-ready failure diagnostics:", child_cli$output, fixed = TRUE)),
  "the failing child CLI prints its bundle path"
)
expect_identical(
  length(list.dirs(diagnostic_root, recursive = FALSE)), 4L,
  "the bundle survives the CLI process exit"
)

for (system in c("Darwin", "Windows", "Linux", "unknown")) {
  guidance <- review_ready_crash_guidance(system)
  expect_true(grepl("unavailable", guidance, fixed = TRUE), paste(system, "fallback"))
  expect_true(grepl("not copied", guidance, fixed = TRUE), paste(system, "no collection"))
}
expect_true(grepl("deno-*.ips", review_ready_crash_guidance("Darwin"), fixed = TRUE),
  "a candidate macOS crash-report location")
expect_true(grepl("Event ID 1000", review_ready_crash_guidance("Windows"), fixed = TRUE),
  "Windows Application Error guidance")
expect_true(grepl("coredumpctl info", review_ready_crash_guidance("Linux"), fixed = TRUE),
  "Linux metadata guidance")

local({
  variables <- c("QUARTO_PATH", "QUARTO_DENO")
  previous <- Sys.getenv(variables, unset = NA_character_)
  on.exit({
    Sys.unsetenv(variables[is.na(previous)])
    if (!all(is.na(previous))) {
      do.call(Sys.setenv, as.list(previous[!is.na(previous)]))
    }
  })
  bin <- file.path(diagnostic_fixture, "windows", "bin")
  dir.create(file.path(bin, "tools", "x86_64"), recursive = TRUE)
  quarto <- file.path(bin, "quarto.exe")
  deno <- file.path(bin, "tools", "x86_64", "deno.exe")
  expect_true(all(file.create(c(quarto, deno))), "Windows executable path fixtures")
  Sys.setenv(QUARTO_PATH = quarto, QUARTO_DENO = "")
  versions <- review_ready_render_versions()
  expect_true(grepl(normalizePath(deno, winslash = "/"), versions[["Deno"]], fixed = TRUE),
    "Windows bundled Deno is identified even when it cannot execute on this host")
})

# These fixtures record subprocess arguments and durable files. The checker
# performs no rendering, and the synthetic signal text causes no signal.
local({
  retry_fixture <- tempfile("review-ready-retry-")
  dir.create(retry_fixture)
  on.exit(unlink(retry_fixture, recursive = TRUE), add = TRUE)

  make_retry_fixture <- function(results = NULL, after_first = NULL) {
    fixture <- new.env(parent = environment())
    fixture$root <- tempfile("case-", tmpdir = retry_fixture)
    fixture$workspace <- file.path(fixture$root, "workspace")
    fixture$diagnostics <- file.path(fixture$root, "retained")
    dir.create(fixture$workspace, recursive = TRUE)
    fixture$tarball <- file.path(fixture$root, "fixture.tar.gz")
    expect_true(file.copy(fixture_tarball, fixture$tarball), "retry fixture tarball")
    bin <- file.path(fixture$root, "Quarto [fixture]", "bin")
    dir.create(bin, recursive = TRUE)
    fixture$paths <- c(Quarto = file.path(bin, "quarto"), Deno = file.path(bin, "deno"))
    fixture$files <- c(fixture$paths, Quarto_script = file.path(bin, "quarto.js"))
    writeLines("fixture launcher", fixture$paths[["Quarto"]])
    writeLines("fixture Deno", fixture$paths[["Deno"]])
    writeLines("fixture Quarto script", fixture$files[["Quarto_script"]])
    fixture$identity <- function(tarball) {
      paths <- c(tarball = tarball, fixture$files)
      list(
        paths = fixture$paths,
        hashes = stats::setNames(unname(tools::md5sum(paths)), names(paths)),
        versions = c(Quarto = "fixture Quarto", Deno = "fixture Deno")
      )
    }
    fixture$signal <- paste0(
      fixture$paths[["Quarto"]],
      ': line 208: 4242 Segmentation fault: 11  "${QUARTO_DENO}" ',
      '${QUARTO_ACTION} ${QUARTO_DENO_OPTIONS} ${QUARTO_DENO_EXTRA_OPTIONS} ',
      '"${QUARTO_TARGET}" "$@"'
    )
    fixture$crash <- empty_result
    fixture$crash$status <- 1L
    fixture$crash$errors <- paste(c(
      "* checking re-building of vignette outputs ... ERROR",
      "Error(s) in re-building vignettes:",
      "--- re-building ‘recipes.qmd’ using html",
      fixture$signal,
      "Error: processing vignette 'recipes.qmd' failed with diagnostics:",
      "! Error running quarto CLI from R.",
      "Caused by error:",
      "! Could not evaluate cli `{}` expression: `QUARTO_DENO`.",
      "Caused by error:",
      "! object 'QUARTO_DENO' not found",
      "--- failed re-building ‘recipes.qmd’",
      "",
      "SUMMARY: processing the following file failed:",
      "  ‘recipes.qmd’",
      "",
      "Error: Vignette re-building failed.",
      "Execution halted"
    ), collapse = "\n")
    fixture$crash$stdout <- fixture$crash$errors
    fixture$results <- if (is.null(results)) {
      list(fixture$crash, empty_result)
    } else {
      results(fixture$crash)
    }
    fixture$calls <- list()
    fixture$checker <- function(
      path, args, build_args, check_dir, error_on, env, repos
    ) {
      index <- length(fixture$calls) + 1L
      fixture$calls[[index]] <- list(
        path = path, hash = unname(tools::md5sum(path)), args = args,
        build_args = build_args, check_dir = check_dir,
        fresh = !dir.exists(check_dir), error_on = error_on, env = env, repos = repos
      )
      if (index > length(fixture$results)) {
        stop("The fixture has no further attempt.")
      }
      result <- fixture$results[[index]]
      check_root <- file.path(check_dir, "fixture.Rcheck")
      dir.create(check_root, recursive = TRUE)
      writeLines(c(paste("attempt", index), result$errors),
        file.path(check_root, "00check.log"))
      cat("checker attempt ", index, "\n", sep = "")
      if (index == 1L && !is.null(after_first)) {
        after_first(fixture)
      }
      if (inherits(result, "error")) {
        stop(result)
      }
      result
    }
    fixture$run <- function(...) {
      run_review_ready_rcmdcheck(
        fixture$tarball, fixture$workspace, clean_identity,
        diagnostic_root = fixture$diagnostics, checker = fixture$checker,
        env = fixture_check_env, system = "Darwin", retry_identity = fixture$identity, ...
      )
    }
    fixture
  }

  if (.Platform$OS.type == "unix") local({
    variables <- c("QUARTO_PATH", "QUARTO_DENO")
    previous <- Sys.getenv(variables, unset = NA_character_)
    on.exit({
      Sys.unsetenv(variables[is.na(previous)])
      if (!all(is.na(previous))) {
        do.call(Sys.setenv, as.list(previous[!is.na(previous)]))
      }
    })
    executable_fixture <- make_retry_fixture()
    versions <- c(Quarto = "fixture Quarto 1.0.0", Deno = "fixture Deno 2.0.0")
    for (tool in names(versions)) {
      writeLines(c(
        "#!/bin/sh",
        'if [ "$#" -ne 1 ] || [ "$1" != "--version" ]; then exit 97; fi',
        paste0("printf '%s\\n' '", versions[[tool]], "'")
      ), executable_fixture$paths[[tool]])
    }
    Sys.chmod(executable_fixture$paths, mode = "0755")
    Sys.setenv(
      QUARTO_PATH = executable_fixture$paths[["Quarto"]],
      QUARTO_DENO = executable_fixture$paths[["Deno"]]
    )
    snapshot <- review_ready_retry_identity(executable_fixture$tarball)
    expect_identical(snapshot$paths, stats::setNames(
      normalizePath(executable_fixture$paths, winslash = "/"), names(versions)
    ), "the production identity resolves the selected executables")
    expect_identical(snapshot$versions, versions,
      "the production identity reads the selected executable versions")
    files <- c(tarball = executable_fixture$tarball, executable_fixture$files)
    expect_identical(snapshot$hashes, stats::setNames(tools::md5sum(files), names(files)),
      "the production identity hashes the tarball and rendering components")
    cat("changed script bytes", file = executable_fixture$files[["Quarto_script"]],
      append = TRUE)
    changed_snapshot <- review_ready_retry_identity(executable_fixture$tarball)
    expect_identical(changed_snapshot$paths, snapshot$paths,
      "a script replacement need not change executable paths")
    expect_identical(changed_snapshot$versions, snapshot$versions,
      "a script replacement need not change version output")
    changed_hashes <- names(snapshot$hashes)[snapshot$hashes != changed_snapshot$hashes]
    expect_identical(changed_hashes, "Quarto_script",
      "the production identity detects changed Quarto script bytes")
  })

  recovered <- make_retry_fixture()
  original_hash <- unname(tools::md5sum(recovered$tarball))
  capture.output({
    recovered_outcome <- recovered$run()
  })
  expect_identical(length(recovered$calls), 2L, "one native-crash retry")
  expect_identical(recovered_outcome$attempts, 2L, "the recovered attempt count")
  expect_identical(recovered_outcome$retried, TRUE, "the recovered outcome")
  expect_identical(recovered_outcome$passed, TRUE, "the retry must pass the check")
  expect_true(all(vapply(recovered$calls, `[[`, logical(1), "fresh")),
    "each attempt starts with a fresh check directory")
  expect_identical(length(unique(vapply(recovered$calls, `[[`, character(1), "check_dir"))),
    2L, "separate attempt check directories")
  for (call in recovered$calls) {
    expect_identical(call$path, recovered$tarball, "the same tarball path")
    expect_identical(call$hash, original_hash, "the same tarball bytes")
    expect_identical(call$args, "--as-cran", "every attempt is a complete CRAN-style check")
    expect_identical(call$build_args, NULL, "no tarball rebuild on retry")
    expect_identical(call$error_on, "never", "returned failure diagnostics")
    expect_identical(call$env, fixture_check_env, "unchanged check environment")
  }
  expect_identical(recovered$calls[[1L]]$repos, recovered$calls[[2L]]$repos,
    "unchanged offline repositories")
  documentation_retry <- make_retry_fixture()
  capture.output(documentation_outcome <- documentation_retry$run(path = "Documentation"))
  expect_identical(documentation_outcome$retried, TRUE, "Documentation native-crash retry")
  for (call in documentation_retry$calls) {
    expect_identical(call$args, c("--as-cran", "--no-tests"),
      "Documentation retry keeps its exact arguments")
    expect_identical(call$path, documentation_retry$tarball,
      "Documentation retry keeps its source archive")
    expect_identical(call$hash, documentation_retry$calls[[1L]]$hash,
      "Documentation retry keeps tarball bytes")
  }
  recovered_bundles <- list.dirs(recovered$diagnostics, recursive = FALSE)
  expect_identical(length(recovered_bundles), 1L, "only the failed attempt creates a bundle")
  expect_identical(normalizePath(recovered_outcome$first_failure),
    normalizePath(recovered_bundles[[1L]]), "the recovered outcome links the first failure")
  first_log <- readLines(file.path(recovered_outcome$first_failure,
    "check", "fixture.Rcheck", "00check.log"))
  expect_true("attempt 1" %in% first_log && !"attempt 2" %in% first_log,
    "the first failed check log is not overwritten")
  expect_true(any(grepl(recovered$signal, first_log, fixed = TRUE)),
    "the first native-crash signature survives recovery")
  second_log <- readLines(file.path(recovered_outcome$first_failure,
    "retry", "check", "fixture.Rcheck", "00check.log"))
  expect_true("attempt 2" %in% second_log && !"attempt 1" %in% second_log,
    "the successful retry log is retained separately from the first failure")
  first_console <- readLines(file.path(recovered_outcome$first_failure, "console.log"))
  expect_true("checker attempt 1" %in% first_console &&
    !"checker attempt 2" %in% first_console, "separate attempt console logs")
  retry_summary <- readLines(file.path(recovered_outcome$first_failure, "retry-result.txt"))
  expect_true(all(c("Attempt 1: failed", "Attempt 2: passed") %in% retry_summary),
    "the durable summary distinguishes failure and recovery")
  expect_true(all(c("Process status: 0", "ERRORs: 0", "WARNINGs: 0") %in% retry_summary),
    "the durable summary records the second terminal outcome")
  expect_identical(dget(file.path(recovered_outcome$first_failure, "retry-identity.txt")),
    recovered$identity(recovered$tarball), "the durable retry identity")

  clean <- make_retry_fixture(function(crash) list(empty_result))
  capture.output({
    clean_outcome <- clean$run()
  })
  expect_identical(length(clean$calls), 1L, "a first-attempt pass is not repeated")
  expect_identical(clean_outcome$attempts, 1L, "the clean attempt count")
  expect_identical(clean_outcome$retried, FALSE, "the clean outcome is not recovery")
  expect_identical(clean_outcome$first_failure, NULL, "a clean check has no failure link")
  expect_true(!dir.exists(clean$diagnostics), "a clean check creates no failure bundle")

  repeated <- make_retry_fixture(function(crash) list(crash, crash))
  capture.output(expect_error(repeated$run(),
    "The source-tarball R CMD check did not pass.", "two native-crash failures"))
  expect_identical(length(repeated$calls), 2L, "a failed retry has no third attempt")
  repeated_bundles <- list.dirs(repeated$diagnostics, recursive = FALSE)
  expect_identical(length(repeated_bundles), 2L, "both failed attempts retain diagnostics")
  summary_files <- file.path(repeated_bundles, "retry-result.txt")
  summary_files <- summary_files[file.exists(summary_files)]
  expect_identical(length(summary_files), 1L, "one linked retry-failure summary")
  expect_true(all(c("Attempt 1: failed", "Attempt 2: failed") %in%
    readLines(summary_files)), "the durable summary preserves both failures")

  retry_exception <- make_retry_fixture(function(crash) {
    list(crash, simpleError("retry checker raised an exception"))
  })
  capture.output(expect_error(retry_exception$run(),
    "retry checker raised an exception", "an exception in the retry"))
  expect_identical(length(retry_exception$calls), 2L, "a retry exception is terminal")

  unknown_note <- make_retry_fixture(function(crash) list(crash, unknown_result))
  capture.output({
    note_outcome <- unknown_note$run()
  })
  expect_identical(note_outcome$unexpected_notes, 1L,
    "recovery does not waive unexplained NOTE review")
  expect_identical(note_outcome$notes, 1L, "the retry NOTE is retained")
  expect_true(unknown_result$notes %in%
    readLines(file.path(note_outcome$first_failure, "retry-findings.txt")),
    "the retry NOTE survives workspace cleanup")

  negative <- list(
    ordinary = function(result) {
      result$errors <- "* checking tests ... ERROR\nordinary failure"
      result
    },
    mixed_errors = function(result) {
      result$errors <- c(result$errors, "* checking tests ... ERROR\nordinary failure")
      result
    },
    same_block_error = function(result) {
      result$errors <- paste(result$errors, "Error: ordinary failure", sep = "\n")
      result
    },
    same_block_warning = function(result) {
      result$errors <- paste(result$errors, "WARNING: independent warning", sep = "\n")
      result
    },
    warning = function(result) {
      result$warnings <- "an independent warning"
      result
    },
    test_failure = function(result) {
      result$test_fail <- "a failed test"
      result
    },
    timeout = function(result) {
      result$timeout <- TRUE
      result
    },
    unavailable_status = function(result) {
      result$status <- NULL
      result
    },
    zero_status = function(result) {
      result$status <- 0L
      result
    },
    different_status = function(result) {
      result$status <- 139L
      result
    },
    exit_139_only = function(result) {
      result$status <- 139L
      result$errors <- "* checking re-building of vignette outputs ... ERROR\nexit status 139"
      result
    },
    malformed_signal = function(result) {
      result$errors <- sub("Segmentation fault: 11", "Segmentation fault", result$errors,
        fixed = TRUE)
      result
    },
    other_native_process = function(result) {
      result$errors <- sub('"${QUARTO_DENO}"', '"${OTHER_PROCESS}"', result$errors,
        fixed = TRUE)
      result
    },
    wrong_phase = function(result) {
      result$errors <- sub("re-building of vignette outputs", "tests", result$errors,
        fixed = TRUE)
      result
    },
    unknown_wrapper = function(result) {
      result$errors <- sub("! Error running quarto CLI from R.",
        "! An unknown rendering error occurred.", result$errors, fixed = TRUE)
      result
    },
    unclosed_segment = function(result) {
      result$errors <- sub("--- failed re-building ‘recipes.qmd’", "", result$errors,
        fixed = TRUE)
      result
    },
    different_summary_file = function(result) {
      result$errors <- sub("  ‘recipes.qmd’", "  ‘other.qmd’", result$errors,
        fixed = TRUE)
      result
    },
    quoted_signal = function(result) {
      result$errors <- sub(": line 208: 4242", ": line text quoted 208: 4242",
        result$errors, fixed = TRUE)
      result
    },
    another_failed_vignette = function(result) {
      additional <- paste(
        "--- re-building ‘other.qmd’ using html",
        "Error: an ordinary rendering error",
        "--- failed re-building ‘other.qmd’",
        sep = "\n"
      )
      result$errors <- sub("SUMMARY:", paste(additional, "SUMMARY:", sep = "\n"),
        result$errors, fixed = TRUE)
      result
    }
  )
  for (label in names(negative)) {
    modifier <- negative[[label]]
    fixture <- make_retry_fixture(function(crash) list(modifier(crash)))
    capture.output(expect_error(fixture$run(),
      "The source-tarball R CMD check did not pass.", label))
    expect_identical(length(fixture$calls), 1L, paste(label, "is not retried"))
  }
  classifier_identity <- recovered$identity(recovered$tarball)
  expect_true(review_ready_native_crash(recovered$crash, classifier_identity, "Darwin"),
    "the full macOS launcher signature is eligible")
  # R's --as-cran print_time() adds CPU/elapsed seconds or minutes before ERROR.
  for (timing in c("[3s/34s]", "[1m/11m]")) {
    fixture <- make_retry_fixture(function(crash) {
      crash$errors <- sub("... ERROR", paste("...", timing, "ERROR"),
        crash$errors, fixed = TRUE)
      list(crash, empty_result)
    })
    capture.output(timed_outcome <- fixture$run())
    expect_identical(timed_outcome$attempts, 2L,
      paste("the standard R timing", timing, "retains recovery"))
  }
  for (timing in c("[3s/1m]", "[unknown]", "[3s]")) {
    result <- recovered$crash
    result$errors <- sub("... ERROR", paste("...", timing, "ERROR"),
      result$errors, fixed = TRUE)
    expect_identical(review_ready_native_crash(result, classifier_identity, "Darwin"),
      FALSE, paste("an unsupported timing", timing, "is refused"))
  }
  for (system in c("Linux", "Windows", "unknown")) {
    expect_identical(review_ready_native_crash(recovered$crash, classifier_identity, system),
      FALSE, paste(system, "has no verified native-crash predicate"))
  }
  wrong_launcher <- classifier_identity
  wrong_launcher$paths[["Quarto"]] <- "/another/quarto"
  expect_identical(review_ready_native_crash(recovered$crash, wrong_launcher, "Darwin"),
    FALSE, "a different launcher cannot identify this Quarto process")
  expect_identical(review_ready_native_crash(recovered$crash, NULL, "Darwin"),
    FALSE, "missing tool identity prevents retry")
  finished <- paste(
    "--- re-building ‘other.qmd’ using html", "rendering completed",
    "--- finished re-building ‘other.qmd’", sep = "\n"
  )
  for (anchor in c("--- re-building ‘recipes.qmd’ using html", "SUMMARY:")) {
    result <- recovered$crash
    result$errors <- sub(anchor, paste(finished, anchor, sep = "\n"), result$errors,
      fixed = TRUE)
    expect_true(review_ready_native_crash(result, classifier_identity, "Darwin"),
      "completed sibling vignettes do not obscure the native failure")
  }

  for (changed in c("tarball", "Quarto", "Deno", "Quarto_script")) {
    fixture <- make_retry_fixture(after_first = function(fixture) {
      path <- if (changed == "tarball") fixture$tarball else fixture$files[[changed]]
      if (changed == "tarball") {
        bytes <- readBin(path, "raw", n = file.info(path)$size)
        # The gzip timestamp can change without invalidating the archive.
        bytes[[5L]] <- as.raw((as.integer(bytes[[5L]]) + 1L) %% 256L)
        writeBin(bytes, path)
      } else {
        cat("changed bytes", file = path, append = TRUE)
      }
    })
    capture.output(expect_error(fixture$run(),
      "The source-tarball R CMD check did not pass.", paste(changed, "changed")))
    expect_identical(length(fixture$calls), 1L,
      paste(changed, "identity changes prevent retry"))
  }
  retention_failure <- make_retry_fixture()
  capture.output(expect_error(retention_failure$run(
    preserver = function(...) stop("fixture retention failure")
  ), "The source-tarball R CMD check did not pass.", "failed diagnostic preservation"))
  expect_identical(length(retention_failure$calls), 1L,
    "failed preservation prevents retry")

  cli_fixture <- make_retry_fixture()
  runner_calls <- list()
  retry_runner <- function(command, args, directory, ...) {
    runner_calls[[length(runner_calls) + 1L]] <<- list(command = command, args = args)
    fixture_runner(command, args, directory, ...)
  }
  cli_output <- capture.output({
    cli_status <- review_ready_check_cli(character(), getwd(),
      identity = clean_identity, prerequisites = prerequisites,
      runner = retry_runner, checker = cli_fixture$checker,
      diagnostic_root = cli_fixture$diagnostics, system = "Darwin",
      retry_identity = cli_fixture$identity
    )
  })
  expect_identical(cli_status, 0L, "the recovered CLI status")
  expect_identical(length(cli_fixture$calls), 2L, "the CLI performs one eligible retry")
  expect_true(any(grepl("passed-after-retry", cli_output, fixed = TRUE)),
    "the CLI reports recovery distinctly")
  expect_true(!dir.exists(observed_workspace), "recovered workspace cleanup")
  expect_identical(length(runner_calls), length(steps) + 2L,
    "archive, source steps, and build are not repeated")
  cli_bundles <- list.dirs(cli_fixture$diagnostics, recursive = FALSE)
  expect_identical(length(cli_bundles), 1L, "first-failure evidence outlives CLI cleanup")
  expect_true(file.exists(file.path(cli_bundles[[1L]], "retry-result.txt")),
    "the retry summary outlives CLI cleanup")
})

# Documentation keeps the package-check boundary with exactly one added flag.
local({
  calls <- list()
  route <- list(path = "Documentation", sha = clean_identity$sha,
    verifiers = character())
  runner <- function(command, args, directory, ...) {
    calls[[length(calls) + 1L]] <<- list(command = command, args = args,
      directory = directory, extra = list(...))
    fixture_runner(command, args, directory, ...)
  }
  output <- capture.output(status <- review_ready_check_cli(character(), getwd(),
    identity = clean_identity, prerequisites = prerequisites, runner = runner,
    checker = function(path, args, ..., env) {
      expect_identical(args, c("--as-cran", "--no-tests"), "Documentation check flags")
      expect_identical(env[["TESTTHAT_PARALLEL"]], "false", "serial documentation tests")
      empty_result
    }, selection = route))
  expect_identical(status, 0L, "Documentation succeeds")
  expect_true(any(grepl("full suite and snapshots omitted", output, fixed = TRUE)),
    "Documentation identifies its omissions")
  expect_identical(lapply(calls[2:4], `[[`, "args"),
    unname(lapply(steps, `[[`, "args")), "Documentation retains every common source check")
  assertion <- Filter(function(call) "tools/review-ready-documentation.R" %in% call$args, calls)
  expect_identical(length(assertion), 1L, "one separate documentation assertion stage")
  expect_true(endsWith(assertion[[1L]]$directory, "/source"),
    "assertions run against the submitted archive")
  expect_identical(assertion[[1L]]$extra$env,
    c("MARGINPLYR_REVIEW_STAGE_REPORT=", "MARGINPLYR_REQUIRED_SUGGESTS=stats,utils"),
    "assertions require provisioned Suggests without writing the supervisor's report")
  failed_runner <- function(command, args, directory, ...) {
    if (identical(args, "tools/review-ready-documentation.R")) return(list(status = 7L))
    runner(command, args, directory, ...)
  }
  capture.output(failed <- review_ready_check_cli(character(), getwd(),
    identity = clean_identity, prerequisites = prerequisites, runner = failed_runner,
    checker = function(...) stop("No check after failed assertions"), selection = route))
  expect_identical(failed, 1L, "failed documentation assertions fail the invocation")

})

local({
  root <- tempfile("review-ready-bootstrap-")
  dir.create(file.path(root, "tools"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixtures <- c("tools/test-review-ready-runner.R", "tools/test-review-ready-renderer.R")
  expect_true(all(file.create(file.path(root, fixtures))), "bootstrap fixture sources")
  commands <- list()
  runner <- function(command, args, directory, ...) {
    commands[[length(commands) + 1L]] <<- list(command = command, args = args, directory = directory)
    list(status = 0L)
  }
  selection <- list(path = "Full", protocol = "3", capacity = "Full")
  identity <- list(root = root)
  prerequisites <- list(rscript = "fixture-Rscript")
  capture.output(run_review_ready_bootstrap(selection, identity, prerequisites, runner))
  expect_identical(vapply(commands, `[[`, "", "args"), fixtures,
    "the admitted installed Full gate executes both replacement contracts")
  expect_true(all(vapply(commands, function(command) identical(command$directory, root), logical(1))),
    "bootstrap contracts run against the submitted checkout")
  for (route in list(list(path = "Full", protocol = "3", capacity = "exempt"),
      list(path = "Repository", protocol = "3", capacity = "Full"),
      list(path = "Documentation", protocol = "3", capacity = "Full"),
      list(path = "Full", protocol = "4", capacity = "Full"),
      list(path = "Full"))) {
    commands <- list()
    run_review_ready_bootstrap(route, identity, prerequisites, runner)
    expect_identical(length(commands), 0L, "only the admitted installed Full boundary bootstraps")
  }
  commands <- list()
  unlink(file.path(root, fixtures[[2L]]))
  expect_error(run_review_ready_bootstrap(selection, identity, prerequisites, runner),
    "Required R replacement contract is missing", "missing bootstrap source")
  expect_identical(length(commands), 0L, "missing bootstrap inputs stop before executing either fixture")
  file.create(file.path(root, fixtures[[2L]]))
  failed <- function(...) { runner(...); list(status = 7L) }
  expect_error(run_review_ready_bootstrap(selection, identity, prerequisites, failed),
    "failed with status 7", "a failed replacement contract")
  expect_identical(length(commands), 1L, "bootstrap failure stops the second replacement contract")
})

local({
  calls <- character()
  directories <- character()
  fixtures <- c("tools/test-review-ready-runner.R", "tools/test-review-ready-renderer.R")
  runner <- function(command, args, directory, ...) {
    if (length(args) == 1L && args %in% fixtures) {
      calls <<- c(calls, args)
      directories <<- c(directories, directory)
    } else if (identical(args[1:2], c("CMD", "build"))) {
      calls <<- c(calls, "build")
    }
    fixture_runner(command, args, directory, ...)
  }
  checker <- function(...) {
    calls <<- c(calls, "check")
    empty_result
  }
  capture.output(run_review_ready_check(getwd(), identity = clean_identity,
    prerequisites = prerequisites, runner = runner, checker = checker,
    diagnostic_root = diagnostic_root,
    selection = list(path = "Full", protocol = "3", capacity = "Full")))
  expect_identical(calls, c(fixtures, "build", "check"),
    "the fixed Full gate runs both replacement contracts once before package build/check")
  expect_identical(directories, rep(clean_identity$root, 2L),
    "the fixed gate bootstraps against the admitted checkout")
})
