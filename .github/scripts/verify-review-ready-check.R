# Exercises the review-ready command's deterministic contract without running
# its expensive test, lint, build, and R CMD check subprocesses.

source(".github/scripts/cran-note-policy.R")
source("tools/dependency-requirements.R")
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
  c("covr", "lintr", "pkgload", "rcmdcheck", "spelling", "testthat", "yaml"),
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
  c("Package spelling", "jarl", "package-aware lintr",
    "Strict line coverage"),
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
expect_identical(
  steps[["Strict line coverage"]]$args,
  "tools/coverage-check.R",
  "the shared strict coverage gate"
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
expect_identical(
  review_ready_rcmdcheck_env(),
  c(
    `_R_CHECK_CRAN_INCOMING_REMOTE_` = "false",
    `_R_CHECK_SYSTEM_CLOCK_` = "false"
  ),
  "the named offline settings"
)

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
  grepl("env = review_ready_rcmdcheck_env()", library_source, fixed = TRUE),
  "the applied offline settings"
)
expect_true(
  grepl(
    "repos = review_ready_offline_repositories(workspace)",
    library_source,
    fixed = TRUE
  ),
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
  "Config/marginplyr/cran-status: unpublished"
), file.path(fixture_package, "DESCRIPTION"))
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
    checker = failed_checker, diagnostic_root = diagnostic_root
  ),
  "The source-tarball R CMD check did not pass.",
  "a simulated child-process failure"
))
expect_identical(checker_calls, 1L, "no automatic retry")
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
exception_output <- capture.output(expect_error(
  run_review_ready_rcmdcheck(
    fixture_tarball, check_workspace, clean_identity,
    checker = throwing_checker, diagnostic_root = diagnostic_root
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
# without running coverage or installing a package in this focused verifier.
observed_workspace <- NULL
fixture_runner <- function(command, args, directory, ...) {
  if (identical(command, "git")) {
    archive <- sub("^--output=", "", args[startsWith(args, "--output=")])
    observed_workspace <<- dirname(archive)
    previous <- setwd(fixture_package)
    on.exit(setwd(previous))
    utils::tar(archive, "DESCRIPTION")
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
  success_status <- run_fixture_cli(function(...) empty_result)
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

child_script <- file.path(diagnostic_fixture, "failed-cli.R")
writeLines(c(
  'source(".github/scripts/cran-note-policy.R")',
  'source("tools/dependency-requirements.R")',
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
