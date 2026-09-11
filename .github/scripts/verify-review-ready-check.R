# Exercises the review-ready command's deterministic contract without running
# its expensive test, lint, build, and R CMD check subprocesses.

source(".github/scripts/cran-note-policy.R")
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

fixture_prerequisites <- review_ready_prerequisites(
  package_available = function(package) TRUE,
  find_command = function(command) "/jarl",
  r_bin = "/R/bin"
)
expect_identical(
  fixture_prerequisites,
  list(r = "/R/bin/R", rscript = "/R/bin/Rscript", jarl = "/jarl"),
  "available prerequisites"
)
expect_error(
  review_ready_prerequisites(
    package_available = function(package) package != "lintr",
    find_command = function(command) "",
    r_bin = "/R/bin"
  ),
  "lintr, jarl",
  "missing prerequisites"
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
  c("Full testthat suite", "jarl", "package-aware lintr"),
  "the fixed source-step order"
)
expect_identical(steps$jarl$args, c("check", "."), "the jarl command")
expect_identical(
  steps[["package-aware lintr"]]$env,
  "LINTR_ERROR_ON_LINT=true",
  "the lint failure setting"
)
expect_true(
  grepl("testthat::test_local", steps[["Full testthat suite"]]$args[[2L]], fixed = TRUE),
  "the full test suite"
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
  c(`_R_CHECK_CRAN_INCOMING_REMOTE_` = "false"),
  "the named remote-incoming setting"
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
  "the applied remote incoming setting"
)
expect_true(
  grepl("review_ready_check_cli", entrypoint, fixed = TRUE),
  "the public entry point"
)
