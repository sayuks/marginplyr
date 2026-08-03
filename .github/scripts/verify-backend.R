# Confirms that a dedicated backend job executed the contracts it exists for.
#
# `MARGINPLYR_REQUIRED_SUGGESTS` proves only that the backend package is
# installed. It says nothing about whether the tests that exercise the backend
# actually ran: a test that is renamed, deleted, or skipped for some unrelated
# reason leaves the job green and the contract unproven, which is the exact
# failure the dedicated jobs exist to rule out.
#
# So the workflow names each contract as a test name, and this script re-runs
# the checked tarball's suite against the package the check installed, then
# asserts that every named test ran, passed, and did not skip. Naming them in
# the workflow is deliberate: renaming a contract test now forces an edit to
# the gate that guards it, instead of silently retiring the gate.

source(".github/scripts/ci-helpers.R")

required_tests <- trimws(strsplit(
  Sys.getenv("MARGINPLYR_BACKEND_TESTS", ""),
  ";",
  fixed = TRUE
)[[1]])
required_tests <- required_tests[nzchar(required_tests)]
if (length(required_tests) == 0L) {
  stop("MARGINPLYR_BACKEND_TESTS is empty, so this job asserts nothing.")
}

backend <- Sys.getenv("MARGINPLYR_BACKEND_NAME", "backend")
rcheck <- rcheck_directory()

# R CMD check installs the package under the .Rcheck directory and copies the
# tarball's tests beside it, so both come from the tarball rather than the
# working tree.
library_path <- normalizePath(rcheck)
test_path <- file.path(rcheck, "tests", "testthat")
if (!dir.exists(test_path)) {
  stop(sprintf("No tests directory at '%s'.", test_path))
}
.libPaths(c(library_path, .libPaths()))

results <- testthat::test_dir(
  test_path,
  package = "marginplyr",
  load_package = "installed",
  reporter = "silent",
  stop_on_failure = FALSE
)
outcomes <- as.data.frame(results)

report <- function(test) {
  row <- outcomes[outcomes$test == test, , drop = FALSE]
  if (nrow(row) == 0L) {
    return("did not run")
  }
  if (sum(row$skipped) > 0L) {
    return("skipped")
  }
  if (sum(row$failed) > 0L || any(row$error)) {
    return("failed")
  }
  if (sum(row$passed) == 0L) {
    return("asserted nothing")
  }
  sprintf("%d assertion(s) passed", sum(row$passed))
}

statuses <- vapply(required_tests, report, character(1))
proved <- grepl("passed$", statuses)

summary_lines <- c(
  sprintf("## Live %s contracts", backend),
  "",
  sprintf(
    "%d of %d named contracts executed.",
    sum(proved),
    length(required_tests)
  ),
  "",
  paste0(
    "- ", ifelse(proved, "OK", "MISSING"),
    " **", names(statuses), "** — ", statuses
  )
)
write_step_summary(summary_lines)

if (!all(proved)) {
  stop(sprintf(
    "These %s contracts were not executed: %s.",
    backend,
    paste(names(statuses)[!proved], collapse = "; ")
  ))
}
