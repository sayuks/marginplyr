# Holds a finished check's test run to the CRAN tier's time budget.
#
# A check that runs the CRAN tier is the one CRAN itself runs, and CRAN expects
# a package's whole check to finish within minutes. Nothing else fails when the
# registered files grow past that: every test still passes, only later. This
# reads the time out of the check's own test log, so it measures the run that
# happened rather than a second one.
#
# A check that asked for the complete suite is not held to the budget, which is
# the tier's and not the suite's. `cran-test-budget.R` holds the budget and that
# decision; ADR 0045 holds why the tier exists.

source(".github/scripts/ci-helpers.R")
source(".github/scripts/cran-test-budget.R")

if (cran_tier_run()) {
  log_path <- test_output_path(rcheck_directory())
  if (is.na(log_path)) {
    stop(
      "No testthat output under the .Rcheck directory, so the CRAN tier's ",
      "run cannot be held to its budget."
    )
  }
  test_log <- readLines(log_path, warn = FALSE)
  problem <- cran_test_budget_problem(test_log)
  if (!is.null(problem)) {
    stop(problem)
  }
  message(sprintf(
    "Verified: the CRAN tier's tests took %.0f s, within the %.0f s budget.",
    test_elapsed_seconds(test_log),
    cran_test_budget_seconds
  ))
} else {
  message("The complete suite was requested; the CRAN tier's budget is not applied.")
}
