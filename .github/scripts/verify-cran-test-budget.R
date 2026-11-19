# Holds a finished check's test run to the CRAN tier's time budget.
#
# A check that runs the CRAN tier is the one CRAN itself runs, and CRAN expects
# a package's whole check to finish within minutes. Nothing else fails when the
# registered files grow past that: every test still passes, only later. This
# reads the time out of the check's own test log, so it measures the run that
# happened rather than a second one.
#
# It refuses a check that asked for the complete suite instead of passing it
# unmeasured. `r-lib/actions/setup-r` exports `NOT_CRAN=true` into any job that
# does not set the variable itself, so a job meant to run the tier that loses
# its `NOT_CRAN: false` would otherwise run every test and report this gate
# green. `cran-test-budget.R` holds the budget; ADR 0045 holds why the tier
# exists.

source(".github/scripts/ci-helpers.R")
source(".github/scripts/cran-test-budget.R")

if (!cran_tier_run()) {
  stop(
    "This check asked for the complete suite (`NOT_CRAN` or ",
    "`MARGINPLYR_COMPLETE_SUITE` is true), so it did not run the CRAN tier ",
    "this step exists to time."
  )
}
rcheck <- rcheck_directory()
problem <- cran_test_budget_problem(rcheck)
if (!is.null(problem)) {
  stop(problem)
}
message(sprintf(
  "Verified: the CRAN tier's tests took %.0f s, within the %.0f s budget.",
  test_elapsed_seconds(
    readLines(file.path(rcheck, "tests", "testthat.Rout"), warn = FALSE)
  ),
  cran_test_budget_seconds
))
