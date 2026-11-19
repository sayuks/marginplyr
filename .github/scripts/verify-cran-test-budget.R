# Holds a finished check's test run to the CRAN tier's time budget.
#
# A check that runs the CRAN tier is the one CRAN itself runs, and CRAN expects
# a package's whole check to finish within minutes. Nothing else fails when the
# registered files grow past that: every test still passes, only later. This
# reads the time out of the check's own test log, so it measures the run that
# happened rather than a second one.
#
# A job that asks for the complete suite by name, `MARGINPLYR_COMPLETE_SUITE`,
# has no budget and passes unmeasured. `NOT_CRAN` being true does not excuse a
# job the same way: `r-lib/actions/setup-r` exports it as true into any job
# that leaves it unset, so a job meant to run the tier that lost its
# `NOT_CRAN: false` would run every test and report this gate green. That check
# is refused instead. `cran-test-budget.R` holds the budget; ADR 0045 holds why
# the tier exists.

source(".github/scripts/ci-helpers.R")
source(".github/scripts/cran-test-budget.R")

if (isTRUE(as.logical(Sys.getenv("MARGINPLYR_COMPLETE_SUITE")))) {
  message(
    "This job asked for the complete suite by name; the CRAN tier's budget ",
    "does not apply."
  )
} else {
  if (!cran_tier_run()) {
    stop(
      "`NOT_CRAN` is true, so this check ran the complete suite, not the CRAN ",
      "tier it is held to. Set `NOT_CRAN: false` on the job, or ",
      "`MARGINPLYR_COMPLETE_SUITE: true` if the complete suite is what it runs."
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
}
