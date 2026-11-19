# The CRAN tier's time budget (ADR 0045), for the two readers that hold a
# finished check to it: `verify-cran-test-budget.R` in CI and the local CRAN
# preflight in `tools/cran-preflight-lib.R`. Sourced by path, like
# `cran-note-policy.R`, because one of those readers is not a workflow step.

# The elapsed seconds `tests/testthat.R` may take when it runs the CRAN tier.
# The tier is sized against the maintainer's machine, where it takes well under
# half of this; the rest is room for a hosted runner and for CRAN's slower
# check machines, whose whole check has to stay within minutes.
cran_test_budget_seconds <- 300

# Whether a check started from this environment runs the CRAN tier rather than
# the complete suite. `tests/testthat.R` decides that for the check itself and,
# being shipped, cannot share this definition; `tools/test-cran-tier.R` holds
# the two to one answer.
cran_tier_run <- function() {
  requested <- vapply(
    c("NOT_CRAN", "MARGINPLYR_COMPLETE_SUITE"),
    function(name) isTRUE(as.logical(Sys.getenv(name))),
    logical(1)
  )
  !any(requested)
}

# The elapsed seconds one test script took, from the lines of its `.Rout`.
# `R CMD check` runs each script through `R CMD BATCH`, which ends the log with
# `proc.time()`; a script that stopped early never prints it, so `NA` means the
# script did not finish.
test_elapsed_seconds <- function(test_log) {
  header <- grep(
    "^[[:space:]]*user[[:space:]]+system[[:space:]]+elapsed[[:space:]]*$",
    test_log,
    useBytes = TRUE
  )
  if (length(header) == 0L || header[length(header)] == length(test_log)) {
    return(NA_real_)
  }
  times <- strsplit(trimws(test_log[header[length(header)] + 1L]), "[[:space:]]+")
  elapsed <- suppressWarnings(as.numeric(times[[1L]]))
  if (length(elapsed) != 3L || anyNA(elapsed)) NA_real_ else elapsed[[3L]]
}

# `NULL` when a CRAN-tier test log is within `budget`, otherwise the finding as
# one sentence. An unreadable time is a finding too: a gate that cannot see the
# run would otherwise pass it.
cran_test_budget_problem <- function(test_log, budget = cran_test_budget_seconds) {
  elapsed <- test_elapsed_seconds(test_log)
  if (is.na(elapsed)) {
    return(paste0(
      "The testthat log does not end with its elapsed time, so the CRAN tier's ",
      "run cannot be held to its budget."
    ))
  }
  if (elapsed <= budget) {
    return(NULL)
  }
  sprintf(
    paste0(
      "The CRAN tier's tests took %.0f s, over the %.0f s budget. Move test ",
      "files out of `cran_tier` in tests/testthat.R, or make the registered ",
      "ones faster."
    ),
    elapsed,
    budget
  )
}
