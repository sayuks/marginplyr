# The CRAN tier's time budget (ADR 0045), for the two readers that hold a
# finished check to it: `verify-cran-test-budget.R` in CI and the local CRAN
# preflight in `tools/cran-preflight-lib.R`. Sourced by path, like
# `cran-note-policy.R`, because one of those readers is not a workflow step.

# The elapsed seconds `tests/testthat.R` may take when it runs the CRAN tier.
# The number is for the hosted Windows runner, the slowest machine that
# enforces it. Win-Builder, whose whole check has to stay inside CRAN's ten
# minutes, takes more than twice as long over the same tests; ADR 0045's
# *The budget* holds the conversion.
cran_test_budget_seconds <- 100

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

# `NULL` when the CRAN-tier check that left `rcheck`, its `<package>.Rcheck`
# directory, ran its tests within `budget`; otherwise the finding as one
# sentence. A run whose time cannot be read is a finding too, since a gate that
# cannot see the run would otherwise pass it: `R CMD check` renames a failing
# script's log to `.Rout.fail`, so only `testthat.Rout` is a finished run.
cran_test_budget_problem <- function(rcheck, budget = cran_test_budget_seconds) {
  log_path <- file.path(rcheck, "tests", "testthat.Rout")
  if (length(log_path) != 1L || !file.exists(log_path)) {
    return(paste0(
      "The check left no finished testthat log, so the CRAN tier's run cannot ",
      "be held to its budget."
    ))
  }
  elapsed <- test_elapsed_seconds(readLines(log_path, warn = FALSE))
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
