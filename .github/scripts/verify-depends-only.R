# Confirms that the minimal-dependency job tested what it claims to test.
#
# `_R_CHECK_DEPENDS_ONLY_=true` passing is not by itself evidence: a check whose
# test suite failed to start, or one where a Suggested package leaked into the
# library, reports the same green status. This script reads the check's own test
# log and asserts both halves of the claim, so the job cannot quietly stop being
# a minimal-dependency gate.
#
# It is the mirror image of `MARGINPLYR_REQUIRED_SUGGESTS` in the dedicated
# backend jobs: there an optional backend must be present, here it must be
# absent.

source(".github/scripts/ci-helpers.R")

# The backends whose absence this job depends on, and the reason `DBI` is not
# among them, are recorded with `optional_backends()` in `ci-helpers.R`.
#
# This script asserts absence at check time, from the tests' own skip lines.
# `verify-library-isolation.R` asserts it earlier and from the library itself,
# which is the half that catches a poisoned dependency cache before the check
# runs. Neither replaces the other: a library can be clean while the suite
# never starts, and the suite can skip a backend for a reason unrelated to
# `_R_CHECK_DEPENDS_ONLY_`.
withheld <- optional_backends()

log_path <- test_output_path(rcheck_directory())
if (is.na(log_path)) {
  stop(
    "No testthat output under the .Rcheck directory, so the ",
    "minimal-dependency check never ran the tests."
  )
}
test_log <- readLines(log_path, warn = FALSE)

# testthat's final tally, for example
# "[ FAIL 0 | WARN 0 | SKIP 68 | PASS 1083 ]".
tally <- grep("\\[ FAIL [0-9]+ \\|", test_log, value = TRUE)
if (length(tally) == 0L) {
  stop("The testthat log has no result tally, so the suite did not complete.")
}
tally <- tally[length(tally)]
counts <- as.integer(regmatches(tally, gregexpr("[0-9]+", tally))[[1]])
names(counts) <- c("fail", "warn", "skip", "pass")
message("Minimal-dependency test tally: ", tally)

if (counts[["fail"]] > 0L) {
  stop("The minimal-dependency run has failing tests.")
}
if (counts[["pass"]] == 0L) {
  stop("The minimal-dependency run passed no tests, so it proved nothing.")
}

# Every optional backend must have skipped. A backend that ran here was visible
# to the check, which means Suggested packages were not actually withheld.
visible <- withheld[!vapply(
  withheld,
  function(package) {
    skipped <- sprintf("{%s} is not installed", package)
    any(grepl(skipped, test_log, fixed = TRUE))
  },
  logical(1)
)]
if (length(visible) > 0L) {
  stop(sprintf(
    paste0(
      "These optional backends did not skip: %s. Suggested packages were ",
      "visible, so this run is not a minimal-dependency gate."
    ),
    paste(visible, collapse = ", ")
  ))
}

message(sprintf(
  "Verified: %d tests passed with %d skipped, and %s were all withheld.",
  counts[["pass"]],
  counts[["skip"]],
  paste(withheld, collapse = ", ")
))
