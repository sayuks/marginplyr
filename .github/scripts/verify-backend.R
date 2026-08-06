# Confirms that a `backend` job's run is sound before its green is believed.
#
# `MARGINPLYR_REQUIRED_SUGGESTS` proves only that the backend package is
# installed, and `verify-library-isolation.R` only that the library holds what
# the job declared. Neither says the suite did anything: a run whose tests never
# started, or one where the tests quietly skipped, reports the same status.
#
# This script used to answer that by re-running the tarball's suite and checking
# a list of contract test names the workflow carried in `proves`. That list is
# gone (#93). It caught a deleted contract test, which is the most visible
# change a diff can carry, and missed the invisible one: a guard added to a test
# body makes it skip in every isolated job, and the diff shows one added line.
# It also could not fail a job for a contract test that exists and is named
# nowhere -- dropping a line from `proves` left the job green.
#
# What replaces it is stronger and needs no names. `verify-suite-coverage.R`
# establishes that every test executes in some single-backend configuration, so
# this job's only remaining question is whether its own run was one. That is
# answered from the check's own testthat log:
#
#   the suite started        a result tally exists at all
#   it failed nothing        FAIL is zero
#   it proved something      PASS is not zero
#   nothing skipped silently every skip reason names a backend this job withheld
#
# The last line is what `proves` could never do. A skip for any other reason --
# a stray `skip_if()`, a `skip_on_os()`, or `NOT_CRAN` being dropped so the
# snapshot tests skip under CRAN semantics -- fails the job, whether or not
# anyone remembered to name the test it belongs to. It is the same technique
# `verify-depends-only.R` uses, pointed the other way: there every optional
# backend must skip, here every skip must be an optional backend the job
# deliberately withheld.

source(".github/scripts/ci-helpers.R")

label <- check_label()
declared <- env_list("MARGINPLYR_REQUIRED_SUGGESTS")
if (length(declared) == 0L) {
  stop(call. = FALSE, sprintf(
    paste0(
      "%s declares no MARGINPLYR_REQUIRED_SUGGESTS, so it is not a backend ",
      "job and has no backend absence to attribute its skips to."
    ),
    label
  ))
}

# The only skips this job may contain. A package it declared is one it promised
# to execute, so a skip naming it is a failure however the log spells it.
withheld <- setdiff(optional_backends(), declared)
permitted <- sprintf("{%s} is not installed", withheld)
# A generated matrix cannot produce a job that withholds nothing, but the
# wording is read in three places and a bare empty vector would silently drop
# out of each of them, so name the case once.
withheld_label <- if (length(withheld) == 0L) {
  "none"
} else {
  paste(withheld, collapse = ", ")
}

log_path <- test_output_path(rcheck_directory())
if (is.na(log_path)) {
  stop(call. = FALSE, sprintf(
    paste0(
      "%s has no testthat output under its .Rcheck directory, so its suite ",
      "never ran and nothing this job exists to prove was executed."
    ),
    label
  ))
}
test_log <- readLines(log_path, warn = FALSE)

tally <- test_tally(test_log)
if (is.null(tally)) {
  stop(call. = FALSE, sprintf(
    "%s: the testthat log has no result tally, so the suite did not complete.",
    label
  ))
}
counts <- tally$counts

# testthat prints one bullet per distinct skip reason under a "Skipped tests"
# heading, each carrying its own count and the locations that produced it:
#
#   • {arrow} is not installed (22): 'test-factor.R:124:3', ...
#     'test-share-backends.R:53:3', ...
#
# The reasons are the lines that begin in column one and carry a parenthesized
# count; the locations wrap onto indented continuation lines. The bullet
# character is not matched, because it is the one part of the format that
# depends on whether the runner had a UTF-8 locale.
skip_reasons <- function(lines) {
  heading <- grep("Skipped tests", lines, useBytes = TRUE)
  if (length(heading) == 0L) {
    return(character())
  }
  section <- lines[seq(heading[length(heading)] + 1L, length(lines))]
  ends <- grep("\\[ FAIL [0-9]+ \\|", section, useBytes = TRUE)
  if (length(ends) > 0L) {
    section <- section[seq_len(ends[1] - 1L)]
  }
  reported <- grep(
    "^\\S.*\\([0-9]+\\):",
    section,
    value = TRUE,
    useBytes = TRUE
  )
  trimws(sub("\\s*\\([0-9]+\\):.*$", "", sub("^[^[:alnum:]{]+", "", reported)))
}

reasons <- skip_reasons(test_log)
unexplained <- setdiff(reasons, permitted)

# A skip the log counted but whose reason this script could not read is not
# evidence of anything, and treating it as clean would make the whole check
# depend on a report format staying put.
unattributed <- counts[["skip"]] > 0L && length(reasons) == 0L

write_step_summary(c(
  sprintf("## %s suite soundness", label),
  "",
  sprintf(
    "%s Withheld: %s.",
    tally$line,
    withheld_label
  ),
  "",
  if (length(reasons) == 0L) {
    "No skips were reported."
  } else {
    paste0(
      "- ", ifelse(reasons %in% permitted, "OK", "UNEXPECTED"),
      " ", reasons
    )
  }
))

problems <- character()
if (counts[["fail"]] > 0L) {
  problems <- c(problems, sprintf(
    "%d test(s) failed.",
    counts[["fail"]]
  ))
}
if (counts[["pass"]] == 0L) {
  problems <- c(problems, "The suite passed nothing, so it proved nothing.")
}
if (unattributed) {
  problems <- c(problems, sprintf(
    paste0(
      "%d skip(s) were counted but no skip reasons could be read from the ",
      "log, so none of them can be attributed to a withheld backend."
    ),
    counts[["skip"]]
  ))
}
if (length(unexplained) > 0L) {
  problems <- c(problems, sprintf(
    paste0(
      "These skips are not explained by a backend this job withheld (%s): ",
      "%s. A `backend` job exists to execute its tests, so any other skip ",
      "means it did not."
    ),
    withheld_label,
    paste(sprintf("\"%s\"", unexplained), collapse = "; ")
  ))
}

if (length(problems) > 0L) {
  stop(call. = FALSE, sprintf("%s: %s", label, paste(problems, collapse = " ")))
}

message(sprintf(
  "Verified: %s ran the suite -- %s -- and every skip was one of %s.",
  label,
  tally$line,
  withheld_label
))
