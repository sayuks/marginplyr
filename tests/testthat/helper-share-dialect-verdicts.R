# The per-dialect verdict cache is package state, so a test that reads what a
# call records starts from an empty one -- what a call records is only
# observable from there -- and puts back whatever the rest of the suite had
# recorded. Restoring empties first, so that an entry the test itself wrote is
# not left beside the saved ones.
#
# Shared rather than written where they are used because testthat gives each
# test file its own environment, so a second file reaching this cache would
# otherwise carry a copy of both bodies. `test-share-backends.R` reads the
# cache to assert what is recorded under a dialect, and
# `test-sent-queries.R` empties it so that the probe sends its queries at all.
empty_share_dialect_verdicts <- function() {
  rm(
    list = ls(share_dialect_verdicts, all.names = TRUE),
    envir = share_dialect_verdicts
  )
}

restore_share_dialect_verdicts <- function(saved) {
  empty_share_dialect_verdicts()
  list2env(saved, envir = share_dialect_verdicts)
  invisible(NULL)
}
