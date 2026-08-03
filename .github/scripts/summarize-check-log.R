# Copies a finished check's status and every ERROR, WARNING, and NOTE into the
# GitHub job summary.
#
# `check-r-package` uploads the check directory as an artifact, but reading a
# NOTE then costs a download and an unzip, which is enough friction that NOTEs
# go unread between releases. This puts the same information on the run page.
# Unlike `check-tarball.R`, which drives rcmdcheck itself, this reads the log
# an already-completed check left behind, so it can be bolted onto a workflow
# that uses the standard action.

source(".github/scripts/ci-helpers.R")

rcheck <- rcheck_directory(required = FALSE)
log_path <- file.path(rcheck, "00check.log")
if (is.na(rcheck) || !file.exists(log_path)) {
  message("No check log under '", check_directory(), "'; nothing to summarize.")
  quit(save = "no")
}

check_log <- readLines(log_path, warn = FALSE)

# Check results are one line per test, so a result and its detail lines run
# from a line ending in ERROR/WARNING/NOTE up to the next line starting a new
# test. Keeping the detail is the point: "1 NOTE" alone is not auditable.
starts <- grep("^[*] .*(ERROR|WARNING|NOTE)$", check_log)
boundaries <- grep("^[*] ", check_log)
findings <- unlist(lapply(starts, function(start) {
  next_boundary <- boundaries[boundaries > start]
  end <- if (length(next_boundary) == 0L) {
    length(check_log)
  } else {
    next_boundary[1] - 1L
  }
  as_summary_block(paste(check_log[start:end], collapse = "\n"))
}))

status <- grep("^Status:", check_log, value = TRUE)
summary_lines <- c(
  sprintf("## %s", check_label()),
  "",
  if (length(status) == 0L) "No status line in the check log." else status,
  "",
  findings
)

write_step_summary(summary_lines)
