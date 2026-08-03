# Copies a finished check's status and every ERROR, WARNING, and NOTE into the
# GitHub job summary.
#
# `check-r-package` uploads the check directory as an artifact, but reading a
# NOTE then costs a download and an unzip, which is enough friction that NOTEs
# go unread between releases. This puts the same information on the run page.
# Unlike `check-tarball.R`, which drives rcmdcheck itself, this reads the log
# an already-completed check left behind, so it can be bolted onto a workflow
# that uses the standard action.

check_dir <- Sys.getenv("MARGINPLYR_CHECK_DIR", "check")
label <- Sys.getenv("MARGINPLYR_CHECK_LABEL", "R CMD check")

log_paths <- Sys.glob(file.path(check_dir, "*.Rcheck", "00check.log"))
if (length(log_paths) == 0L) {
  message("No check log under '", check_dir, "'; nothing to summarize.")
  quit(save = "no")
}

check_log <- readLines(log_paths[1], warn = FALSE)

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
  c("```", check_log[start:end], "```", "")
}))

status <- grep("^Status:", check_log, value = TRUE)
summary_lines <- c(
  sprintf("## %s", label),
  "",
  if (length(status) == 0L) "No status line in the check log." else status,
  "",
  findings
)

summary_path <- Sys.getenv("GITHUB_STEP_SUMMARY", "")
if (nzchar(summary_path)) {
  write(summary_lines, file = summary_path, append = TRUE)
}
cat(summary_lines, sep = "\n")
