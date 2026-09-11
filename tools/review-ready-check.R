#!/usr/bin/env Rscript

# Runs the fixed local gate for one reviewable package commit. The implementation
# lives beside this entry point so its command contract can be fixture-tested.

file_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(file_argument) != 1L) {
  cat("Unable to locate tools/review-ready-check.R.\n", file = stderr())
  quit(status = 2L, save = "no")
}

script_path <- normalizePath(
  sub("^--file=", "", file_argument[[1L]]),
  mustWork = TRUE
)
repository_path <- dirname(dirname(script_path))

source(file.path(repository_path, ".github", "scripts", "cran-note-policy.R"))
source(file.path(repository_path, "tools", "review-ready-check-lib.R"))

status <- review_ready_check_cli(
  commandArgs(trailingOnly = TRUE),
  expected_root = repository_path
)
quit(status = status, save = "no")
