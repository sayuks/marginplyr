#!/usr/bin/env Rscript
# Repository-owned command for source-only or full pinned verification.
argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(argument) != 1L) stop("Cannot locate the repository runner.", call. = FALSE)
entry <- normalizePath(sub("^--file=", "", argument[[1L]]), mustWork = TRUE)
root <- dirname(dirname(entry))
for (relative in c(".github/scripts/cran-note-policy.R", "tools/dependency-requirements.R",
    "tools/required-test-suggests.R", "tools/review-ready-progress.R", "tools/review-ready-check-lib.R")) {
  source(file.path(root, relative))
}
quit(status = review_ready_check_cli(commandArgs(trailingOnly = TRUE), root), save = "no")
