#!/usr/bin/env Rscript
# Validates this checkout's registration without executing package tests.
argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(argument) != 1L || length(commandArgs(trailingOnly = TRUE))) {
  stop("Usage: Rscript tools/verify-test-priority.R", call. = FALSE)
}
entry <- normalizePath(sub("^--file=", "", argument[[1L]]), mustWork = TRUE)
source(file.path(dirname(entry), "review-ready-priority.R"))
invisible(review_ready_priority_validate(getwd()))
cat("Test priority registration passed.\n")
