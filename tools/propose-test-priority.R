#!/usr/bin/env Rscript
# Recalculates an identified run's proposal against this worktree without editing it.
argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(argument) != 1L) stop("Cannot locate the priority proposal command.", call. = FALSE)
entry <- normalizePath(sub("^--file=", "", argument[[1L]]), mustWork = TRUE)
root <- dirname(dirname(entry))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || args[[1L]] != "--run") {
  stop("Usage: Rscript tools/propose-test-priority.R --run RUN_DIRECTORY", call. = FALSE)
}
source(file.path(root, "tools", "review-ready-timing.R"))
source(file.path(root, "tools", "review-ready-priority.R"))
proposal <- review_ready_priority_propose(root, normalizePath(args[[2L]], mustWork = TRUE))
quit(status = if (isTRUE(proposal$eligible)) 0L else 1L, save = "no")
