# A disposable package measures R CMD check's treatment of the prototype.
args <- commandArgs(trailingOnly = TRUE)
probe <- if (length(args)) args[[1L]] else {
  "investigation/condition-handler-replay-2026-09-30/probe.R"
}
root <- tempfile("marginplyr-replay-check-")
dir.create(root)
pkg <- file.path(root, "replayprobe")
dir.create(pkg)
dir.create(file.path(pkg, "R"))
writeLines(c(
  "Package: replayprobe",
  "Version: 0.0.1",
  "Title: Replay Probe",
  "Description: Experimental replay handler probe.",
  "License: GPL-3",
  "Author: Research",
  "Maintainer: Research <research@example.invalid>"
), file.path(pkg, "DESCRIPTION"))
writeLines(character(), file.path(pkg, "NAMESPACE"))
source_text <- readLines(probe)
start <- which(startsWith(source_text, "failure <-"))[[1L]]
writeLines(source_text[seq_len(start - 1L)], file.path(pkg, "R", "probe.R"))
old_dir <- setwd(root)
status <- system2(file.path(R.home("bin"), "R"), c(
  "CMD", "check", "--no-manual", "--no-vignettes", shQuote(pkg)
), stdout = TRUE, stderr = TRUE, env = "_R_CHECK_CRAN_INCOMING_REMOTE_=false")
setwd(old_dir)
cat(status, sep = "\n")
stopifnot(any(grepl("Packages should not call .Internal()", status, fixed = TRUE)))
cat("\nInternal-call warning confirmed; disposable evidence:", root, "\n")
