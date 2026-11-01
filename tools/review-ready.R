#!/usr/bin/env Rscript
# Repository-owned Review-ready entry protocol: 4. No check/skip path options.
file_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(file_argument) != 1L) stop("Cannot locate the repository runner.", call. = FALSE)
entry <- normalizePath(sub("^--file=", "", file_argument[[1L]]), mustWork = TRUE)
runner_root <- dirname(dirname(entry))
for (name in c("core", "registry", "evidence", "manager")) {
  source(file.path(runner_root, "tools", paste0("review-ready-", name, ".R")))
}
status <- tryCatch(rr_entry(commandArgs(trailingOnly = TRUE), runner_root), error = function(e) {
  rr_say("Review-ready setup/management failed: ", conditionMessage(e), "; no passing check evidence.")
  2L
})
quit(status = status, save = "no")
