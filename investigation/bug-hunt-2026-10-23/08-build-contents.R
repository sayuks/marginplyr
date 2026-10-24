# Investigated 2026-10-23 at 0e3f1dcca46f4d51d4d99cd200cf73540c96133d.
# Pass the source tarball produced by the build documented in the report.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, file.exists(args[[1L]]))
members <- sub("/+$", "", utils::untar(args[[1L]], list = TRUE))
unwanted <- members[grepl("/.worktrees/", members, fixed = TRUE)]
cat("Worktree descendants in source tarball:", length(unwanted), "\n")
stopifnot("Local worktrees must not enter a source tarball" = length(unwanted) == 0L)
