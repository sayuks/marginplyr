#!/usr/bin/env Rscript
# Review-ready selection protocol: 1

# Runs the fixed local gate for one reviewable package commit. The implementation
# lives beside this entry point so its command contract can be fixture-tested.

# Current direct invocations join the shared boundary; older unmodified entry
# points still need the instruction migration described in the admission guide.
if (!nzchar(Sys.getenv("MARGINPLYR_REVIEW_REQUEST"))) {
  if (length(commandArgs(trailingOnly = TRUE)) > 0L) {
    cat("The review-ready check takes no arguments.\n", file = stderr())
    quit(status = 2L, save = "no")
  }
  launcher <- Sys.which("marginplyr-review-ready")
  if (!nzchar(launcher)) {
    cat(
      "Install marginplyr-review-ready with python3 tools/install-review-ready.py ",
      "and expose it on PATH. No check was run.\n",
      file = stderr()
    )
    quit(status = 2L, save = "no")
  }
  status <- system2(launcher)
  quit(status = status, save = "no")
}

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
source(file.path(repository_path, "tools", "dependency-requirements.R"))
source(file.path(repository_path, "tools", "required-test-suggests.R"))
source(file.path(repository_path, "tools", "review-ready-progress.R"))
source(file.path(repository_path, "tools", "review-ready-check-lib.R"))

status <- review_ready_check_cli(
  commandArgs(trailingOnly = TRUE),
  expected_root = repository_path,
  selection = read_review_ready_selection()
)
quit(status = status, save = "no")
