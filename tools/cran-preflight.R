#!/usr/bin/env Rscript

# Runs the one local release gate described by issue #505. It reads a clean
# commit into an external evidence directory and never installs, documents,
# updates, dispatches, submits, or releases anything.

file_argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(file_argument) != 1L) {
  cat("Unable to locate tools/cran-preflight.R.\n", file = stderr())
  quit(status = 2L, save = "no")
}

script_path <- normalizePath(
  sub("^--file=", "", file_argument[[1L]]),
  mustWork = TRUE
)
repository_path <- dirname(dirname(script_path))

# The policy check has no package dependencies, so a drifted or incomplete
# agent sandbox stops before any preflight work begins.
policy_verified <- tryCatch(
  {
    agent_policy_repository_path <- repository_path
    source(
      file.path(
        repository_path,
        ".github",
        "scripts",
        "verify-agent-network-policy.R"
      ),
      local = TRUE
    )
    TRUE
  },
  error = function(condition) {
    cat(conditionMessage(condition), "\n", file = stderr(), sep = "")
    FALSE
  }
)
if (!policy_verified) {
  quit(status = 2L, save = "no")
}

source(file.path(repository_path, ".github", "scripts", "cran-note-policy.R"))
source(file.path(repository_path, "tools", "dependency-requirements.R"))
source(file.path(repository_path, "tools", "cran-preflight-lib.R"))

status <- cran_preflight_cli(
  commandArgs(trailingOnly = TRUE),
  expected_root = repository_path
)
quit(status = status, save = "no")
