#!/usr/bin/env Rscript

# Prepares local JSON support before invoking the non-installing formal gate.
argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(argument) != 1L) stop("Cannot locate local preflight.", call. = FALSE)
entry <- normalizePath(sub("^--file=", "", argument[[1L]]), mustWork = TRUE)
root <- dirname(dirname(entry))
source(file.path(root, "tools", "duckdb-json.R"))
source(file.path(root, "tools", "cran-preflight-lib.R"))
status <- tryCatch(
  run_local_cran_preflight(commandArgs(trailingOnly = TRUE), root),
  interrupt = function(cnd) 130L,
  error = function(cnd) {
    cat("Local preflight unavailable: ", conditionMessage(cnd), "\n",
        file = stderr(), sep = "")
    2L
  }
)
quit(status = status, save = "no")
