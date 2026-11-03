# Compare controlled notification and reached-checkpoint SIGINT for #756.
args <- commandArgs(TRUE)
entry <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
root <- dirname(dirname(dirname(normalizePath(entry))))
source(file.path(root, "tools/interruption-supervisor.R"))
cases <- list()
for (mode in c("controlled", "sigint", "healthy")) {
  for (warn in c(1L, 2L)) for (warnings in c(FALSE, TRUE)) {
    cases[[length(cases) + 1L]] <- list(operation = "summary", mode = mode, warn = warn, warnings = warnings)
  }
  for (flag in c(FALSE, TRUE)) for (outer in c(FALSE, TRUE)) {
    for (commit in if (outer) c(FALSE, TRUE) else FALSE) {
      cases[[length(cases) + 1L]] <- list(operation = "sqlite", mode = mode, flag = flag, outer = outer, commit = commit)
    }
  }
}
tryCatch({
  if (length(args) != 1L) stop("Usage: Rscript tools/competing-conditions/run.R <evidence directory>")
  supervise_interruption(root, args[[1L]], cases, "competing-conditions", timeout = 30)
}, error = function(cnd) { message(conditionMessage(cnd)); quit(status = 1L) })
