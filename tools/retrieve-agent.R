# Local entry: source relative to this script, independent of the current directory.
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
source(file.path(dirname(normalizePath(script)), "retrieve-agent-lib.R"))
tryCatch(retrieval_main(commandArgs(trailingOnly = TRUE)), error = function(error) {
  # Parser diagnostics may quote an oversized record; errors expose only our summary.
  message("Retrieval failed: ", substr(conditionMessage(error), 1L, 400L))
  quit(status = 1L)
})
