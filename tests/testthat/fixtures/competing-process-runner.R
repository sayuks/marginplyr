# Controlled checkpoints and stalls at the process runner's own boundary.
args <- commandArgs(TRUE)
if (args[[2L]] == "missing") {
  cat("startup failed\n")
  quit(status = 7L)
}
saveRDS(list(events = "checkpoint", observed = list()), args[[5L]])
cat("checkpoint written\n")
if (args[[2L]] == "slow") Sys.sleep(2)
if (args[[2L]] == "exit") quit(status = 1L)
