# Replay the five non-PostgreSQL minimal signals sequentially, without a suite.
root <- "investigation/2026-11-03-summary-grouping-share-bug-hunt"
targets <- c(
  "summary/reproduce-pick-forwarding.R",
  "summary/reproduce-vector-names.R",
  "grouping/reproduce-warning-preflight.R",
  "share/list2env-unresolved-reproduction.R",
  "share/dtplyr-list2env-reproduction.R"
)
rows <- lapply(targets, function(target) {
  result <- processx::run(file.path(R.home("bin"), "Rscript"),
    c(file.path(root, target)), error_on_status = FALSE)
  name <- sub("[.]R$", "", basename(target))
  writeLines(c(result$stdout, result$stderr), file.path(root,
    paste0("coordinator-", name, ".log")))
  cat(target, "exit", result$status, "\n")
  data.frame(target, exit_status = result$status)
})
statuses <- do.call(rbind, rows)
utils::write.csv(statuses, file.path(root, "coordinator-replay.csv"), row.names = FALSE)
stopifnot(all(statuses$exit_status == 0L))
