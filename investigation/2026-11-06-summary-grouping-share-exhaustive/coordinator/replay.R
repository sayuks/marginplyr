# Replay research witnesses against a disposable source archive.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
source_root <- normalizePath(args[[1L]], mustWork = TRUE)
artifacts <- normalizePath(args[[2L]], mustWork = TRUE)
output <- args[[3L]]
dir.create(output, recursive = TRUE, showWarnings = FALSE)
output <- normalizePath(output, mustWork = TRUE)
copied <- file.path(output, "harnesses")
dir.create(copied, showWarnings = FALSE)
stopifnot(all(file.copy(list.files(artifacts, full.names = TRUE), copied,
                       recursive = TRUE)))
checks <- list(
  list("summary/frame-casts.R", character(), 0L),
  list("summary/frame-casts.R", "--assert", 1L),
  list("summary/frame-casts.R", c("--assert", "--named-control"), 0L),
  list("summary/frame-cast-matrix.R", character(), 0L),
  list("grouping/callee-rebinding.R", character(), 0L),
  list("grouping/callee-rebinding.R", "--assert", 1L),
  list("grouping/callee-rebinding-controls.R", character(), 0L),
  list("share/closure-values.R", character(), 0L),
  list("share/closure-values.R", c("syntax", "--assert"), 1L),
  list("share/closure-values.R", c("transfer", "--assert"), 1L),
  list("share/closure-values.R", c("mutation", "--assert"), 1L),
  list("share/closure-values.R", c("--assert", "--controls-only"), 0L),
  list("share/environment-aliases.R", character(), 0L),
  list("share/environment-aliases.R", "--assert", 1L),
  list("share/environment-aliases.R", c("--assert", "--controls-only"), 0L)
)
rows <- vector("list", length(checks))
for (i in seq_along(checks)) {
  check <- checks[[i]]
  script <- file.path(copied, check[[1L]])
  stopifnot(file.exists(script))
  result <- processx::run(file.path(R.home("bin"), "Rscript"),
    c(script, check[[2L]]), wd = source_root, timeout = 180000,
    error_on_status = FALSE, cleanup_tree = TRUE)
  writeLines(c(result$stdout, result$stderr),
             file.path(output, sprintf("replay-%02d.log", i)))
  rows[[i]] <- data.frame(script = check[[1L]],
    args = paste(check[[2L]], collapse = " "),
    digest = digest::digest(file = script, algo = "sha256"),
    expected_status = check[[3L]], observed_status = result$status)
  cat(i, check[[1L]], paste(check[[2L]], collapse = " "),
      "expected", check[[3L]], "observed", result$status, "\n")
  flush.console()
}
results <- do.call(rbind, rows)
write.csv(results, file.path(output, "replay-results.csv"), row.names = FALSE)
stopifnot(identical(results$expected_status, results$observed_status))
