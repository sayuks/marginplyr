# Supplements the worker benchmark with actual-run reporter volume and parsing.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 4L)
root <- normalizePath(args[[1L]], mustWork = TRUE)
run <- normalizePath(args[[2L]], mustWork = TRUE)
baseline <- normalizePath(args[[3L]], mustWork = TRUE)
output <- normalizePath(args[[4L]], mustWork = TRUE)
source(file.path(root, "tools/review-ready-timing.R"))
source(file.path(root, "tools/review-ready-progress.R"))
current_factory <- source(file.path(root, "tools/review-ready-progress-reporter.R"),
  local = new.env())$value
baseline_factory <- source(baseline, local = new.env())$value
terminal <- readRDS(file.path(run, "result.rds"))
stopifnot(identical(terminal$status, "passed"), identical(terminal$scope, "full"))
timing <- file.path(run, "timing")
results <- read.delim(file.path(timing, "file-results.tsv"), header = FALSE,
  col.names = c("file", "skip", "other"))
stopifnot(nrow(results) >= 196L, all(results$skip == 0L))
# Use all successful-run expectations across only 196 boundaries, conservatively.
counts <- results$other[seq_len(196L)]
counts[[196L]] <- counts[[196L]] + sum(results$other[-seq_len(196L)])
files <- results$file[seq_len(196L)]
success <- testthat::capture_expectation(testthat::expect_true(TRUE))
transcript <- review_ready_test_transcript(file.path(run, "check"))
clock <- function(code) {
  start <- proc.time()[["elapsed"]]
  force(code)
  proc.time()[["elapsed"]] - start
}
local({
  workspace <- tempfile("controller-recording-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE))
  old <- Sys.getenv("MARGINPLYR_REVIEW_TIMING", unset = NA_character_)
  on.exit(if (is.na(old)) Sys.unsetenv("MARGINPLYR_REVIEW_TIMING") else
    Sys.setenv(MARGINPLYR_REVIEW_TIMING = old), add = TRUE)
  replay <- function(factory, directory) {
    dir.create(directory)
    Sys.setenv(MARGINPLYR_REVIEW_TIMING = directory)
    reporter <- factory(file.path(directory, "progress.tsv"))
    invisible(capture.output({
      reporter$start_reporter()
      for (i in seq_along(files)) {
        reporter$start_file(files[[i]])
        for (j in seq_len(counts[[i]])) {
          reporter$add_result(context = NULL, test = "recording cost", result = success)
        }
        reporter$end_file()
      }
      reporter$end_reporter()
    }))
  }
  samples <- vector("list", 9L)
  for (sample in seq_len(9L)) {
    plain <- file.path(workspace, paste0("baseline-", sample))
    recorded <- file.path(workspace, paste0("recorded-", sample))
    if (sample %% 2L) baseline_s <- clock(replay(baseline_factory, plain))
    recording_s <- clock(replay(current_factory, recorded))
    if (!(sample %% 2L)) baseline_s <- clock(replay(baseline_factory, plain))
    actual <- read.delim(file.path(recorded, "file-results.tsv"), header = FALSE)
    stopifnot(nrow(actual) == 196L, sum(actual[[3L]]) == sum(results$other))
    finalize_s <- clock({
      evidence <- review_ready_timing_evidence(timing, terminal, transcript)
      stopifnot(isTRUE(evidence$eligible), nrow(evidence$files) == nrow(results))
      saveRDS(evidence, file.path(workspace, "evidence.rds"))
    })
    samples[[sample]] <- data.frame(sample = sample, files = 196L,
      expectations = sum(results$other), baseline_s = baseline_s,
      recording_s = recording_s, incremental_s = recording_s - baseline_s,
      actual_parse_files = nrow(results), finalize_s = finalize_s)
  }
  samples <- do.call(rbind, samples)
  write.csv(samples, file.path(output, "controller-samples.csv"), row.names = FALSE)
  cat("Measured SHA:", terminal$sha, "\n")
  cat("Baseline reporter MD5:", unname(tools::md5sum(baseline)), "\n")
  print(samples)
  print(c(min = min(samples$incremental_s + samples$finalize_s),
    median = median(samples$incremental_s + samples$finalize_s),
    max = max(samples$incremental_s + samples$finalize_s)))
  capture.output(sessionInfo(), file = file.path(output, "controller-session.txt"))
})
