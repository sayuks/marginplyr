args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
source_root <- normalizePath(args[[1L]], mustWork = TRUE)
output_root <- normalizePath(args[[2L]], mustWork = TRUE)
data_root <- file.path(source_root, "investigation",
  "2026-11-02-review-ready-order-calibration")
helper <- file.path(data_root, "helper-order-calibration.R")
files <- read.csv(file.path(data_root, "files.csv"))$file
stopifnot(length(files) == 196L)
batches <- 5L
samples <- 9L
elapsed <- function(fun) {
  begin <- Sys.time()
  fun()
  as.numeric(difftime(Sys.time(), begin, units = "secs"))
}
run_calls <- function(target) {
  for (batch in seq_len(batches)) {
    for (path in files) target$queue_task(path)
  }
  invisible(NULL)
}
plain_target <- new.env(parent = globalenv())
plain_target$queue_task <- function(path) invisible(NULL)
recording_target <- new.env(parent = globalenv())
recording_target$queue_task <- function(path) invisible(NULL)
helper_context <- new.env(parent = globalenv())
helper_context$recording_target <- recording_target
helper_text <- readLines(helper, warn = FALSE)
adapted_text <- gsub('asNamespace("testthat")', "recording_target",
  helper_text, fixed = TRUE)
stopifnot(!identical(helper_text, adapted_text))
Sys.setenv(MARGINPLYR_REVIEW_PROGRESS = file.path(output_root,
  "warmup", "test-progress.tsv"))
invisible(capture.output(suppressMessages(eval(parse(text = adapted_text),
  envir = helper_context))))
run_calls(plain_target)
for (path in head(files, 5L)) recording_target$queue_task(path)
rows <- vector("list", samples)
for (sample in seq_len(samples)) {
  run_root <- file.path(output_root, sprintf("recording-%02d", sample))
  Sys.setenv(MARGINPLYR_REVIEW_PROGRESS = file.path(run_root,
    "test-progress.tsv"))
  invisible(gc())
  if (sample %% 2L) {
    recorded <- elapsed(function() run_calls(recording_target))
    plain <- elapsed(function() run_calls(plain_target))
  } else {
    plain <- elapsed(function() run_calls(plain_target))
    recorded <- elapsed(function() run_calls(recording_target))
  }
  log_file <- file.path(run_root, "file-timings",
    paste0(Sys.getpid(), ".tsv"))
  records <- readLines(log_file, warn = FALSE)
  stopifnot(length(records) == 2L * length(files) * batches)
  rows[[sample]] <- data.frame(sample = sample,
    baseline_196_s = plain / batches,
    recording_196_s = recorded / batches,
    increment_196_s = (recorded - plain) / batches,
    bytes_196 = unname(file.info(log_file)$size) / batches,
    lines_196 = length(records) / batches)
}
results <- do.call(rbind, rows)
write.csv(results, file.path(output_root, "recording-samples.csv"),
  row.names = FALSE)
real_namespace <- asNamespace("testthat")
init_rows <- vector("list", samples)
for (sample in seq_len(samples)) {
  install <- elapsed(function() {
    invisible(capture.output(suppressMessages(sys.source(helper,
      envir = new.env(parent = globalenv())))))
  })
  remove <- elapsed(function() {
    invisible(capture.output(suppressMessages(untrace("queue_task",
      where = real_namespace))))
  })
  init_rows[[sample]] <- data.frame(sample = sample,
    install_one_worker_s = install, remove_s = remove)
}
initialization <- do.call(rbind, init_rows)
write.csv(initialization, file.path(output_root, "initialization-samples.csv"),
  row.names = FALSE)
summary <- function(x) c(min = min(x), median = median(x), max = max(x))
sink(file.path(output_root, "recording-summary.txt"))
cat("Run date UTC:", format(Sys.time(), tz = "UTC"), "\n")
cat("Source SHA: 68fb4775bfe10370f45a5f9b0472f77cfe1db003\n")
cat("Helper MD5:", unname(tools::md5sum(helper)), "\n")
cat("No package tests executed; empty traced queue_task harness.\n")
cat("Each sample: 5 x 196 calls, reported per 196 files.\n")
cat("Recording incremental seconds per 196 files:\n")
print(summary(results$increment_196_s))
cat("Recording total seconds per 196 files:\n")
print(summary(results$recording_196_s))
cat("Recording bytes per 196 files:\n")
print(summary(results$bytes_196))
cat("Real testthat trace install seconds per worker:\n")
print(summary(initialization$install_one_worker_s))
print(sessionInfo())
sink()
cat(readLines(file.path(output_root, "recording-summary.txt")), sep = "\n")
