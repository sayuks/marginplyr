# Component acceptance benchmark; no package test files are executed.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
root <- normalizePath(args[[1L]], mustWork = TRUE)
output <- normalizePath(args[[2L]], mustWork = TRUE)
source(file.path(root, "tools/review-ready-timing.R"))
helper <- readLines(file.path(root, "tools/review-ready-timing.R"))
files <- head(testthat::find_test_scripts(file.path(root, "tests/testthat"), full.names = FALSE), 196L)
stopifnot(length(files) == 196L)
clock <- function(code) {
  start <- proc.time()[["elapsed"]]
  force(code)
  proc.time()[["elapsed"]] - start
}
cluster <- parallel::makePSOCKcluster(4L, outfile = file.path(output, "workers.log"))
tryCatch({
  worker_samples <- parallel::clusterApply(cluster, seq_len(4L), function(index, helper, files, output) {
    # Retain the supported body, with its collaborators doing no file work.
    target <- new.env(parent = asNamespace("testthat"))
    target$queue_task <- get("queue_task", asNamespace("testthat"))
    environment(target$queue_task) <- target
    target$test_one_file <- function(...) NULL
    target$with_reporter <- function(reporters, code) force(code)
    target$SubprocessSnapshotReporter <- target$SubprocessReporter <- target$MultiReporter <-
      list(new = function(...) NULL)
    target$the <- list(testing_env = new.env())
    context <- new.env(parent = globalenv())
    context$target <- target
    adapted <- gsub('asNamespace("testthat")', "target", helper, fixed = TRUE)
    clock <- function(code) {
      start <- proc.time()[["elapsed"]]
      force(code)
      proc.time()[["elapsed"]] - start
    }
    assigned <- files[seq(index, length(files), by = 4L)]
    samples <- vector("list", 9L)
    calls <- function() for (path in assigned) target$queue_task(path)
    calls()
    for (sample in seq_len(9L)) {
      directory <- file.path(output, paste0("sample-", sample))
      dir.create(directory, showWarnings = FALSE)
      Sys.setenv(MARGINPLYR_REVIEW_TIMING = directory)
      # Alternating order keeps baseline drift visible in the distribution.
      if (sample %% 2L) baseline <- clock(for (batch in 1:5) calls())
      setup <- clock({
        eval(parse(text = adapted), envir = context)
        invisible(context$review_ready_timing_install())
      })
      recorded <- clock(for (batch in 1:5) calls())
      invisible(utils::capture.output(untrace("queue_task", where = target)))
      if (!(sample %% 2L)) baseline <- clock(for (batch in 1:5) calls())
      records <- readLines(file.path(directory, paste0(Sys.getpid(), ".tsv")))
      stopifnot(length(records) == 2L * length(assigned) * 5L)
      samples[[sample]] <- data.frame(sample = sample, worker = index,
        files = length(assigned), setup_s = setup, baseline_s = baseline / 5,
        recording_s = recorded / 5, incremental_s = (recorded - baseline) / 5)
    }
    do.call(rbind, samples)
  }, helper = helper, files = files, output = output)
}, finally = parallel::stopCluster(cluster)) -> workers
workers <- do.call(rbind, workers)
write.csv(workers, file.path(output, "worker-samples.csv"), row.names = FALSE)
# Full preparation cost captures every pinned test input, including helpers/data.
metadata <- list(sha = trimws(system2("git", c("-C", shQuote(root), "rev-parse", "HEAD"), stdout = TRUE)),
  started_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"))
prepare <- vapply(seq_len(9L), function(sample) {
  clock(stopifnot(review_ready_timing_prepare(root, file.path(output, paste0("manifest-", sample)), metadata)))
}, numeric(1))
aggregate <- aggregate(cbind(setup_s, incremental_s) ~ sample, workers, sum)
aggregate$prepare_s <- prepare
aggregate$total_s <- aggregate$setup_s + aggregate$incremental_s + aggregate$prepare_s
write.csv(aggregate, file.path(output, "aggregate-samples.csv"), row.names = FALSE)
writeLines(c(paste("SHA:", metadata$sha),
  paste("Recorder MD5:", unname(tools::md5sum(file.path(root, "tools/review-ready-timing.R"))))),
  file.path(output, "identity.txt"))
capture.output(sessionInfo(), file = file.path(output, "session.txt"))
print(aggregate)
cat("Conservative serial sum across four workers, plus input preparation:\n")
print(c(min = min(aggregate$total_s), median = median(aggregate$total_s), max = max(aggregate$total_s)))
stopifnot(median(aggregate$total_s) < 1)
