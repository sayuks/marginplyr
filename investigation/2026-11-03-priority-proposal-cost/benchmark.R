# Acceptance-only empty-task harness: no suite execution or priority adoption.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
root <- normalizePath(args[[1L]], mustWork = TRUE)
output <- normalizePath(args[[2L]], mustWork = TRUE)
source(file.path(root, "tools", "review-ready-timing.R"))
source(file.path(root, "tools", "review-ready-priority.R"))
workspace <- tempfile("priority-cost-")
dir.create(workspace)
source_root <- file.path(workspace, "source")
dir.create(file.path(source_root, "tests"), recursive = TRUE)
stopifnot(file.copy(file.path(root, "tests", "testthat"), file.path(source_root, "tests"), recursive = TRUE))
stopifnot(file.copy(file.path(root, "tests", "testthat.R"), file.path(source_root, "tests")))
files <- testthat::find_test_scripts(file.path(source_root, "tests", "testthat"), full.names = FALSE)
unlink(file.path(source_root, "tests", "testthat", tail(files, length(files) - 196L)))
files <- head(files, 196L)
stopifnot(length(files) == 196L, file.copy(file.path(root, "DESCRIPTION"), source_root))
metadata <- list(version = 1L, scope = "full", sha = strrep("a", 40L),
  started_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"))
terminal <- c(metadata, list(status = "passed", heavy_check = "passed"))
elapsed <- function(action) {
  start <- proc.time()[["elapsed"]]
  action()
  proc.time()[["elapsed"]] - start
}
rows <- vector("list", 10L)
for (sample in seq_along(rows)) {
  run <- file.path(workspace, paste0("sample-", sample))
  timing <- file.path(run, "timing")
  prep <- elapsed(function() stopifnot(review_ready_timing_prepare(source_root, timing, metadata)))
  saveRDS(metadata, file.path(run, "invocation.rds"))
  saveRDS(terminal, file.path(run, "result.rds"))
  Sys.setenv(MARGINPLYR_REVIEW_TIMING = timing)
  setup <- 0
  for (worker in seq_len(4L)) {
    setup <- setup + elapsed(function() {
      invisible(capture.output(suppressMessages(stopifnot(review_ready_timing_install()))))
    })
    invisible(capture.output(suppressMessages(untrace("queue_task", where = asNamespace("testthat")))))
  }
  # Reuse production recorder expressions, replacing only the work target and
  # shape predicate so that 196 empty tasks do not execute real package tests.
  target <- new.env(parent = globalenv())
  target$queue_task <- function(path) invisible(NULL)
  helper <- new.env(parent = globalenv())
  helper$target <- target
  code <- gsub('asNamespace("testthat")', "target",
    readLines(file.path(root, "tools", "review-ready-timing.R")), fixed = TRUE)
  eval(parse(text = code), envir = helper)
  helper$review_ready_timing_supported <- function(...) TRUE
  invisible(capture.output(suppressMessages(stopifnot(helper$review_ready_timing_install()))))
  plain <- function(path) invisible(NULL)
  baseline <- elapsed(function() for (path in files) plain(path))
  recording <- elapsed(function() for (path in files) target$queue_task(path))
  writeLines(paste(files, 0, 1, sep = "\t"), file.path(timing, "file-results.tsv"))
  parsing <- elapsed(function() invisible(capture.output(review_ready_timing_finish(timing, terminal,
    "Starting 4 test processes."))))
  proposal <- elapsed(function() {
    result <- NULL
    invisible(capture.output({ result <- review_ready_priority_propose(source_root, run) }))
    stopifnot(isTRUE(result$eligible))
  })
  rows[[sample]] <- data.frame(sample = sample, prepare_s = prep,
    setup_four_s = setup, recording_increment_s = recording - baseline,
    parsing_s = parsing, proposal_s = proposal,
    combined_s = prep + setup + recording - baseline + parsing + proposal)
}
results <- do.call(rbind, rows)[-1L, ]
write.csv(results, file.path(output, "samples.csv"), row.names = FALSE)
sink(file.path(output, "summary.txt"))
cat("196 files, four real hook installs per sample; serial empty-task recording.\n")
cat("First sample discarded as warmup; nine retained samples.\n")
cat("Included preparation, four setups, recording delta, both evidence parses, calculation and output.\n")
cat("Excluded process startup and actual test work; no full suite executed.\n")
print(results)
print(vapply(results[-1L], function(x) c(min = min(x), median = median(x), max = max(x)), numeric(3)))
print(tools::md5sum(file.path(root, "tools", c("review-ready-timing.R", "review-ready-priority.R"))))
print(sessionInfo())
sink()
cat(readLines(file.path(output, "summary.txt")), sep = "\n")
stopifnot(median(results$combined_s) < 1)
unlink(workspace, recursive = TRUE)
