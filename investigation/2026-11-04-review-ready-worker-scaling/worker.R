args <- commandArgs(trailingOnly = TRUE)
job <- readRDS(args[[1L]])
for (relative in c('.github/scripts/cran-note-policy.R',
    'tools/review-ready-check-lib.R', 'tools/review-ready-progress.R')) {
  source(file.path(job$source, relative))
}
phases <- list()
original_build <- build_review_ready_tarball
build_review_ready_tarball <- function(...) {
  start <- as.numeric(Sys.time())
  value <- original_build(...)
  phases$build <<- c(start = start, end = as.numeric(Sys.time()))
  saveRDS(phases, file.path(job$directory, 'phases.rds'))
  value
}
original_check <- review_ready_progress_check
review_ready_progress_check <- function(...) {
  start <- as.numeric(Sys.time())
  value <- original_check(...)
  phases$check <<- c(start = start, end = as.numeric(Sys.time()))
  saveRDS(phases, file.path(job$directory, 'phases.rds'))
  value
}
# The experimental override changes only native worker count, not package bytes.
original_env <- review_ready_rcmdcheck_env
review_ready_rcmdcheck_env <- function(required_suggests) {
  value <- original_env(required_suggests)
  value[['TESTTHAT_CPUS']] <- as.character(job$workers)
  value
}
start <- as.numeric(Sys.time())
outcome <- tryCatch({
  run_review_ready_heavy(job$source, job$directory, job$required, job$r)
  list(status = 'passed')
}, interrupt = function(e) list(status = 'cancelled', error = conditionMessage(e)),
   error = function(e) list(status = 'failed', error = conditionMessage(e)))
outcome$started <- start
outcome$finished <- as.numeric(Sys.time())
outcome$workers <- job$workers
outcome$sha <- job$sha
outcome$benchmark_only <- TRUE
saveRDS(outcome, file.path(job$directory, 'benchmark-result.rds'))
quit(status = if (identical(outcome$status, 'passed')) 0L else 1L, save = 'no')
