# One bounded driver: exactly seven fresh Rscript samples, then 30 warm calls.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
stopifnot(length(script_arg) == 1L)
cost_here <- dirname(normalizePath(sub("^--file=", "", script_arg)))
cost_root <- normalizePath(args[[1L]], mustWork = TRUE)
cost_archive <- file.path(
  cost_root, "investigation/2026-11-02-review-ready-order-calibration"
)
dir.create(args[[2L]], recursive = TRUE, showWarnings = FALSE)
cost_output <- normalizePath(args[[2L]], mustWork = TRUE)
source(file.path(cost_here, "cost_pipeline.R"))
dir.create(file.path(cost_output, "r-output"), showWarnings = FALSE)
cost_rscript <- file.path(R.home("bin"), "Rscript")
cost_fresh <- vector("list", 7L)
for (index in seq_len(7L)) {
  result_path <- file.path(cost_output, sprintf("r-fresh-%02d.rds", index))
  stdout_path <- file.path(cost_output, sprintf("r-fresh-%02d.stdout", index))
  stderr_path <- file.path(cost_output, sprintf("r-fresh-%02d.stderr", index))
  started <- cost_clock()
  status <- system2(cost_rscript,
                    c(shQuote(file.path(cost_here, "run_once.R")),
                      shQuote(cost_here), shQuote(cost_root), shQuote(cost_output),
                      shQuote(result_path)),
                    stdout = stdout_path, stderr = stderr_path)
  elapsed <- (cost_clock() - started) * 1000
  stopifnot(status == 0L)
  result <- readRDS(result_path)
  result$sample <- index
  result$fresh_process_wall_ms <- elapsed
  cost_fresh[[index]] <- result
  saveRDS(result, file.path(cost_output, sprintf("r-fresh-with-wall-%02d.rds", index)),
          compress = FALSE)
}
cost_warm <- lapply(seq_len(30L), function(index) {
  result <- cost_pipeline(cost_root, file.path(cost_output, "r-output"))
  result$iteration <- index
  saveRDS(result, file.path(cost_output, sprintf("r-warm-%02d.rds", index)),
          compress = FALSE)
  result
})
cost_metrics <- c("parse_validate_ms", "registration_reorder_replay_ms",
                  "serialize_write_ms", "pipeline_ms")
cost_summarize <- function(rows, metrics, kind) {
  do.call(rbind, lapply(metrics, function(metric) {
    values <- vapply(rows, function(row) row[[metric]], numeric(1L))
    data.frame(kind = kind, metric = metric, n = length(values),
               min_ms = min(values), median_ms = median(values), max_ms = max(values))
  }))
}
cost_summary <- rbind(
  cost_summarize(cost_fresh, c(cost_metrics, "fresh_process_wall_ms"), "fresh"),
  cost_summarize(cost_warm, cost_metrics, "warm")
)
cost_flatten <- function(rows) {
  do.call(rbind, lapply(rows, function(row) {
    as.data.frame(row[setdiff(names(row), "models")], stringsAsFactors = FALSE)
  }))
}
write.csv(cost_summary, file.path(cost_output, "r-summary.csv"), row.names = FALSE)
write.csv(cost_flatten(cost_fresh), file.path(cost_output, "r-fresh-process.csv"),
           row.names = FALSE)
write.csv(cost_flatten(cost_warm), file.path(cost_output, "r-warm-in-process.csv"),
           row.names = FALSE)
saveRDS(list(fresh = cost_fresh, warm = cost_warm, summary = cost_summary),
        file.path(cost_output, "r-all-results.rds"), compress = FALSE)
cost_inputs <- c(list.files(file.path(cost_archive, "run-1/check/file-timings"),
                            pattern = "[.]tsv$", full.names = TRUE),
                  file.path(cost_archive, c("files.csv", "run-summary.csv", "executions.json")),
                  file.path(cost_root, "DESCRIPTION"))
cost_environment <- list(
  utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
  R = R.version, session_info = capture.output(sessionInfo()), sys_info = Sys.info(),
  measurement_source_sha = readRDS(file.path(cost_output, "r-output/proposal.rds"))$measurement_sha,
  source_head_sha = system2("git", c("-C", shQuote(cost_root), "rev-parse", "HEAD"),
                           stdout = TRUE),
  inputs_md5 = tools::md5sum(cost_inputs),
  programs_md5 = tools::md5sum(file.path(cost_here,
                                        c("cost_pipeline.R", "run_once.R", "benchmark.R"))),
  fresh_process_samples = 7L, warm_iterations = 30L,
  timer = "as.numeric(Sys.time()), elapsed wall seconds converted to milliseconds",
  limits = c("Fresh means process; filesystem caches were not reset.",
             "Ordinary buffered writes, without fsync/durable-storage timing.",
             "Driver exists separately from the seven fresh measured Rscript processes.",
             "No all-skipped eligibility flags exist in the archived intervals.",
             "Registration insertion/tie rule and RDS output are benchmark choices, not agreed formats.",
             "No Python dependency, package tests, Review-ready, or production changes.")
)
saveRDS(cost_environment, file.path(cost_output, "r-environment.rds"), compress = FALSE)
writeLines(capture.output(dput(cost_environment)), file.path(cost_output, "r-environment.txt"))
print(cost_summary, row.names = FALSE)
cat("RDS bytes:", cost_fresh[[1L]]$output_rds_bytes,
    "; patch bytes:", cost_fresh[[1L]]$output_patch_bytes, "\n")
print(cost_fresh[[1L]]$models)
