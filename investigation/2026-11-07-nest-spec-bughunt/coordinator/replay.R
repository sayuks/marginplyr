args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
source_root <- normalizePath(args[[1L]], mustWork = TRUE)
script_root <- normalizePath(args[[2L]], mustWork = TRUE)
output_root <- args[[3L]]
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
output_root <- normalizePath(output_root, mustWork = TRUE)
script <- file.path(script_root, "replay-root-bracket.R")
stopifnot(file.exists(script))
cases <- list(
  bracket_observe = character(), bracket_contract = "--contract",
  converter_observe = "--converter", converter_contract = c("--converter", "--contract")
)
expected <- c(0L, 1L, 0L, 1L)
results <- lapply(seq_along(cases), function(i) {
  log <- file.path(output_root, paste0(names(cases)[[i]], ".log"))
  status <- system2(file.path(R.home("bin"), "Rscript"),
                    args = shQuote(c(script, source_root, cases[[i]])),
                    stdout = log, stderr = log,
                    env = "TESTTHAT_PARALLEL=false")
  data.frame(case = names(cases)[[i]], expected = expected[[i]], observed = status,
             script_md5 = unname(tools::md5sum(script)))
})
results <- do.call(rbind, results)
write.csv(results, file.path(output_root, "replay-results.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(output_root, "coordinator-session.txt"))
print(results)
stopifnot(identical(as.integer(results$observed), expected))
