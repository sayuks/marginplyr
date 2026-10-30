# Measurement-only helper, copied into tests/testthat for the measured commits.
# Each worker writes its own file; reporter replay cannot change these times.
if (nzchar(Sys.getenv("MARGINPLYR_TIMING_DIR"))) {
  trace("test_one_file", where = asNamespace("testthat"), print = FALSE,
    tracer = quote({
      if (identical(Sys.getenv("TESTTHAT_IS_PARALLEL"), "true")) {
        cat("start", basename(path), Sys.getpid(),
            sprintf("%.6f", as.numeric(Sys.time())),
            sprintf("%.6f", proc.time()[["elapsed"]]), sep = "\t",
            file = file.path(Sys.getenv("MARGINPLYR_TIMING_DIR"),
                             paste0(Sys.getpid(), ".tsv")), append = TRUE)
        cat("\n", file = file.path(Sys.getenv("MARGINPLYR_TIMING_DIR"),
                                   paste0(Sys.getpid(), ".tsv")), append = TRUE)
      }
    }),
    exit = quote({
      if (identical(Sys.getenv("TESTTHAT_IS_PARALLEL"), "true")) {
        cat("end", basename(path), Sys.getpid(),
            sprintf("%.6f", as.numeric(Sys.time())),
            sprintf("%.6f", proc.time()[["elapsed"]]), sep = "\t",
            file = file.path(Sys.getenv("MARGINPLYR_TIMING_DIR"),
                             paste0(Sys.getpid(), ".tsv")), append = TRUE)
        cat("\n", file = file.path(Sys.getenv("MARGINPLYR_TIMING_DIR"),
                                   paste0(Sys.getpid(), ".tsv")), append = TRUE)
      }
    })
  )
}
