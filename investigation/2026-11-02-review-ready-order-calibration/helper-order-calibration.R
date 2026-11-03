# Records worker file intervals only for this committed calibration experiment.
# queue_task surrounds file cleanup; controller reporter replay is outside it.
if (nzchar(Sys.getenv("MARGINPLYR_REVIEW_PROGRESS"))) {
  trace("queue_task", where = asNamespace("testthat"), print = FALSE,
    tracer = quote({
      calibration_directory <- file.path(
        dirname(Sys.getenv("MARGINPLYR_REVIEW_PROGRESS")), "file-timings"
      )
      dir.create(calibration_directory, showWarnings = FALSE, recursive = TRUE)
      calibration_log <- file.path(
        calibration_directory, paste0(Sys.getpid(), ".tsv")
      )
      calibration_line <- paste(
        "start", basename(path), Sys.getpid(),
        sprintf("%.6f", as.numeric(Sys.time())),
        sprintf("%.6f", proc.time()[["elapsed"]]), sep = "\t"
      )
      cat(calibration_line, "\n", file = calibration_log,
          append = TRUE, sep = "")
    }),
    exit = quote({
      calibration_line <- paste(
        "end", basename(path), Sys.getpid(),
        sprintf("%.6f", as.numeric(Sys.time())),
        sprintf("%.6f", proc.time()[["elapsed"]]), sep = "\t"
      )
      cat(calibration_line, "\n", file = calibration_log,
          append = TRUE, sep = "")
    })
  )
}
