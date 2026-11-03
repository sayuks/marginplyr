# Creates the ordinary check reporter with one event per completed file.
# Keeping buffered parallel reporting avoids per-expectation progress traffic.
review_ready_progress_reporter <- function(path) {
  total <- length(list.files("testthat", "^test.*[.][rR]$"))
  completed_file <- NULL
  tally <- c(skip = 0L, other = 0L)
  timing <- Sys.getenv("MARGINPLYR_REVIEW_TIMING")
  observer <- R6::R6Class(
    "ReviewReadyProgressReporter", inherit = testthat::Reporter,
    public = list(
      start_reporter = function() {
        cat("total\t", total, "\n", file = path, sep = "")
      },
      start_file = function(filename) {
        completed_file <<- filename
        tally[] <<- 0L
      },
      add_result = function(context, test, result) {
        if (!nzchar(timing)) return(invisible(NULL))
        kind <- if (inherits(result, "expectation_skip")) "skip" else "other"
        tally[[kind]] <<- tally[[kind]] + 1L
      },
      end_file = function() {
        cat("done\t", completed_file, "\n",
            file = path, append = TRUE, sep = "")
        if (nzchar(timing)) tryCatch(withCallingHandlers({
          cat(paste(completed_file, tally[["skip"]], tally[["other"]], sep = "\t"),
            "\n", file = file.path(timing, "file-results.tsv"), append = TRUE, sep = "")
        }, warning = function(cnd) stop(conditionMessage(cnd))), error = function(cnd) {
          cat("Review-ready timing disabled: ", conditionMessage(cnd), "\n", sep = "")
        })
      },
      end_reporter = function() {
        cat("finished\n", file = path, append = TRUE)
      }
    )
  )$new()
  observer$capabilities$parallel_support <- TRUE
  testthat::MultiReporter$new(list(testthat::CheckReporter$new(), observer))
}
