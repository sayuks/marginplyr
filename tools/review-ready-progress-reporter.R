# Creates the ordinary check reporter with one event per completed file.
# Keeping buffered parallel reporting avoids per-expectation progress traffic.
review_ready_progress_reporter <- function(path) {
  total <- length(list.files("testthat", "^test.*[.][rR]$"))
  completed_file <- NULL
  observer <- R6::R6Class(
    "ReviewReadyProgressReporter", inherit = testthat::Reporter,
    public = list(
      start_reporter = function() {
        cat("total\t", total, "\n", file = path, sep = "")
      },
      start_file = function(filename) {
        completed_file <<- filename
      },
      end_file = function() {
        cat("done\t", completed_file, "\n",
            file = path, append = TRUE, sep = "")
      },
      end_reporter = function() {
        cat("finished\n", file = path, append = TRUE)
      }
    )
  )$new()
  observer$capabilities$parallel_support <- TRUE
  testthat::MultiReporter$new(list(testthat::CheckReporter$new(), observer))
}
