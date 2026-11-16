# Wraps `reporter` so a coverage shard's test process appends each test file's
# wall-clock seconds to `path`, one `<file>\t<seconds>` line per completed file.
# Times run from start_file() to end_file() in this process, which measures a
# file only in a serial run, so the reporter does not claim parallel support.
coverage_durations_reporter <- function(reporter, path) {
  file <- NULL
  started <- NULL
  observer <- R6::R6Class(
    "CoverageDurationsReporter", inherit = testthat::Reporter,
    public = list(
      start_file = function(filename) {
        file <<- basename(filename)
        started <<- proc.time()[["elapsed"]]
      },
      end_file = function() {
        cat(file, "\t", sprintf("%.3f", proc.time()[["elapsed"]] - started),
          "\n", file = path, append = TRUE, sep = "")
      }
    )
  )$new()
  # testthat::check_reporter() names its reporter rather than creating it.
  if (is.character(reporter)) {
    reporter <- getExportedValue("testthat", paste0(reporter, "Reporter"))$new()
  }
  testthat::MultiReporter$new(list(reporter, observer))
}
