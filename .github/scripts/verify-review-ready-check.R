# The base-R CI job checks the public command's argument rejection contract.
# Real check, lock, process, retention and progress fixtures run in the lint job.
local({
  entry <- normalizePath("tools/review-ready.R", mustWork = TRUE)
  for (args in list(character(), c("--scope", "source"),
      c("--scope", "Full", "--reason", "fixture"),
      c("--scope", "source", "--reason", " "),
      c("--scope", "full", "--reason", "fixture", "--retry"))) {
    output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
      vapply(c(entry, args), shQuote, character(1)), stdout = TRUE, stderr = TRUE))
    stopifnot(identical(attr(output, "status"), 1L),
      any(grepl("Usage: Rscript tools/review-ready.R --scope source|full --reason TEXT",
        output, fixed = TRUE)))
  }
  cat("Review-ready public command argument contracts passed.\n")
})
