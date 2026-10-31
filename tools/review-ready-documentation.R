# Runs the documentation assertions against the exact archived source tree.
# Maintain this registration when a test gains documentation inputs; filenames
# alone cannot establish that the selection covers every documentation contract.
review_ready_documentation_tests <- function() {
  c("test-documentation.R", "test-duckdb-storage.R", "test-package-metadata.R")
}

# Refuses missing selections and skips; test_local retains helpers and lifecycle.
run_review_ready_documentation_tests <- function(source_root = ".") {
  targets <- review_ready_documentation_tests()
  if (!length(targets) || anyDuplicated(targets)) {
    stop("Documentation assertion registration is empty or duplicated.", call. = FALSE)
  }
  stems <- sub("[.]R$", "", sub("^test-", "", targets))
  filter <- paste0("^(", paste(stems, collapse = "|"), ")$")
  selected <- testthat::find_test_scripts(file.path(source_root, "tests", "testthat"),
    filter = filter, full.names = FALSE)
  if (!identical(sort(selected), sort(targets))) {
    stop("Documentation selection must match every registered file exactly.", call. = FALSE)
  }
  Sys.setenv(
    TESTTHAT_PARALLEL = "false", TESTTHAT_CPUS = "1", NOT_CRAN = "true"
  )
  cat("Documentation assertions: ", paste(targets, collapse = ", "), "\n", sep = "")
  results <- testthat::test_local(source_root, filter = filter, reporter = "summary",
    stop_on_failure = TRUE, stop_on_warning = TRUE)
  for (target in targets) {
    entries <- Filter(function(entry) identical(entry$file, target), results)
    assertions <- unlist(lapply(entries, `[[`, "results"), recursive = FALSE)
    if (!length(assertions) || any(vapply(assertions, inherits, logical(1), "expectation_skip"))) {
      stop("Documentation assertions were empty or skipped: ", target, call. = FALSE)
    }
  }
  invisible(results)
}

if (sys.nframe() == 0L) run_review_ready_documentation_tests()
