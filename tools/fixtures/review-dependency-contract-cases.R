# Shared dependency inputs for the existing local and preflight command seams.
review_dependency_contract_cases <- function() {
  edit_review <- function(change) function(root) {
    path <- file.path(root, "DESCRIPTION")
    description <- read.dcf(path)
    value <- gsub("[[:space:]]+", " ", description[[1L, "Config/Needs/review"]])
    description[[1L, "Config/Needs/review"]] <- change(value)
    write.dcf(description, path)
  }
  mismatch <- "Review dependency contract mismatch"
  invalid <- "Review dependency contract input invalid"
  list(
    matching = list(edit = function(root) NULL, diagnostic = NULL),
    valid_constraint = list(edit = edit_review(function(value) sub("digest", "digest (>= 0.1.0)", value,
      fixed = TRUE)), diagnostic = NULL),
    historical_missing_roxygen_expectation = list(edit = function(root) {
      path <- file.path(root, "tools/repository-contracts.R")
      text <- readLines(path)
      target <- '"rcmdcheck", "roxygen2", "spelling"'
      stopifnot(sum(grepl(target, text, fixed = TRUE)) == 1L)
      writeLines(sub(target, '"rcmdcheck", "spelling"', text, fixed = TRUE), path)
    }, diagnostic = mismatch),
    added_dependency = list(edit = edit_review(function(value) paste(value, "utils", sep = ", ")),
      diagnostic = mismatch),
    removed_dependency = list(edit = edit_review(function(value) sub("roxygen2,", "", value,
      fixed = TRUE)), diagnostic = mismatch),
    duplicate_dependency = list(edit = edit_review(function(value) paste(value, "yaml", sep = ", ")),
      diagnostic = mismatch),
    dependency_order = list(edit = edit_review(function(value) sub("digest, filelock", "filelock, digest",
      value, fixed = TRUE)), diagnostic = mismatch),
    trailing_comma = list(edit = edit_review(function(value) paste0(value, ",")), diagnostic = invalid),
    empty_entry = list(edit = edit_review(function(value) sub("digest,", "digest,,", value,
      fixed = TRUE)), diagnostic = invalid),
    malformed_token = list(edit = edit_review(function(value) sub("digest", "digest [>= 1.0]", value,
      fixed = TRUE)), diagnostic = invalid),
    malformed_version = list(edit = edit_review(function(value) sub("digest", "digest (>= not-a-version)",
      value, fixed = TRUE)), diagnostic = invalid),
    empty_field = list(edit = edit_review(function(value) ""), diagnostic = invalid),
    missing_field = list(edit = function(root) {
      path <- file.path(root, "DESCRIPTION")
      description <- read.dcf(path)
      write.dcf(description[, colnames(description) != "Config/Needs/review", drop = FALSE], path)
    }, diagnostic = invalid),
    multiple_records = list(edit = function(root) cat("\nPackage: extra\n",
      file = file.path(root, "DESCRIPTION"), append = TRUE), diagnostic = invalid),
    missing_description = list(edit = function(root) unlink(file.path(root, "DESCRIPTION")),
      diagnostic = invalid, local_diagnostic = "cannot open", local_stage = "not-run"),
    malformed_dcf = list(edit = function(root) cat("not a DCF field\n",
      file = file.path(root, "DESCRIPTION"), append = TRUE), diagnostic = invalid,
      local_diagnostic = "is malformed", local_stage = "not-run")
  )
}
