#!/usr/bin/env Rscript

# The strict coverage gate cannot pass by silently removing source lines from
# the denominator. Exercise each exclusion route without running the test suite.

source("tools/coverage-check-lib.R")
coverage_verify_policy()

fixture <- tempfile("marginplyr-coverage-policy-")
dir.create(file.path(fixture, "R"), recursive = TRUE)
on.exit(unlink(fixture, recursive = TRUE), add = TRUE)
writeLines("f <- function() TRUE", file.path(fixture, "R", "f.R"))
writeLines("coverage: {}", file.path(fixture, "codecov.yml"))
coverage_verify_policy(fixture)

expect_rejected <- function(expression, fragment) {
  error <- tryCatch({
    force(expression)
    NULL
  }, error = identity)
  if (!inherits(error, "error") ||
      !grepl(fragment, conditionMessage(error), fixed = TRUE)) {
    stop("Coverage policy failed to reject: ", fragment, call. = FALSE)
  }
}

writeLines("f <- function() TRUE # nocov", file.path(fixture, "R", "f.R"))
expect_rejected(coverage_verify_policy(fixture), "Coverage exclusions")
writeLines("f <- function() TRUE", file.path(fixture, "R", "f.R"))

writeLines("R/f.R", file.path(fixture, ".covrignore"))
expect_rejected(coverage_verify_policy(fixture), "Coverage exclusions")
unlink(file.path(fixture, ".covrignore"))

writeLines(c("coverage: {}", "ignore:", "  - R/f.R"),
  file.path(fixture, "codecov.yml"))
expect_rejected(coverage_verify_policy(fixture), "Codecov ignore")
writeLines(c("coverage: {}", "'ignore':", "  - R/f.R"),
  file.path(fixture, "codecov.yml"))
expect_rejected(coverage_verify_policy(fixture), "Codecov ignore")

gate <- paste(deparse(body(coverage_check)), collapse = "\n")
required <- c("coverage_verify_policy(root)", "coverage_verify_runtime()",
  "coverage_required_suggests(root)", "covr::package_coverage(",
  "covr::tally_coverage(cov, by = \"line\")", "lines$value == 0L")
for (part in required) {
  if (!grepl(part, gate, fixed = TRUE)) {
    stop("Coverage gate is missing: ", part, call. = FALSE)
  }
}
forbidden <- c("line_exclusions", "function_exclusions", "exclusions =")
for (part in forbidden) {
  if (grepl(part, gate, fixed = TRUE)) {
    stop("Coverage gate contains an exclusion argument: ", part,
      call. = FALSE)
  }
}

cat("Coverage exclusion policy passed.\n")
