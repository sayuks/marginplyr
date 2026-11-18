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

with_covr_runtime <- function(covrignore, envvar, expression) {
  previous <- options(covr.covrignore = covrignore)
  on.exit(options(previous), add = TRUE)
  previous_envvar <- Sys.getenv("COVR_COVRIGNORE", unset = NA_character_)
  on.exit(if (is.na(previous_envvar)) {
    Sys.unsetenv("COVR_COVRIGNORE")
  } else {
    Sys.setenv(COVR_COVRIGNORE = previous_envvar)
  }, add = TRUE)
  if (is.na(envvar)) {
    Sys.unsetenv("COVR_COVRIGNORE")
  } else {
    Sys.setenv(COVR_COVRIGNORE = envvar)
  }
  force(expression)
}
# Before the cases: covr < 4.0.0's .onLoad() would reset an unset option.
invisible(loadNamespace("covr"))
with_covr_runtime(NULL, NA, coverage_verify_runtime())
with_covr_runtime(".covrignore", NA, coverage_verify_runtime())
expect_rejected(with_covr_runtime("other", NA, coverage_verify_runtime()),
  "Nonstandard covr exclusion option: covr.covrignore")
expect_rejected(with_covr_runtime(NULL, "other", coverage_verify_runtime()),
  "COVR_COVRIGNORE must be unset")

source_of <- function(...) {
  paste(unlist(lapply(list(...), function(f) deparse(body(f)))), collapse = "\n")
}
# The unsharded check and each shard measure through coverage_measure(); the
# unsharded check and the merge of the shards reach coverage_verdict().
gate <- source_of(coverage_measure, coverage_verdict)
required <- c("coverage_verify_policy(root)", "coverage_verify_runtime()",
  "required_test_suggests(root)", "covr::package_coverage(",
  "covr::tally_coverage(cov, by = \"line\")", "lines$value == 0L")
for (part in required) {
  if (!grepl(part, gate, fixed = TRUE)) {
    stop("Coverage gate is missing: ", part, call. = FALSE)
  }
}
routes <- list(coverage_check = c("coverage_measure(", "coverage_verdict("),
  coverage_check_shard = "coverage_measure(")
for (route in names(routes)) {
  for (part in routes[[route]]) {
    if (!grepl(part, source_of(get(route)), fixed = TRUE)) {
      stop("Coverage gate is missing from ", route, ": ", part, call. = FALSE)
    }
  }
}
every_route <- source_of(coverage_check, coverage_measure, coverage_verdict,
  coverage_check_shard, coverage_merge)
forbidden <- c("line_exclusions", "function_exclusions", "exclusions =")
for (part in forbidden) {
  if (grepl(part, every_route, fixed = TRUE)) {
    stop("Coverage gate contains an exclusion argument: ", part,
      call. = FALSE)
  }
}

cat("Coverage exclusion policy passed.\n")
