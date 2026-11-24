# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(marginplyr)

# The CRAN tier (ADR 0045): the test files `R CMD check` runs when nothing asks
# for the complete suite, each named as testthat's `filter` matches it, without
# `test-` and `.R`. A file this vector does not name runs in the complete suite
# only, which is where a new test file therefore starts.
cran_tier <- c(
  "dependency-floor",
  "documentation",
  "expand-operation",
  "factor",
  "get-col-names",
  "grouping-interface",
  "grouping-patterns",
  "margin-display",
  "margin-id",
  "metamorphic-margin-semantics",
  "native-classes",
  "nest-rows",
  "optional-backends",
  "package-metadata",
  "postgres-share-probe",
  "retail-sales",
  "sent-query-details",
  "sf-summary",
  "suggest-guard-scan",
  "utils"
)

# `.github/scripts/cran-test-budget.R` reads the same two variables to decide
# whether the budget applies; this file ships and cannot source it.
complete_suite <- any(vapply(
  c("NOT_CRAN", "MARGINPLYR_COMPLETE_SUITE"),
  function(name) isTRUE(as.logical(Sys.getenv(name))),
  logical(1)
))
filter <- if (!complete_suite) {
  paste0("^(", paste(cran_tier, collapse = "|"), ")$")
}

reporter <- testthat::check_reporter()
progress_reporter <- Sys.getenv("MARGINPLYR_REVIEW_PROGRESS_REPORTER")
if (nzchar(progress_reporter)) {
  reporter <- source(progress_reporter, local = TRUE)$value(
    Sys.getenv("MARGINPLYR_REVIEW_PROGRESS")
  )
}
test_check("marginplyr", reporter = reporter, filter = filter)
