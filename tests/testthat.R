# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(marginplyr)

reporter <- testthat::check_reporter()
progress_reporter <- Sys.getenv("MARGINPLYR_REVIEW_PROGRESS_REPORTER")
if (nzchar(progress_reporter)) {
  reporter <- source(progress_reporter, local = TRUE)$value(
    Sys.getenv("MARGINPLYR_REVIEW_PROGRESS")
  )
}
test_check("marginplyr", reporter = reporter)
